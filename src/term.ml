
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Common Term representation} *)

type t = {
  term : term_cell;             (** the term itself *)
  ty : Type.t option lazy;      (** type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable id : int;             (** hashconsing tag *)
}
(** content of the term *)
and term_cell = private
  | TType                       (** the "type" of types *)
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Const of Symbol.t           (** Constant *)
  | At of t * t list            (** function application. *)
  | Lambda of t * t             (** lambda abstraction over one variable. *)
  | Pi of t * t                 (** quantification on types *)

type term = t

(** {2 Basics} *)

let hash t = t.id

let eq t1 t2 = t1 == t2

let compare t1 t2 = t1.id - t2.id

(** {2 Boolean flags} *)

let __gen = Util.Flag.create ()
let new_flag () = Util.Flag.get_new __gen

let flag_db_closed = new_flag ()
and flag_normal_form = new_flag ()
and flag_ground = new_flag ()
and flag_db_closed_computed = new_flag ()

let set_flag flag t truth =
  if truth
    then t.flags <- t.flags lor flag
    else t.flags <- t.flags land (lnot flag)

let get_flag flag t = (t.flags land flag) != 0

(** {2 Global terms table (hashconsing)} *)

let _hash t =
  match t.term with
  | TType -> 13
  | Var i -> Hash.combine i (hash t.ty)
  | BoundVar i -> Hash.combine (hash.t.ty) i
  | Const s -> Hash.combine (Symbol.hash s) (hash t.ty)
  | At (f, args) ->
    let h = Hash.combine (hash f) (hash t.ty) in
    Hash.hash_list hash h args
  | Lambda (arg,t') -> Hash.combine (hash t') (hash arg)
  | Pi (arg,t') -> Hash.combine (hash arg) (hash t')

let rec _eq_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | t1::l1', t2::l2' -> eq t1 t2 && _eq_list l1' l2

let _eq t1 t2 =
  eq t1.ty t2.ty &&
  match t1, t2 with
  | TType, TType -> true
  | Var i1, Var i2 
  | BoundVar i1, BoundVar i2 -> i1 = i2
  | Const s1, Const s2 -> Symbol.eq s1 s2
  | At (f1, args1), At (f2, args2) -> eq f1 f2 && _eq_list args1 args2
  | Lambda (arg1,t1'), Lambda (arg2,t2') -> eq arg1 arg2 && eq t1' t2'
  | Pi (arg1, t1'), Pi (arg2, t2') -> eq arg1 arg2 && eq t1' t2'

module H = Hashcons.Make(struct
  type t = term
  let equal = _eq
  let hash = _hash
  let tag i t = (assert (t.id = ~-1); t.id <- i)
end)

(** {2 Constructors} *)

let tType =
  let my_t = {term= TType; ty=Lazy.from_val None; tsize=1;
              flags=(flag_db_closed lor flag_db_closed_computed
              lor flag_normal_form); tag= -1}
  in
  H.hashcons my_t


let var ~ty i =
  assert (idx >= 0);
  let my_t = {term = Var idx; ty=Lazy.from_val (Some ty); tsize = 1;
              flags=(flag_db_closed lor flag_db_closed_computed lor
                     flag_normal_form);
              tag= -1} in
  H.hashcons my_t
  
let mk_bound_var idx =
  assert (idx >= 0);
  let my_t = {term = BoundVar idx; ty=Lazy.from_val None; tsize = 1;
              flags=(flag_db_closed_computed lor flag_normal_form);
              tag= -1} in
  H.hashcons my_t

exception DependentType

let pi ~varty t =
  (* check that the type is not dependent *)
  begin match varty.term, t.ty.term with
  | Some TType, Some TType -> ()
  | _, Some TType -> raise DependentType
  | _ -> ()
  end;
  let my_t = {term= Pi(varty, t); ty=Lazy.from_val (Some tType);
              tsize=1+t.tsize; tag= ~-1; }
  in
  H.hashcons my_t

exception SystemF

let lambda ~varty t =
  (* check that we stay in an easy fragment: lambda only at the term level *)
  begin match varty.term, t.ty.term with
  | Some TType, _
  | _, Some TType -> raise SystemF
  | _ -> ()
  end;
  let my_t = {term=Lambda(varty, t); ty=lazy(pi ~varty t.ty)


(** {2 Variable environment} *)

module Env = struct
  type t = {
    stack : term option list;
    depth : int;
  }

  let empty = { stack=[]; depth = 0; }

  let depth env = env.depth

  let push env t = { stack=(Some t) :: env.stack; depth=env.depth+1; }

  let push_none env = {stack=None :: env.stack; depth=env.depth+1; }

  let pop env =
    match env.stack with
    | [] -> raise (Invalid_argument "empty Term.env")
    | _::stack' ->
      { stack=stack'; depth=env.depth-1; }

  let find env n =
    let rec find stack n = match stack, n with
      | x::_, 0 -> x
      | _::stack', _ -> find stack' (n-1)
      | [], _ -> None
    in
    assert (n >= 0);
    find env.stack n
end










(* TODO *)

let arg_ty t = match t.term with
  | Lambda (arg, _)
  | Pi (arg, _) -> arg
  | _ -> failwith "Term.arg_ty: expected lambda or pi"

