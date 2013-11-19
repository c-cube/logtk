
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
  mutable ty : t option;        (** type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable id : int;             (** hashconsing tag *)
}
(** content of the term *)
and term_cell =
  | TType                       (** the "type" of types *)
  | Kind                        (** the "type" of TType *)
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Const of Symbol.t           (** Constant *)
  | At of t * t list            (** function application. *)
  | Lambda of t * t             (** lambda abstraction over one variable. *)
  | Pi of t * t                 (** quantification on types *)

type term = t

(** {2 Exceptions} *)

exception SystemF

exception TypeError of string
  (** Type error, raised when types do not match *)


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

let _hash_ty = function
  | Some ty -> ty.id
  | None -> assert false

let _hash t =
  match t.term with
  | TType -> 13
  | Kind -> 14
  | Var i -> Hash.combine i (_hash_ty t.ty)
  | BoundVar i -> Hash.combine (_hash_ty t.ty) i
  | Const s -> Hash.combine (Symbol.hash s) (_hash_ty t.ty)
  | At (f, args) ->
    Hash.hash_list hash (hash f) args
  | Lambda (arg,t') -> Hash.combine (hash t') (hash arg)
  | Pi (arg,t') -> Hash.combine (hash arg) (hash t')

let rec _eq_list l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | t1::l1', t2::l2' -> eq t1 t2 && _eq_list l1' l2'

(* structural equality *)
let _eq t1 t2 =
  let eq_ty ty1 ty2 = match ty1, ty2 with
  | Some ty1, Some ty2 -> eq ty1 ty2
  | _ -> assert false
  in
  match t1.term, t2.term with
  | TType, TType
  | Kind, Kind -> true
  | BoundVar i1, BoundVar i2
  | Var i1, Var i2 -> i1 = i2 && eq_ty t1.ty t2.ty
  | Const s1, Const s2 -> Symbol.eq s1 s2 && eq_ty t1.ty t2.ty
  | At (f1, args1), At (f2, args2) -> eq f1 f2 && _eq_list args1 args2
  | Lambda (arg1,t1'), Lambda (arg2,t2') -> eq arg1 arg2 && eq t1' t2'
  | Pi (arg1, t1'), Pi (arg2, t2') -> eq arg1 arg2 && eq t1' t2'
  | _, _ -> false

(* obtain the type of this term. It must be present. *)
let _get_ty t = match t.ty with
  | None -> assert false
  | Some ty -> ty

module H = Hashcons.Make(struct
  type t = term
  let equal = _eq
  let hash = _hash
  let tag i t = (assert (t.id = ~-1); t.id <- i)
end)

let print_all_types = ref false

(* pretty-printing, used for debug messages *)
let rec pp_depth depth buf t =
  let rec pp_rec depth buf t =
    begin match t.term with
    | TType -> Buffer.add_string buf "type"
    | Kind -> Buffer.add_string buf "kind"
    | BoundVar i -> Printf.bprintf buf "Y%d" (depth - i - 1)
    | Lambda (ty, t') ->
      Printf.bprintf buf "λ%a:%a. " pp_bvar depth (pp_surrounded depth) ty;
      pp_surrounded (depth+1) buf t';
    | Pi (ty, t') ->
      Printf.bprintf buf "Π%a:%a. " pp_bvar depth (pp_surrounded depth) ty;
      pp_surrounded (depth+1) buf t';
    | Const s -> Symbol.pp buf s
    | Var i ->
      begin match t.ty with
      | None -> assert false
      | Some ty -> Printf.bprintf buf "X%d:%a" i (pp_rec depth) ty
      end
    | At (t, l) -> Util.pp_list ~sep:" " (pp_surrounded depth) buf (t :: l)
    end;
    begin match t.ty with
      | Some ty when !print_all_types -> Printf.bprintf buf ":%a" (pp_rec depth) ty
      | _ -> ()
    end
  and pp_surrounded depth buf t = match t.term with
    | At _ ->
      Buffer.add_char buf '('; pp_rec depth buf t; Buffer.add_char buf ')'
    | _ -> pp_rec depth buf t
  and pp_bvar buf depth =
    Printf.bprintf buf "Y%d" depth
  in
  pp_rec depth buf t

(* debug function *)
let pp_debug buf t = pp_depth 0 buf t

(** {2 Constructors} *)

let kind = 
  let my_t = {term= Kind; ty=None; tsize=1;
              flags=(flag_db_closed lor flag_db_closed_computed
              lor flag_normal_form); id= -1}
  in
  H.hashcons my_t

let tType =
  let my_t = {term= TType; ty=Some kind; tsize=1;
              flags=(flag_db_closed lor flag_db_closed_computed
              lor flag_normal_form); id= -1}
  in
  H.hashcons my_t

let var ~ty i =
  assert (i >= 0);
  let my_t = {term = Var i; ty=Some ty; tsize = 1;
              flags=(flag_db_closed lor flag_db_closed_computed lor
                     flag_normal_form);
              id= -1} in
  H.hashcons my_t
  
let bound_var ~ty i =
  assert (i >= 0);
  let my_t = {term = BoundVar i; ty=Some ty; tsize = 1;
              flags=(flag_db_closed_computed lor flag_normal_form);
              id= -1} in
  H.hashcons my_t

let __pi ~varty t =
  let my_t = {term=Pi(varty, t); ty=None;
              tsize=1+t.tsize; id= ~-1; flags=0; }
  in
  H.hashcons my_t

let __lambda ~varty t =
  let my_t = {term=Lambda(varty, t); ty=None;
              tsize=1+t.tsize; id= ~-1; flags=0; }
  in
  H.hashcons my_t

let rec __at head l = match head.term, l with
  | _, [] -> head
  | At (head, l'), _ -> __at head (l' @ l)   (* flatten *)
  | _, _ ->
    let my_t = {term=At(head,l); ty=None; tsize=0; id= ~-1; flags=0; } in
    let t = H.hashcons my_t in
    if t == my_t then begin
      (* compute ground-ness of term *)
      let is_ground = get_flag flag_ground head &&
        List.for_all (get_flag flag_ground) l
      in
      set_flag flag_ground t is_ground;
      t.tsize <- List.fold_left (fun s t' -> s + t'.tsize) (t.tsize+1) l;
      end;
    t

let const ~ty s =
  let my_t = {term=Const s; ty=Some ty; id= ~-1; tsize=1; flags=flag_ground; } in
  H.hashcons my_t

(* are the two types convertible?
  TODO: beta-reduction at root? *)
let convertible a b = eq a b

(* substitution under a De Bruijn environment.
   TODO: optim in case [t] is DB-closed *)
let rec _subst ~env t = match t.term with
  | TType
  | Kind -> t
  | Var i ->
    let ty = _subst ~env (_get_ty t) in
    var ~ty i
  | Const s ->
    let ty = _subst ~env (_get_ty t) in
    const ~ty s
  | BoundVar i ->
    begin match DBEnv.find env i with
    | None ->
      let ty = _subst ~env (_get_ty t) in
      bound_var ~ty i
    | Some t' ->
      (* [t']'s De Bruijn indices made sense in a different context.
        Since this context has been created, we crossed [i] binders (because
        the variable bound to [t'] is the [i]-th), so the free DB indices
        of [t] must be shifted by [i] *)
      _shift ~depth:0 i t'
    end
  | At (head, l) ->
    __at (_subst ~env head) (_subst_list ~env l)
  | Lambda (varty, t') ->
    __lambda ~varty (_subst (DBEnv.push_none env) t')
  | Pi (varty, t') ->
    __pi ~varty (_subst (DBEnv.push_none env) t')
and _subst_list ~env l = match l with
  | [] -> []
  | x::l' -> _subst ~env x :: _subst_list ~env l'

(* shift De Bruijn indices by [n] unless they are bound within [depth].
    [depth] is the number of binders met since the first call to [shift].

    TODO: special case when [t'] DB-closed *)
and _shift ~depth n t = match t.term with
  | TType
  | Kind 
  | Var _
  | Const _ -> t
  | BoundVar i ->
    if i < depth then t else bound_var ~ty:(_get_ty t) (i+n)
  | At (head, l) ->
    __at (_shift ~depth n head) (_shift_list ~depth n l)
  | Lambda (varty, t') ->
    __lambda ~varty (_shift ~depth:(depth+1) n t')
  | Pi (varty, t') ->
    __pi ~varty (_shift ~depth:(depth+1) n t')
and _shift_list ~depth n l = match l with
  | [] -> []
  | x::l' -> _shift ~depth n x :: _shift_list ~depth n l'

(* compute the type of this term. An environment is used to
   bind DB indices to other terms *)
and _infer_ty t =
  match t.ty with
  | Some ty -> ty
  | None ->
    let ty = match t.term with
    | Kind -> raise (Invalid_argument "infer_ty: Kind has no type")
    | BoundVar _
    | TType
    | Const _
    | Var _ -> assert false  (* should never happen *)
    | At (head,l) ->
      let ty_head = _infer_ty head in
      _ty_rule_match ~env:DBEnv.empty ty_head l
    | Lambda (ty, t') ->
      let ty_ty = _infer_ty ty in
      if not (eq ty_ty tType) && not (eq ty_ty kind)
        then begin
          let msg = Util.sprintf
            "in %a, the variable's type is %a and should be of type Type or Kind"
            pp_debug t pp_debug ty_ty in
          raise (TypeError msg)
        end;
      (* lambda x:ty. t' : pi x:ty. tau'   where  x:ty |- t':tau' *)
      let ty_t' = _infer_ty t' in
      __pi ~varty:ty ty_t'
    | Pi (ty, t') ->
      (* check the type of [ty] *)
      let ty_ty = _infer_ty ty in
      if not (eq ty_ty tType) && not (eq ty_ty kind) then begin
        let msg = Util.sprintf
          "in %a, variable's type should have type Type or Kind but has type %a"
          pp_debug t pp_debug ty_ty in
        raise (TypeError msg)
        end;
      (* check the type of [t'] *)
      let ty_t' = _infer_ty t' in
      if not (eq ty_t' tType) && not (eq ty_t' kind) then begin
        let msg = Util.sprintf
          "in %a, pi's type should be of type kind or Type but has type %a"
          pp_debug t pp_debug ty_t' in
        raise (TypeError msg)
        end;
      (* rturn type is [ty_t'] *)
      ty_t'
    in
    t.ty <- Some ty;
    ty

(* typing rule for [at]. [env] is used for beta-reduction/substitution under pi. *)
and _ty_rule_match ~env ty args =
  match ty.term, args with
  | _, [] -> _subst ~env ty  (* evaluate type in env *)
  | At _, _ 
  | TType, _
  | Kind, _
  | BoundVar _, _
  | Var _, _
  | Const _, _
  | Lambda _, _ ->
    let msg = Util.sprintf "expected pi type, got %a" pp_debug ty in
    raise (TypeError msg)
  | Pi (ty_expected, t'), a::args' ->
    let ty_a = _infer_ty a in
    if convertible ty_a ty_expected
      then  (* evaluate [t'] where the DB 0 is bound to [a] (dependent type) *)
        let env' = DBEnv.push env a in
        _ty_rule_match ~env:env' t' args'
      else
        let msg = Util.sprintf "in %a, expected argument of type %a but got %a:%a"
          pp_debug ty pp_debug ty_expected pp_debug a pp_debug ty_a in
        raise (TypeError msg)

let infer_ty t = _infer_ty t

let at head l =
  let t = __at head l in
  ignore (infer_ty t);
  t

let lambda ~varty t' =
  let t = __lambda ~varty t' in
  ignore (infer_ty t);
  t

let pi ~varty t' =
  let t = __pi ~varty t' in
  ignore (infer_ty t);
  t

let arrow t1 t2 =
  pi ~varty:t1 (_shift ~depth:0 1 t2)

(** {2 Beta-reduction} *)

(* apply head to l, reducing redex if any is met *)
let rec _beta_apply ~env head l =
  match head.term, l with
  | Lambda (ty, head'), a::args' ->
    (* beta redex: check that a:ty then evaluate head' under DB0=a *)
    let ty_a = _infer_ty a in
    if convertible ty ty_a
      then
        (* recurse, with DB0 bound to [a] *)
        let env = DBEnv.push env a in
        _beta_apply ~env head' args'
      else
        let msg = Util.sprintf
          "applying %a to %a: expected argument of type %a but got type %a"
            pp_debug head pp_debug a pp_debug ty pp_debug ty_a in
        raise (TypeError msg)
  | _ ->
    (* eval [head], and apply it *)
    let head = _subst ~env head in
    __at head l

(* reduce [t] into weak head normal form (no redex at root) *)
let beta_hnf t = match t.term with
  | At (head, l) ->
    (* reduce the given redex, if any *)
    _beta_apply ~env:DBEnv.empty head l
  | _ -> t

(* beta-normal form, even in subterms *)
let rec beta_nf t = match t.term with
  | Var _
  | BoundVar _
  | Const _
  | TType
  | Kind -> t
  | Pi (varty, t') -> __pi ~varty (beta_nf t')
  | Lambda (varty, t') -> __lambda ~varty (beta_nf t')
  | At (head, l) ->
    (* evaluate arguments *)
    let l = List.map beta_nf l in
    (* apply head to the arguments *)
    _beta_apply ~env:DBEnv.empty head l

(** {2 Easy constructors} *)

(* replace [sub] by a De Bruijn index in [t] with type [ty]. *)
let rec _replace ~depth ~sub ~ty t = match t.term with
  | _ when eq t sub -> bound_var ~ty depth  (* replace by bound variable *)
  | TType
  | Kind
  | Var _
  | BoundVar _
  | Const _ -> t
  | At (head, l) ->
    __at (_replace ~depth ~sub ~ty head) (_replace_list ~depth ~sub ~ty l)
  | Lambda (varty, t') ->
    __lambda ~varty (_replace ~depth:(depth+1) ~sub ~ty t')
  | Pi (varty, t') ->
    __pi ~varty (_replace ~depth:(depth+1) ~sub ~ty t')
and _replace_list ~depth ~sub ~ty l = match l with
  | [] -> []
  | x::l' -> _replace ~depth ~sub ~ty x :: _replace_list ~depth ~sub ~ty l'

let arrow_list args ret =
  match args with
  | [] -> ret
  | _::_ ->
    let n = List.length args in
    let ret = _shift ~depth:0 n ret in
    List.fold_right (fun arg ret -> __pi ~varty:arg ret) args ret

let lambda_list vars t =
  let rec mk vars t = match vars with
    | [] -> t
    | v::vars' ->
      (* type of [var] *)
      let varty = infer_ty v in
      let t' = mk vars' t in
      (* replace [v] by De Bruijn 0 in [t'] *)
      let t' = _replace ~depth:0 ~sub:v ~ty:varty (_shift ~depth:0 1 t') in
      lambda ~varty t'
  in
  mk vars t

let pi_list vars t =
  let rec mk vars t = match vars with
    | [] -> t
    | v::vars' ->
      (* type of [var] *)
      let varty = infer_ty v in
      let t' = mk vars' t in
      (* replace [v] by De Bruijn 0 in [t'] *)
      let t' = _replace ~depth:0 ~sub:v ~ty:varty (_shift ~depth:0 1 t') in
      pi ~varty t'
  in
  mk vars t

let ty_const str = const ~ty:tType (Symbol.mk_const str)

let i = ty_const "$i"
let o = ty_const "$o"
let int = ty_const "$int"
let rat = ty_const "$rat"
let real = ty_const "$real"

let (==>) args ret = arrow_list args ret
let (-->) a b = arrow_list [a] b

let term_const ?(ty=i) str = const ~ty (Symbol.mk_const str)

(** {2 Properties} *)

let is_var t = match t.term with | Var _ -> true | _ -> false
let is_bound_var t = match t.term with | BoundVar _ -> true | _ -> false
let is_const t = match t.term with | Const _ -> true | _ -> false
let is_at t = match t.term with | At _ -> true | _ -> false
let is_lambda t = match t.term with | Lambda _ -> true | _ -> false
let is_pi t = match t.term with | Pi _ -> true | _ -> false
let is_tType t = match t.term with | TType -> true | _ -> false

let is_type t = match t.term with
  | Kind -> false
  | _ -> eq (infer_ty t) tType

(** {2 IO} *)

let to_string_debug = Util.on_buffer pp_debug

let fmt fmt t = Format.pp_print_string fmt (to_string_debug t)
