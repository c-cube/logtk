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

(** {1 Search for a LPO ordering} *)

module C = OrderSolve.Precedence

module type TERM = sig
  type t

  val eq : t -> t -> bool
    (** Term syntactic equality *)

  val subterm : t -> sub:t -> bool
    (** [subterm t ~sub] true if [sub] is a subterm of [t] *)

  val head : t -> [ `Var | `App of Symbol.t * t list ]
    (** Examine the structure of this term *)
end

module type S = sig
  type term

  val orient : term -> term -> OrderSolve.Precedence.constraint_
    (** [orient a b] generates a constraint that is sufficient for [a]
        to be bigger than [b] in LPO orderings satisfying the
        constraints *)

  val orient_list : (term * term) list -> OrderSolve.Precedence.constraint_ list
    (** Orient a list of pairs *)
end

module Make(T : TERM) = struct
  type term = T.t

  (* constraint for a > b *)
  let rec orient a b =
    match T.head a, T.head b with
    | `Var, _ ->
      C.mk_false  (* a variable cannot be > *)
    | _, _ when T.subterm ~sub:b a ->
      C.mk_true  (* trivial subterm property --> ok! *)
    | `App (f, ((_::_) as l)), `App (g, l') when List.length l = List.length l' ->
      (* three cases: either some element of [l] is > [r],
          or precedence of first symbol applies,
          or lexicographic case applies (with non empty lists) *)
      C.mk_or
        [ C.mk_and
            [ C.mk_eq (C.prec_of f) (C.prec_of g)
            ; lexico_order l l' ]  (* f=g, lexicographic order of subterms *)
        ; C.mk_and
            [ C.mk_gt (C.prec_of f) (C.prec_of g)
            ; all_bigger a l'
            ]  (* f>g and a > all subterms of b *)
        ; any_bigger l b  (* some subterm of a is > b *)
        ]
    | `App (f, l), `App (g, l') ->
      (* two cases: either some element of [l] is > [r],
          or precedence of first symbol applies *)
      C.mk_or
        [ C.mk_and
            [ C.mk_gt (C.prec_of f) (C.prec_of g)
            ; all_bigger a l'
            ]  (* f>g and a > all subterms of b *)
        ; any_bigger l b  (* some subterm of a is > b *)
        ]
    | `App (f, l), _ ->
      (* only the subterm property can apply *)
      any_bigger l b

  (* constraint that some term in [l] is bigger than [b] *)
  and any_bigger l b = match l with
    | [] -> C.mk_false
    | [x] -> orient x b
    | _ -> (* any element of [l] bigger than [r]? *)
      C.mk_or (List.map (fun x -> orient x b) l)

  (* [a] bigger than all the elements of [l] *)
  and all_bigger a l = match l with
    | [] -> C.mk_true
    | [x] -> orient a x
    | _ ->
      C.mk_and (List.map (fun y -> orient a y) l)

  (* constraint for l1 >_lex l2 (lexicographic extension of LPO) *)
  and lexico_order l1 l2 =
    assert (List.length l1 = List.length l2);
    let c = List.fold_left2
      (fun constr a b ->
        match constr with
        | Some _ -> constr
        | None when T.eq a b -> None
        | None -> Some (orient a b))
      None l1 l2
    in match c with
    | None -> C.mk_false   (* they are equal *)
    | Some c -> c

  let orient_list l =
    List.map (fun (l,r) ->
      let c = orient l r in
      let c' = C.simplify c in
      (if !Util.level >= 4 && c <> c' then
        Util.printf "%% constr %a simplified into %a\n"
          Solve.Constraint.pp c Solve.Constraint.pp c');
      c') l
end

module FO = Make(struct
  type t = FOTerm.t
  let eq = FOTerm.eq
  let subterm t ~sub = FOTerm.subterm ~sub t
  let head t = match t.FOTerm.term with
    | FOTerm.Var _
    | FOTerm.BoundVar _ -> `Var
    | FOTerm.Node (s, l) -> `App (s, l)
end)

module Untyped = Make(struct
  type t = Untyped.FO.t
  let eq = Untyped.FO.t
  let subterm t ~sub = failwith "Untyped.subterm: not implemented" (* TODO *)
  let head t = match t with
    | Untyped.FO.Var _ -> `Var
    | Untyped.FO.App (s, l) -> `App (s, l)
end)
