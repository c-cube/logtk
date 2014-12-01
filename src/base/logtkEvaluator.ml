
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

(** {1 Evaluation of terms and formulas}
This module provides utilities to register functions to evaluate terms
on a per-symbol basis. Each symbol can have its own evaluator, that is
called to normalize terms whose head is the symbol. That is especially
handy for arithmetic, where there are many distinct interpreted symbols.
*)

(** {2 Signature of evaluator for some terms} *)

module type S = LogtkEvaluator_intf.S

module Sym = LogtkSymbol
module Ty = LogtkType
module Form = LogtkFormula.FO
module Util = LogtkUtil

module FO = struct
  module T = LogtkFOTerm
  type term = T.t

  type eval_fun = tyargs:Ty.t list -> Sym.t -> term list -> term option

  type t = eval_fun list Sym.Tbl.t

  let create () = Sym.Tbl.create 13

  let copy = Sym.Tbl.copy

  let register t s f =
    try
      let l = Sym.Tbl.find t s in
      Sym.Tbl.replace t s (f::l)
    with Not_found ->
      Sym.Tbl.add t s [f]

  let register_list t l =
    List.iter (fun (s, f) -> register t s f) l

  let interpreted t s = Sym.Tbl.mem t s

  let rec _eval_head ev funs ~tyargs s l = match funs with
    | [] -> T.app_full s tyargs l
    | f::funs' ->
      match f ~tyargs s l with
      | None -> _eval_head ev funs' ~tyargs s l
      | Some t' -> eval_head ev t'

  (* non-recursive evaluation *)
  and eval_head ev t =
    let hd, tyargs, l = T.open_app t in
    match T.view hd with
    | T.Var _
    | T.BVar _ -> t
    | T.Const s ->
      begin try
        (* evaluate constant *)
        let funs = Sym.Tbl.find ev s in
        _eval_head ev funs ~tyargs t l
      with Not_found ->
        t
      end
    | T.App _ | T.TyApp _ -> t

  (* recursive evaluation *)
  let rec eval ev t = match T.view t with
    | T.Const _
    | T.Var _
    | T.BVar _ -> t
    | T.TyApp (f, ty) ->
      let f = eval ev f in
      T.tyapp f ty
    | T.App (f, l) ->
      (* evaluate subterms, then the term itself *)
      let l' = List.map (eval ev) l in
      let t' = T.app f l' in
      let t'' = eval_head ev t' in
      if T.eq t' t''
        then t'
        else eval ev t''

  let eval_form ev f =
    Form.map_depth (fun _ t -> eval ev t) f

  let app ?(tyargs=[]) ev hd l =
    match T.view hd with
    | T.Const s ->
      begin try
        let funs = Sym.Tbl.find ev s in
        _eval_head ev funs ~tyargs hd l
      with Not_found ->
        T.app_full hd tyargs l
      end
    | _ -> T.app_full hd tyargs l

  module S = Sym

  (* helpers *)

  let _match t =
    let hd, tyargs, l = T.open_app t in
    match T.view hd, l with
    | T.Const s, [] -> `Const (s, T.ty t)
    | T.Const s, _ -> `Node (s, tyargs, l)
    | _ -> `None
  let _unary l = match l with
    | [a] -> _match a
    | _ -> `None
  let _binary l = match l with
    | [a; b] -> `Binary (_match a, a, _match b, b)
    | _ -> `None

  let _ev_uminus ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.uminus n))  (* - n ----> -n *)
    | `Node (s', _, [t']) when S.eq s' Sym.TPTP.Arith.uminus ->
      Some t'  (* -(-t) --> t *)
    | _ -> None

  let _ev_floor ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.floor n))
    | _ -> None

  let _ev_ceiling ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.ceiling n))
    | _ -> None

  let _ev_round ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.round n))
    | _ -> None

  let _ev_truncate ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.truncate n))
    | _ -> None

  let _ev_is_int ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (if S.is_int n then T.TPTP.true_ else T.TPTP.false_)
    | _ -> None

  let _ev_is_rat ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (if S.is_rat n then T.TPTP.true_ else T.TPTP.false_)
    | _ -> None

  let _ev_to_int ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.to_int n))
    | _ -> None

  let _ev_to_rat ~tyargs s l = match _unary l with
    | `Const (n,ty) when S.is_numeric n ->
      Some (T.const ~ty (Sym.ArithOp.to_rat n))
    | _ -> None

  let _ev_sum ~tyargs _ l =
    Util.debug 5 "evaluate sum %a" (Util.pp_list T.pp) l;
    match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.sum n1 n2))
    | `Binary (`Const (n1,ty), _, _, t2) when Sym.ArithOp.is_zero n1 ->
      Some t2 (* 0+x --> x *)
    | `Binary (_, t1, `Const (n2,ty), _) when Sym.ArithOp.is_zero n2 ->
      Some t1 (* x+0 --> x *)
    | `Binary (_, t1, _, t2) when T.cmp t1 t2 > 0 ->
      (* AC-normalization *)
      Some (T.app_full (T.const ~ty:(T.ty t1) Sym.TPTP.Arith.sum) tyargs [t2; t1])
    | _ -> None

  let mk_sum ~ty ~tyargs l =
    match _ev_sum ~tyargs Sym.TPTP.Arith.sum l with
    | None -> T.app_full (T.const ~ty Sym.TPTP.Arith.sum) tyargs l
    | Some t' -> t'

  let _ev_difference ~tyargs _ l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.difference n1 n2))
    | `Binary (`Const (n1,ty), _, _, t2) when Sym.ArithOp.is_zero n1 ->
      Some (T.app_full (Sym.TPTP.Arith.uminus  ~ty ~tyargs:[T.ty t2] [t2])  (* 0-x --> -x *)
    | `Binary (_, t1, `Const (n2,ty), _) when Sym.ArithOp.is_zero n2 ->
      Some t1  (* x-0 ---> x *)
    | `Binary (_, t1, `Node(s',_,[t']), _) when S.eq s' Sym.TPTP.Arith.uminus ->
      Some (mk_sum ~tyargs [t1; t'])   (* x- (-y) ---> x + y *)
    | _ -> None

  let mk_difference ~tyargs l =
    match _ev_difference ~tyargs Sym.TPTP.Arith.difference l with
    | None -> T.app ~tyargs Sym.TPTP.Arith.difference l
    | Some t' -> t'

  let rec _ev_product ~tyargs _ l =
    Util.debug 5 "evaluate product %a" (Util.pp_list T.pp) l;
    match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.product n1 n2))
    | `Binary (`Const (n1,ty), _, _, _) when Sym.TPTP.Arith.is_zero n1 ->
      Some (T.const ~ty n1) (* 0*x --> 0 *)
    | `Binary (_, _, `Const (n2,ty), _) when Sym.TPTP.Arith.is_zero n2 ->
      Some (T.const ~ty n2)  (* x*0 --> 0 *)
    | `Binary (`Const (n1,ty), _, _, t2) when Sym.TPTP.Arith.is_one n1 ->
      Some t2 (* 1*x --> x *)
    | `Binary (_, t1, `Const (n2,ty), _) when Sym.TPTP.Arith.is_one n2 ->
      Some t1 (* x*1 --> x *)
    | ( `Binary (`Node (s, _, [a;b]), _, _, c)
      | `Binary (_, c, `Node (s, _, [a;b]), _) )
      when S.eq s Sym.TPTP.Arith.sum ->
      (* distributivity over sum *)
      Some (mk_sum ~tyargs
        [ mk_product ~tyargs [a; c]
        ; mk_product ~tyargs [b; c]
        ])
    | ( `Binary (`Node (s,_,[a;b]), _, _, c)
      | `Binary (_, c, `Node (s, _, [a;b]), _) )
      when S.eq s Sym.TPTP.Arith.difference ->
      (* distributivity over difference *)
      Some (mk_difference ~tyargs
        [ mk_product ~tyargs [a; c]
        ; mk_product ~tyargs [b; c]
        ])
    | `Binary (_, t1, _, t2) when t1.T.tag > t2.T.tag ->
      (* AC-normalization *)
      Some (T.app ~tyargs Sym.TPTP.Arith.product [t2; t1])
    | _ -> None
  and mk_product ~tyargs l =
    match _ev_product ~tyargs Sym.TPTP.Arith.product l with
    | None -> T.app ~tyargs Sym.TPTP.Arith.product l
    | Some t' -> t'

  let _ev_quotient ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      begin try
        Some (T.const ~ty (Sym.ArithOp.quotient n1 n2))
      with Division_by_zero -> None
      end
    | `Binary (_, t1, `Const (n2,ty), _) when Sym.TPTP.Arith.is_one n2 ->
      Some t1
    | _ -> None

  let _ev_quotient_e ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.quotient_e n1 n2))
    | _ -> None

  let _ev_quotient_f ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.quotient_f n1 n2))
    | _ -> None

  let _ev_quotient_t ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.quotient_t n1 n2))
    | _ -> None

  let _ev_remainder_e ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.remainder_e n1 n2))
    | _ -> None

  let _ev_remainder_f ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.remainder_f n1 n2))
    | _ -> None

  let _ev_remainder_t ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (T.const ~ty (Sym.ArithOp.remainder_t n1 n2))
    | _ -> None

  let _ev_less ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (if Sym.ArithOp.less n1 n2 then T.TPTP.true_ else T.TPTP.false_)
    | _ -> None

  let _ev_lesseq ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (if Sym.ArithOp.lesseq n1 n2 then T.TPTP.true_ else T.TPTP.false_)
    | _ -> None

  let _ev_greater ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (if Sym.ArithOp.greater n1 n2 then T.TPTP.true_ else T.TPTP.false_)
    | _ -> None

  let _ev_greatereq ~tyargs s l = match _binary l with
    | `Binary (`Const (n1,ty), _, `Const (n2,_), _)
      when S.is_numeric n1 && S.is_numeric n2 ->
      Some (if Sym.ArithOp.greatereq n1 n2 then T.TPTP.true_ else T.TPTP.false_)
    | _ -> None

  let arith =
    [ Sym.TPTP.Arith.uminus, _ev_uminus
    ; Sym.TPTP.Arith.floor, _ev_floor
    ; Sym.TPTP.Arith.ceiling, _ev_ceiling
    ; Sym.TPTP.Arith.round, _ev_round
    ; Sym.TPTP.Arith.truncate, _ev_truncate
    ; Sym.TPTP.Arith.is_int, _ev_is_int
    ; Sym.TPTP.Arith.is_rat, _ev_is_rat
    ; Sym.TPTP.Arith.to_int, _ev_to_int
    ; Sym.TPTP.Arith.to_rat, _ev_to_rat
    ; Sym.TPTP.Arith.sum, _ev_sum
    ; Sym.TPTP.Arith.difference, _ev_difference
    ; Sym.TPTP.Arith.product, _ev_product
    ; Sym.TPTP.Arith.quotient, _ev_quotient
    ; Sym.TPTP.Arith.quotient_e, _ev_quotient_e
    ; Sym.TPTP.Arith.quotient_f, _ev_quotient_f
    ; Sym.TPTP.Arith.quotient_t, _ev_quotient_t
    ; Sym.TPTP.Arith.remainder_e, _ev_remainder_e
    ; Sym.TPTP.Arith.remainder_f, _ev_remainder_f
    ; Sym.TPTP.Arith.remainder_t, _ev_remainder_t
    ; Sym.TPTP.Arith.less, _ev_less
    ; Sym.TPTP.Arith.lesseq, _ev_lesseq
    ; Sym.TPTP.Arith.greater, _ev_greater
    ; Sym.TPTP.Arith.greatereq, _ev_greatereq
    ]

  let with_arith ev =
    register_list ev arith
end
