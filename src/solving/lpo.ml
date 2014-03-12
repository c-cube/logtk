
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

(** {1 Constraint Solving for LPO} *)

open Logtk

(** {6 Constraints} *)

module Constraint = struct
  type expr = Symbol.t

  type t =
    | EQ of expr * expr
    | LE of expr * expr
    | LT of expr * expr
    | And of t list
    | Or of t list
    | Not of t
    | True   (* tautology *)
    | False  (* impossible constraint *)


  let eq a b = EQ (a,b)
  let neq a b = Not (EQ (a, b))
  let le a b = LE (a,b)
  let lt a b = LT (a,b)
  let ge a b = LE (b,a)
  let gt a b = LT (b,a)
  let and_ l = And l
  let or_ l = Or l
  let not_ t = Not t
  let imply a b = Or [Not a; b]
  let true_ = True
  let false_ = False

  module Seq = struct
    let exprs c k =
      let rec iter c = match c with
      | EQ (a,b) | LE(a,b) | LT(a,b) -> k a; k b
      | And l | Or l -> List.iter iter l
      | Not t -> iter t
      | True | False -> ()
      in
      iter c
  end

  let pp_expr = Symbol.pp

  let rec pp buf t = match t with
    | EQ (a,b) -> Printf.bprintf buf "(= %a %a)" pp_expr a pp_expr b
    | LE (a,b) -> Printf.bprintf buf "(<= %a %a)" pp_expr a pp_expr b
    | LT (a,b) -> Printf.bprintf buf "(< %a %a)" pp_expr a pp_expr b
    | And l -> Printf.bprintf buf "(and %a)" (Util.pp_list ~sep:" " pp) l
    | Or l -> Printf.bprintf buf "(or %a)" (Util.pp_list ~sep:" " pp) l
    | Not t -> Printf.bprintf buf "(not %a)" pp t
    | True -> Buffer.add_string buf "true"
    | False -> Buffer.add_string buf "false"

  let fmt fmt t =
    Format.pp_print_string fmt (Util.on_buffer pp t)

  (* simplify the constraints *)
  let rec simplify t =
    match t with
    | EQ (a, b) when Symbol.eq a b -> true_
    | LE (a, b) when Symbol.eq a b -> true_
    | LT (a, b) when Symbol.eq a b -> false_
    | Not (Not t) -> simplify t
    | Not True -> true_
    | Not False -> true_
    | Not (And l) -> simplify (or_ (List.map not_ l))
    | Not (Or l) -> simplify (and_ (List.map not_ l))
    | And [] -> true_
    | Or [] -> true_
    | And [x] -> simplify x
    | Or [x] -> simplify x
    | And l ->
      let l' = List.fold_left flatten_and [] l in
      begin match l' with
      | [] -> true_
      | _ when List.mem false_ l' -> false_
      | [x] -> x
      | _ -> and_ l'
      end
    | Or l ->
      let l' = List.fold_left flatten_or [] l in
      begin match l' with
      | [] -> false_
      | _ when List.mem true_ l' -> true_
      | [x] -> x
      | _ -> or_ l'
      end
    | _ -> t
  and flatten_or acc t = match simplify t with
    | False -> acc
    | Or l -> List.fold_left flatten_or acc l
    | t' -> t' :: acc
  and flatten_and acc t = match simplify t with
    | True -> acc
    | And l -> List.fold_left flatten_and acc l
    | t' -> t' :: acc
end

module Solution = struct
  type t = (Symbol.t * Symbol.t) list

  (* constraint that prohibits this solution. We build the
     clause that makes at least one a>b false. *)
  let neg_to_constraint sol =
    let module C = Constraint in
    let l = List.map (fun (a,b) -> C.le a b) sol in
    C.or_ l

  let pp buf s =
    let pp_pair buf (a,b) =
      Printf.bprintf buf "%a > %a" Symbol.pp a Symbol.pp b
    in
    Util.pp_list pp_pair buf s

  let fmt fmt s =
    Format.pp_print_string fmt (Util.on_buffer pp s)
end

module C = Constraint

(** Functor to use Sat, and encode/decode the solution *)
module MakeSolver(X : sig end) = struct
  (* propositional atoms map symbols to the binary digits of
     their index in the precedence *)
  type atom =
    | Digit_n of Symbol.t * int

  let atom_to_hstring = function
    | Digit_n (s, i) -> Aez.Hstring.make (Util.sprintf "prec_%a_%d" Symbol.pp s i)

  (* sat solver *)
  module S = Aez.Smt.Make(struct end)

  let __declared = ref Aez.Hstring.HSet.empty

  let atom_to_term a =
    let s = atom_to_hstring a in
    (* declare type, if needed *)
    if not (Aez.Hstring.HSet.mem s !__declared) then begin
      Aez.Smt.Symbol.declare s [] Aez.Smt.Type.type_proc;
      __declared := Aez.Hstring.HSet.add s !__declared
    end;
    (* build constant *)
    Aez.Smt.Term.make_app s []

  (* get the propositional variable that represents the n-th bit of [s] *)
  let digit s n = atom_to_term (Digit_n (s, n))

  module F = Aez.Smt.Formula

  (* encode [a < b]_n where [n] is the number of digits.
      either the n-th digit of [a] is false and the one of [b] is true,
      or they are equal and [a < b]_{n-1}.
      @return a formula *)
  let rec encode_lt ~n a b =
    if n = 0 then F.f_false
    else
      let d_a = F.make_pred (digit a n) and d_b = F.make_pred (digit b n) in
      F.make_or
        [ F.make_and [ F.make_not d_a; d_b ]
        ; F.make_and [ F.make_equiv d_a d_b; encode_lt ~n:(n-1) a b]
        ]

  (* encode [a <= b]_n, with not [b < a]_n. *)
  let encode_leq ~n a b =
    F.make_not (encode_lt ~n b a)

  (* encode [a = b]_n, digit per digit *)
  let rec encode_eq ~n a b =
    if n = 0 then F.f_true
    else
      let d_a = F.make_pred (digit a n) and d_b = F.make_pred (digit b n) in
      F.make_and [ F.make_equiv d_a d_b; encode_eq ~n:(n-1) a b ]

  (* encode a constraint with [n] bits into a formula. *)
  let rec encode_constr ~n c = match c with
    | C.EQ(a,b) -> encode_eq ~n a b
    | C.LE(a,b) -> encode_leq ~n a b
    | C.LT(a,b) -> encode_lt ~n a b
    | C.And l -> F.make_and (List.rev_map (encode_constr ~n) l)
    | C.Or l -> F.make_or (List.rev_map (encode_constr ~n) l)
    | C.Not c' -> F.make_not (encode_constr ~n c')
    | C.True -> F.f_true
    | C.False -> F.f_false

  (* function to extract symbol -> int from a solution *)
  let int_of_symbol ~n s =
    let r = ref 0 in
    for i = n downto 1 do
      let lit = digit s i in
      let is_true = S.eval lit in
      if is_true
        then r := 2 * !r + 1
        else r := 2 * !r
    done;
    Util.debug 3 "index of symbol %a in precedence is %d" Symbol.pp s !r;
    !r

  (* extract a solution *)
  let get_solution ~n symbols =
    let syms = List.rev_map (fun s -> int_of_symbol ~n s, s) symbols in
    (* sort in increasing order *)
    let syms = List.sort (fun (n1,_)(n2,_) -> n1-n2) syms in
    (* build solution by imposing f>g iff n(f) > n(g) *)
    let _, _, sol = List.fold_left
      (fun (cur_n,other_s,acc) (n,s) ->
        if n = cur_n
          then n, s::other_s, acc   (* yet another symbol with rank [n] *)
          else begin
            (* elements of [other_s] have a lower rank, force them to be smaller  *)
            assert (cur_n < n);
            let acc = List.fold_left
              (fun acc s' -> (s, s') :: acc)
              acc other_s
            in n, [s], acc
          end
      ) (~-1,[],[]) syms
    in
    sol

  (* solve the given list of constraints *)
  let solve_list l =
    (* count the number of symbols *)
    let symbols =
      Sequence.of_list l
        |> Sequence.flatMap C.Seq.exprs
        |> Symbol.Seq.add_set Symbol.Set.empty
        |> Symbol.Set.elements
    in
    let num = List.length symbols in
    (* the number of digits required to map each symbol to a distinct int *)
    let n = int_of_float (ceil (log (float_of_int num) /. log 2.)) in
    Util.debug 2 "constraints on %d symbols -> %d digits (%d bool vars)"
      num n (n * num);
    try
      List.iteri
        (fun i c ->
          Util.debug 5 "encode constr %a..." C.pp c;
          let f = encode_constr ~n c in
          Format.pp_print_flush Format.std_formatter ();
          S.assume ~id:i f;
          Util.debug 5 "form assumed")
        l;
      (* generator of solutions *)
      let rec next () =
        try
          Util.debug 5 "check satisfiability";
          S.check ();
          Util.debug 5 "next solution exists, try to extract it...";
          let solution = get_solution ~n symbols in
          Util.debug 5 "... solution is %a" Solution.pp solution;
          (* obtain another solution: negate current one and continue *)
          let tl = lazy (negate ~n solution) in
          LazyList.Cons (solution, tl)
        with Aez.Smt.Unsat _ ->
          LazyList.Nil
      and negate ~n solution =
        (* negate current solution to get the next one... if any *)
        let c = Solution.neg_to_constraint solution in
        let f = encode_constr ~n c in
        try
          S.assume ~id:0 f;
          S.check ();
          next()
        with Aez.Smt.Unsat _ -> LazyList.Nil
      in
      lazy (next())
    with Aez.Smt.Unsat _ -> LazyList.nil
end

let solve_multiple l =
  let l = List.rev_map C.simplify l in
  Util.debug 2 "lpo: solve constraints %a" (Util.pp_list C.pp) l;
  let module S = MakeSolver(struct end) in
  S.solve_list l

(** {6 LPO} *)

  (* constraint that some term in [l] is bigger than [b] *)
let any_bigger ~orient_lpo l b  = match l with
  | [] -> C.false_
  | [x] -> orient_lpo x b
  | _ -> (* any element of [l] bigger than [r]? *)
    C.or_ (List.rev_map (fun x -> orient_lpo x b) l)

(* [a] bigger than all the elements of [l] *)
and all_bigger ~orient_lpo a l = match l with
  | [] -> C.true_
  | [x] -> orient_lpo a x
  | _ ->
    C.and_ (List.rev_map (fun y -> orient_lpo a y) l)

(* constraint for l1 >_lex l2 (lexicographic extension of LPO) *)
and lexico_order ~eq ~orient_lpo l1 l2 =
  assert (List.length l1 = List.length l2);
  let c = List.fold_left2
    (fun constr a b ->
      match constr with
      | Some _ -> constr
      | None when eq a b -> None
      | None -> Some (orient_lpo a b))
    None l1 l2
  in match c with
  | None -> C.false_   (* they are equal *)
  | Some c -> c

module FO = struct
  module T = FOTerm
  type term = T.t
  module TC = T.Classic

  (* constraint for a > b *)
  let rec orient_lpo a b =
    match TC.view a, TC.view b with
    | (TC.Var _ | TC.BVar _), _ ->
      C.false_  (* a variable cannot be > *)
    | _, _ when T.subterm b a ->
      C.true_  (* trivial subterm property --> ok! *)
    | TC.App (f, ((_::_) as l)), TC.App (g, l')
    when List.length l = List.length l' ->
      (* three cases: either some element of [l] is > [r],
          or precedence of first symbol applies,
          or lexicographic case applies (with non empty lists) *)
      C.or_
        [ C.and_
            [ C.eq f g
            ; lexico_order ~eq:T.eq ~orient_lpo l l' ]
              (* f=g, lexicographic order of subterms *)
        ; C.and_
            [ C.gt f g
            ; all_bigger ~orient_lpo a l'
            ]  (* f>g and a > all subterms of b *)
        ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
        ]
    | TC.App (f, l), TC.App (g, l') ->
      (* two cases: either some element of [l] is > [r],
          or precedence of first symbol applies *)
      C.or_
        [ C.and_
            [ C.gt f g
            ; all_bigger ~orient_lpo a l'
            ]  (* f>g and a > all subterms of b *)
        ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
        ]
    | TC.App (f, l), _ ->
      (* only the subterm property can apply *)
      any_bigger ~orient_lpo l b
    | TC.NonFO, _ ->
      (* no clue... *)
      C.false_

  let orient_lpo_list l =
    List.map
      (fun (l,r) ->
        let c = orient_lpo l r in
        let c' = C.simplify c in
        Util.debug 2 "constr %a simplified into %a" C.pp c C.pp c';
        c')
      l
end

module Prolog = struct
  module T = PrologTerm
  type term = T.t

  (* constraint for a > b *)
  let rec orient_lpo a b =
    match T.view a, T.view b with
    | T.Var _ , _ ->
      C.false_  (* a variable cannot be > *)
    | _ when T.eq a b -> C.false_
    | _ when T.subterm ~strict:true a ~sub:b ->
      C.true_  (* trivial subterm property --> ok! *)
    | T.App ({T.term=T.Const f}, ((_::_) as l)), T.App ({T.term=T.Const g}, l')
    when List.length l = List.length l' ->
      (* three cases: either some element of [l] is > [r],
          or precedence of first symbol applies,
          or lexicographic case applies (with non empty lists) *)
      C.or_
        [ C.and_
            [ C.eq f g
            ; lexico_order ~eq:T.eq ~orient_lpo l l' ]  (* f=g, lexicographic order of subterms *)
        ; C.and_
            [ C.gt f g
            ; all_bigger ~orient_lpo a l'
            ]  (* f>g and a > all subterms of b *)
        ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
        ]
    | T.App ({T.term=T.Const f}, l), T.App ({T.term=T.Const g}, l') ->
      (* two cases: either some element of [l] is > [r],
          or precedence of first symbol applies *)
      C.or_
        [ C.and_
            [ C.gt f g
            ; all_bigger ~orient_lpo a l'
            ]  (* f>g and a > all subterms of b *)
        ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
        ]
    | T.App ({T.term=T.Const _}, l), _ ->
      (* only the subterm property can apply *)
      any_bigger ~orient_lpo l b
    | _ ->
      (* no clue... *)
      C.false_

  let orient_lpo_list l =
    List.map
      (fun (l,r) ->
        let c = orient_lpo l r in
        let c' = C.simplify c in
        Util.debug 2 "constr %a simplified into %a" C.pp c C.pp c';
        c')
      l
end