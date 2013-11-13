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

(** {1 Solve constraints} *)

(** {2 Precedence Constraint problems} *)

module Precedence = struct
  type expr =
    | Prec of Symbol.t
    | Weight of Symbol.t
    | Weight0
    | Const of int
    | Plus of expr * expr
    | Mult of int * expr

  type constraint_ =
    | EQ of expr * expr
    | LE of expr * expr
    | LT of expr * expr
    | And of constraint_ list
    | Or of constraint_ list
    | Not of constraint_
    | True   (* tautology *)
    | False  (* impossible constraint *)

  let mk_eq a b = EQ (a,b)
  let mk_neq a b = Not (EQ (a, b))
  let mk_le a b = LE (a,b)
  let mk_lt a b = LT (a,b)
  let mk_ge a b = LE (b,a)
  let mk_gt a b = LT (b,a)
  let mk_and l = And l
  let mk_or l = Or l
  let mk_not t = Not t
  let mk_imply a b = Or [Not a; b]
  let mk_true = True
  let mk_false = False

  let prec_of s = Prec s
  let weight_of s = Weight s
  let weight0 = Weight0
  let const i = Const i
  let plus a b = Plus (a, b)
  let mult i a = Mult (i, a)

  let iter_expr c =
    Sequence.from_iter
      (fun k -> 
        let rec iter c = match c with
        | EQ (a,b) | LE(a,b) | LT(a,b) -> k a; k b
        | And l | Or l -> List.iter iter l
        | Not t -> iter t
        | True | False -> ()
        in
        iter c)

  let symbols_of_expr ?(acc=[]) e =
    let rec recurse acc e = match e with
    | Prec s
    | Weight s ->
      if List.mem s acc then acc else s :: acc
    | Const _ | Weight0 -> acc
    | Mult (_, a) -> recurse acc a
    | Plus (a, b) -> recurse (recurse acc a) b
    in recurse acc e 

  let symbols_of_constr ?(acc=[]) c =
    Sequence.fold (fun acc t -> symbols_of_expr ~acc t) acc (iter_expr c)

  let rec pp_expr buf e = match e with
    | Prec s -> Printf.bprintf buf "(prec %a)" Symbol.pp s
    | Weight s -> Printf.bprintf buf "(w %a)" Symbol.pp s
    | Weight0 -> Buffer.add_string buf "w0"
    | Const i -> Printf.bprintf buf "%d" i
    | Plus (a, b) -> Printf.bprintf buf "(+ %a %a)" pp_expr a pp_expr b
    | Mult (i, a) -> Printf.bprintf buf "(* %d %a)" i pp_expr a

  let fmt_expr fmt e =
    let buf = Buffer.create 15 in
    pp_expr buf e;
    Format.pp_print_string fmt (Buffer.contents buf)

  let rec pp_constraint buf t = match t with
    | EQ (a,b) -> Printf.bprintf buf "(= %a %a)" pp_expr a pp_expr b
    | LE (a,b) -> Printf.bprintf buf "(<= %a %a)" pp_expr a pp_expr b
    | LT (a,b) -> Printf.bprintf buf "(< %a %a)" pp_expr a pp_expr b
    | And l -> Printf.bprintf buf "(and %a)" (Util.pp_list ~sep:" " pp_constraint) l
    | Or l -> Printf.bprintf buf "(or %a)" (Util.pp_list ~sep:" " pp_constraint) l
    | Not t -> Printf.bprintf buf "(not %a)" pp_constraint t
    | True -> Buffer.add_string buf "true"
    | False -> Buffer.add_string buf "false"

  let fmt_constraint fmt t =
    let buf = Buffer.create 15 in
    pp_constraint buf t;
    Format.pp_print_string fmt (Buffer.contents buf)

  (* simplify the constraints *)
  let rec simplify t =
    match t with
    | EQ (a, b) when a = b -> mk_true
    | LE (a, b) when a = b -> mk_true
    | LT (a, b) when a = b -> mk_false
    | Not (Not t) -> simplify t
    | Not True -> mk_false
    | Not False -> mk_true
    | Not (And l) -> simplify (mk_or (List.map mk_not l))
    | Not (Or l) -> simplify (mk_and (List.map mk_not l))
    | And [] -> mk_true
    | Or [] -> mk_true
    | And [x] -> simplify x
    | Or [x] -> simplify x
    | And l ->
      let l' = List.fold_left flatten_and [] l in
      begin match l' with
      | [] -> mk_true
      | [x] -> x
      | _ when List.mem mk_false l' -> mk_false
      | _ -> mk_and l'
      end
    | Or l ->
      let l' = List.fold_left flatten_or [] l in
      begin match l' with
      | [] -> mk_false
      | [x] -> x
      | _ when List.mem mk_true l' -> mk_true
      | _ -> mk_or l'
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

  type solution = {
    precedence : (Symbol.t * Symbol.t) list;  (* list of symbol > symbol *)
    weight : (Symbol.t * int) list;  (* list of symbol -> weight *)
  }

  (* constraint that prohibits this solution. It has the form
     Or_i(prec(a_i) <= prec(b_i)) or Or_j(weight(a_j) != w_j)
     for a_i, b_i in sol.precedence
     and a_j, w_j in sol.weight *)
  let neg_to_constraint sol =
    let l = List.map (fun (a,b) -> mk_le (prec_of a) (prec_of b)) sol.precedence in
    let l' = List.map (fun (s,i) -> mk_neq (weight_of s) (const i)) sol.weight in
    mk_or (List.rev_append l l')

  let pp_solution buf s =
    let pp_pair buf (a,b) =
      Printf.bprintf buf "%a > %a" Symbol.pp a Symbol.pp b
    and pp_weight buf (a, i) =
      Printf.bprintf buf "%a:%d" Symbol.pp a i
    in
    Printf.bprintf buf "{ precedence: %a;\nweights: %a\n}"
      (Util.pp_list pp_pair) s.precedence
      (Util.pp_list pp_weight) s.weight

  let fmt_solution fmt s =
    let buf = Buffer.create 15 in
    pp_solution buf s;
    Format.pp_print_string fmt (Buffer.contents buf)

  (* TODO: reduce constraints to solved form, check each possibility using
     PartialOrder/Congruence ? *)

  let solve_multiple l = assert false
  (*
    (if !Util.level >= 3 then begin
      Util.printf "%% solve constraints:\n";
      List.iter (fun c -> Util.printf "%%    %a\n" pp_constraint c) l;
      end);
    let module S = MakeSolver(struct end) in
    try
      S.add_list l;
      let solution = ref None in
      Stream.from
        (fun _ ->
          try
            (* first, forbid previous solution *)
            begin match !solution with
            | None -> ()
            | Some s -> S.forbid_solution s
            end;
            (* check if another solution is available *)
            S.check_sat ();
            let s = S.get_solution () in
            solution := Some s;
            Some s
          with Aez.Smt.Unsat _ ->
            None)
    with Aez.Smt.Unsat _ ->
      Stream.from (fun _ -> None)  (* no solution at all *)
  *)
end
