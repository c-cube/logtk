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

(** {1 Solve Constraints on Orderings}

This should provide several tools to deal with constraints over term
orderings. Two main problems arise in theorem proving:
- finding a precedence that makes an ordering behave in some way
- finding instances of variables that satisfy some ordering constraints on terms
*)

(** {2 Precedence Constraint problems}

This module deals with the first issue: given some constraints that should
always be true, find a suitable ordering/precedence on them.
*)

module Precedence : sig
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

  val mk_eq : expr -> expr -> constraint_
  val mk_neq : expr -> expr -> constraint_
  val mk_le : expr -> expr -> constraint_
  val mk_lt : expr -> expr -> constraint_
  val mk_gt : expr -> expr -> constraint_
  val mk_ge : expr -> expr -> constraint_
  val mk_and : constraint_ list -> constraint_
  val mk_or : constraint_ list -> constraint_
  val mk_not : constraint_ -> constraint_
  val mk_imply : constraint_ -> constraint_ -> constraint_
  val mk_true : constraint_
  val mk_false : constraint_

  val prec_of : Symbol.t -> expr
  val weight_of : Symbol.t -> expr
  val weight0 : expr
  val const : int -> expr
  val plus : expr -> expr -> expr
  val mult : int -> expr -> expr

  val iter_expr : constraint_ -> expr Sequence.t
    (** Expressions that occur in the constraint *)

  val symbols_of_expr : ?acc:Symbol.t list -> expr -> Symbol.t list
    (** The list of symbols in this expression *)

  val symbols_of_constr : ?acc:Symbol.t list -> constraint_ -> Symbol.t list
    (** The list of symbols in this constraint *)

  val pp_expr : Buffer.t -> expr -> unit
  val fmt_expr : Format.formatter -> expr -> unit

  val pp_constraint : Buffer.t -> constraint_ -> unit
  val fmt_constraint : Format.formatter -> constraint_ -> unit

  val simplify : constraint_ -> constraint_

  type solution = {
    precedence : (Symbol.t * Symbol.t) list;  (* list of symbol > symbol *)
    weight : (Symbol.t * int) list;  (* list of symbol -> weight *)
  }

  val neg_to_constraint : solution -> constraint_
    (** Constraint that explicitely eliminate this solution *)

  val pp_solution : Buffer.t -> solution -> unit
  val fmt_solution : Format.formatter -> solution -> unit

  val solve_multiple : constraint_ list -> solution Stream.t
    (** A strea, of partial orders over symbols, that satisfy the given
        list of constraints *)
end
