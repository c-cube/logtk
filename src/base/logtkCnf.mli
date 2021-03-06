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

(** {1 Reduction to CNF and simplifications} *)

type form = LogtkFormula.FO.t
type symbol = LogtkSymbol.t

(** See "computing small normal forms", in the handbook of automated reasoning.
    All transformations are made on curried terms and formulas. *)

val is_cnf : form -> bool
  (** Is the formula in CNF? *)

val is_lit : form -> bool
  (** Literal? *)

val is_clause : form list -> bool
  (** Is it a clause, ie a list of literals? *)

val miniscope : ?distribute_exists:bool -> form -> form
  (** Apply miniscoping transformation to the term.
      @param distribute_exists see whether ?X:(p(X)|q(X)) should be
        transformed into (?X: p(X) | ?X: q(X)). Default: [false] *)

type clause = form list
  (** Basic clause representation, as list of literals *)

type options =
  | DistributeExists
  | DisableRenaming
  | InitialProcessing of (form -> form) (** any processing, at the beginning *)
  | PostNNF of (form -> form)  (** any processing that keeps negation at leaves *)
  | PostSkolem of (form -> form) (** must not introduce variables nor negations *)

(** Options are used to tune the behavior of the CNF conversion.

- DistributeExists if enabled, will distribute existential quantifiers over
    disjunctions. This can make skolem symbols smaller (smaller arity) but
    introduce more of them.
- DisableRenaming disables formula renaming. Can re-introduce the worst-case
    exponential behavior of CNF.
- InitialProcessing a simplification function that is called before CNF starts.
- PostNNF transformation applied just after reduction to NNF. Its output
    must not break the NNF form (negation at root only).
- PostSkolem transformation applied just after skolemization. It must not
    break skolemization nor NNF (no quantifier, no non-leaf negation).
- DefLimit number of expected clauses above which a sub-formula is
    renamed (unless [DisableRenaming] is present).
*)

val cnf_of : ?opts:options list -> ?ctx:LogtkSkolem.ctx ->
             form -> clause list
  (** LogtkTransform the clause into proper CNF; returns a list of clauses.
      Options are used to tune the behavior. *)

val cnf_of_list : ?opts:options list -> ?ctx:LogtkSkolem.ctx ->
                  form list -> clause list
