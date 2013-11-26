
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

(** {1 Functorial Unification}

This provides a general purpose implementation of unification *)

type possibility =
  | Possible    (** unification is possible *)
  | Impossible  (** unification is impossible *)
  | Trivial     (** unification is trivial (already equal) *)

type scope = int

exception Fail
  (** Raised when unification/matching fails *)

(** {2 Abstraction on Terms} *)
module type TERM = sig
  type symbol

  type t

  val eq : t -> t -> bool
    (** Equality of terms *)

  val is_var : t -> bool
    (** Is the term a (free) variable that can be bound? *)

  val eq_head : t -> t -> bool
    (** Check whether two terms are equal ASSUMING their subterms
        are equal. For instance, this should return [true]
        for [f(a,b)] and [f(a, c)], but [false] for
        [f(a,b)] and [g(a,b)]. On variables, just means that
        the variables are the same. *)

  val subterms : t -> t list
    (** List of subterms that are to be unified recursively *)

  val can_unify : t -> scope -> t -> scope -> possibility
    (** Specific criterion that might check whether those terms can
        possibly be unified. *)

  val can_match : t -> scope -> t -> scope -> possibility
    (** Specific criterion that might check whether the first term can
        be a generalization of the second one. *)

  val can_be_alpha_equiv : t -> scope -> t -> scope -> possibility
    (** Specific criterion that might check whether the first term and
        the second term can be alpha-equivalent (same up to renaming) *)
end

(** {2 Abstraction on substitutions} *)
module type SUBST = sig
  type term

  type t

  val empty : t
    (** Empty substitution *)

  val bind : t -> term -> scope -> term -> scope -> t
    (** Bind a variable (the first term) to another term. *)

  val lookup : t -> term -> scope -> term * scope
    (** Look the given variable up in the substitution.
        @raise Invalid_argument if the term is not a variable
        @raise Not_found if the variable is not bound *)

  val get_var : t -> term -> scope -> term * scope
    (** Dereference recursively variables (in a given scope) until we
        get a non-bound variable, or a non-variable term.
        Corresponds to calling [lookup] until it raises Not_found. *)

  val pre_unify : t -> term -> scope -> term -> scope -> t
    (** Pre-unification: term-specific unification, for instance,
        unification of the types (or type arguments) if needed.
        If it doesn't make sense, just returning the substitution
        is fine. *)
end

(** {2 Unification module} *)

module type S = sig
  module T : TERM
  module S : SUBST with type term = T.t

  val occur_check : S.t -> T.t -> scope -> T.t -> scope -> bool
    (** [occur_check subst v s_v t s_t] returns true if and only if
        [subst(v, s_v)] occurs in [subst(t, s_t)], by recursing in [t].

        [v, s_v] shouldn't be bound in [subst].
        
        @raise Invalid_argument if [v] is not a variable. *)


  val unify : ?subst:S.t -> T.t -> scope -> T.t -> scope -> S.t
    (** Unify terms, returns a subst or
        @raise Fail if the terms are not unifiable *)

  val match_ : ?subst:S.t -> T.t -> scope -> T.t -> scope -> S.t
    (** [matching a scope_a b scope_b] returns sigma such that sigma(a) = b, or
        @raise Fail if the terms do not match.
        Only variables from the scope of [a] can  be bound in the subst. *)

  val variant : ?subst:S.t -> T.t -> scope -> T.t -> scope -> S.t
    (** Succeeds iff the first term is a variant of the second *)

  val are_unifiable : T.t -> T.t -> bool
    (** Are the two terms unifiable? (assuming distinct scopes) *)

  val are_variant : T.t -> T.t -> bool
    (** Are the two terms equivalent up to renaming? (assuming distinct scopes) *)
end

(** {2 Functor for unification} *)

module Make(T : TERM)(S : SUBST with type term = T.t) :
  S with module T = T and module S = S

(** {2 Matching modulo AC} *)

module type TERM_AC = sig
  include TERM

  val flatten : t -> t list
    (** Assuming the term is a composite term, flatten its arguments under
        the same head symbol.
        For instance [flatten (Or (a, Or (b, Or (c, d))))] would
        return [a; b; c; d] *)

  type var_generator
    (** Generator of fresh variables *)

  val new_var : var_generator -> t
    (** New variable *)
end

module type MATCH_AC = sig
  module T : TERM_AC
  module S : SUBST with type term = T.t

  val match_ac : ?subst:S.t ->
                 is_ac:(T.t -> bool) ->
                 is_com:(T.t -> bool) ->
                 gen:T.var_generator ->
                 T.t -> scope -> T.t -> scope ->
                 S.t Sequence.t
    (** [matching_ac a b] returns substs such that [subst(a) =_AC b]. It
        is much more costly than [matching]. By default [is_ac] returns
        [true] only on terms whose head symbol has [flag_ac], and [is_com]
        only for [flag_commut].
        A variable generator needs be provided (argument [gen]). *)
end

module MakeAC(T : TERM_AC)(S : SUBST with type term = T.t) :
  MATCH_AC with module T = T and module S = S
