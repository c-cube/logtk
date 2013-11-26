
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
    (** Unify T.ts, returns a subst or
        @raise Fail if the T.ts are not unifiable *)

  val match_ : ?subst:S.t -> T.t -> scope -> T.t -> scope -> S.t
    (** [matching a scope_a b scope_b] returns sigma such that sigma(a) = b, or
        @raise Fail if the T.ts do not match.
        Only variables from the scope of [a] can  be bound in the subst. *)

  val variant : ?subst:S.t -> T.t -> scope -> T.t -> scope -> S.t
    (** Succeeds iff the first T.t is a variant of the second *)

  val are_unifiable : T.t -> T.t -> bool
    (** Are the two terms unifiable (assuming distinct scopes)? *)

  val are_variant : T.t -> T.t -> bool
    (** Are the two terms equivalent up to renaming (assuming distinct scopes)? *)
end

(** {2 Functor for unification} *)

let prof_unification = Util.mk_profiler "unif.unify"
let prof_matching = Util.mk_profiler "unif.match"
let prof_variant = Util.mk_profiler "unif.variant"
let prof_ac_matching = Util.mk_profiler "unif.ac_matching"

module Make(T : TERM)(S : SUBST with type term = T.t) = struct
  let rec occur_check subst v sc_v t sc_t =
    (sc_v = sc_t && T.eq v t)
    ||
    if T.is_var t
      then try(* deref [t] *)
        let t', sc_t' = S.lookup subst t sc_t in
        occur_check subst v sc_v t' sc_t'
      with Not_found -> false
      else (* check subterms of [t] *)
        _oc_list subst v sc_v (T.subterms t) sc_t
  and _oc_list subst v sc_v l sc_l = match l with
    | [] -> false
    | t::l' ->
      occur_check subst v sc_v t sc_l ||
      _oc_list subst v sc_v l' sc_l

  (* syntactic unification *)
  let unify ?(subst=S.empty) a s_a b s_b =
    let rec unif subst a s_a b s_b =
      let a, s_a = S.get_var subst a s_a in
      let b, s_b = S.get_var subst b s_b in
      let subst = S.pre_unify subst a s_a b s_b in
      match T.can_unify a s_a b s_b with
      | Impossible -> raise Fail
      | Trivial -> subst   (* success *)
      | Possible ->
        match T.is_var a, T.is_var b with
        | true, true ->
          if s_a = s_b && T.eq a b
            then subst   (* trivial! *)
            else S.bind subst a s_a b s_b
        | true, false ->
          if occur_check subst a s_a b s_b
            then raise Fail
            else S.bind subst a s_a b s_b
        | false, true ->
          if occur_check subst b s_b a s_a
            then raise Fail
            else S.bind subst b s_b a s_a
        | false, false ->
          if T.eq_head a b
            then unif_list subst (T.subterms a) s_a (T.subterms b) s_b
            else raise Fail (* not same head *)
    and unif_list subst la s_a lb s_b = match la, lb with
      | [], [] -> subst
      | [], _
      | _, [] -> raise Fail
      | a::la', b::lb' ->
        let subst = unif subst a s_a b s_b in
        unif_list subst la' s_a lb' s_b
    in
    (* perform unification, with profiling *)
    Util.enter_prof prof_unification;
    try
      let subst = unif subst a s_a b s_b in
      Util.exit_prof prof_unification;
      subst
    with Fail as e ->
      Util.exit_prof prof_unification;
      raise e

  (* syntactic matching *)
  let match_ ?(subst=S.empty) a s_a b s_b =
    let rec unif subst a s_a b s_b =
      let a, s_a = S.get_var subst a s_a in
      let b, s_b = S.get_var subst b s_b in
      let subst = S.pre_unify subst a s_a b s_b in
      match T.can_unify a s_a b s_b with
      | Impossible -> raise Fail
      | Trivial -> subst   (* success *)
      | Possible ->
        match T.is_var a, T.is_var b with
        | true, true ->
          if s_a = s_b
            then if T.eq a b
              then subst  (* trivial *)
              else raise Fail  (* scope of [b]: cannot bind variables of [b] *)
            else S.bind subst a s_a b s_b
        | true, false ->
          if occur_check subst a s_a b s_b
            then raise Fail
            else S.bind subst a s_a b s_b
        | false, true -> raise Fail
        | false, false ->
          if T.eq_head a b
            then unif_list subst (T.subterms a) s_a (T.subterms b) s_b
            else raise Fail (* not same head *)
    and unif_list subst la s_a lb s_b = match la, lb with
      | [], [] -> subst
      | [], _
      | _, [] -> raise Fail
      | a::la', b::lb' ->
        let subst = unif subst a s_a b s_b in
        unif_list subst la' s_a lb' s_b
    in
    (* perform unification, with profiling *)
    Util.enter_prof prof_matching;
    try
      let subst = unif subst a s_a b s_b in
      Util.exit_prof prof_matching;
      subst
    with Fail as e ->
      Util.exit_prof prof_matching;
      raise e

  (* syntactic alpha-equivalence checking*)
  let variant ?(subst=S.empty) a s_a b s_b =
    let rec unif subst a s_a b s_b =
      let a, s_a = S.get_var subst a s_a in
      let b, s_b = S.get_var subst b s_b in
      match T.can_unify a s_a b s_b with
      | Impossible -> raise Fail
      | Trivial -> subst   (* success *)
      | Possible ->
        match T.is_var a, T.is_var b with
        | true, true ->
          if s_a = s_b
            then if T.eq a b
              then subst  (* trivial *)
              else raise Fail  (* scope of [b]: cannot bind variables of [b] *)
            else S.bind subst a s_a b s_b
        | true, false
        | false, true -> raise Fail
        | false, false ->
          if T.eq_head a b
            then unif_list subst (T.subterms a) s_a (T.subterms b) s_b
            else raise Fail (* not same head *)
    and unif_list subst la s_a lb s_b = match la, lb with
      | [], [] -> subst
      | [], _
      | _, [] -> raise Fail
      | a::la', b::lb' ->
        let subst = unif subst a s_a b s_b in
        unif_list subst la' s_a lb' s_b
    in
    (* perform unification, with profiling *)
    Util.enter_prof prof_variant;
    try
      let subst = unif subst a s_a b s_b in
      Util.exit_prof prof_variant;
      subst
    with Fail as e ->
      Util.exit_prof prof_variant;
      raise e

  let are_unifiable t1 t2 =
    try
      let _ = unify t1 0 t2 1 in
      true
    with Fail ->
      false

  let are_variant t1 t2 =
    try
      let _ = variant t1 0 t2 1 in
      true
    with Fail ->
      false
end

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
        A variable generator needs be provided. *)
end

module MakeAC(T : TERM_AC)(S : SUBST with type term = T.t) = struct
  module T = T
  module S = S

  let occur_check = 
    let module Unif = Make(T)(S) in
    M.occur_check

  let match_ac ?(subst=S.empty) ~is_ac ~is_com ~gen a s_a b s_b =
    (* recursive matching. [k] is called with solutions *)
    let rec unif subst s s_s t s_t k =
      let s, s_s = S.get_var subst s s_s in
      match T.can_match s t with
      | Impossible -> ()
      | Trivial -> k subst
      | Possible ->
        match T.is_var s, T.is_var t with
        | true, true ->
          if s_s = s_t
            then if T.eq s t
              then k subst  (* trivial *)
              else ()       (* failure *)
            else k (S.bind subst s s_s t s_t)
        | false, true -> ()
        | true, false ->
          if occur_check subst s s_s t s_t
            then ()
            else k (S.bind subst s s_s t s_t)
        | false, false ->
          if T.eq_head s t
            then if is_ac s  (* match multisets of subterms *)
              then unif_ac subst (T.flatten s) s_s (T.flatten t) s_t k
            else if is_com s   (* match modulo C *)
              then match T.subterms s, T.subterms t with
              | [s1; s2], [t1; t2] ->
                unif_com subst s1 s2 s_s t1 t2 s_t k
              | _ -> ()
            else (* match subterms *)
              unif_list subst (T.subterms s) s_s (T.subterms t) s_t k
          else ()  (* not same head, fail *)
  (* unify pair of lists of terms, with given continuation. *)
  and unif_list subst l1 sc_1 l2 sc_2 k =
    match l1, l2 with
    | [], [] -> k subst  (* success *)
    | [], _ | _, [] -> ()
    | t1::l1', t2::l2' ->
      unif subst t1 sc_1 t2 sc_2
        (fun subst -> unif_list subst l1' sc_1 l2' sc_2 k)
  (* unify terms under a commutative symbol (try both sides) *)
  and unif_com subst x1 y1 s_1 x2 y2 s_2 k =
    unif subst x1 s_1 x2 s_2 (fun subst -> unif subst y1 s_1 y2 s_2 k);
    unif subst x1 s_1 y2 s_2 (fun subst -> unif subst y1 s_1 x2 s_2 k);
    ()
  (* try all permutations of [left@right] against [l1]. [left,right] is a
     zipper over terms to be matched against [l1]. *)
  and unif_ac subst f l1 sc_1 left right sc_2 k =
    match l1, left, right with
    | [], [], [] -> k subst  (* success *)
    | _ when List.length l1 > List.length left + List.length right ->
      ()  (* failure, too many patterns *)
    | x1::l1', left, x2::right' ->
      (* try one-to-one of x1 against x2 *)
      unif subst x1 sc_1 x2 sc_2
        (fun subst ->
          (* continue without x1 and x2 *)
          unif_ac ~tyargs subst f l1' sc_1 [] (left @ right') sc_2 k);
      (* try x1 against right', keeping x2 on the side *)
      unif_ac ~tyargs subst f l1 sc_1 (x2::left) right' sc_2 k;
      (* try to bind x1 to [x2+z] where [z] is fresh,
         if len(l1) < len(left+right) *)
      if T.is_var x1 && List.length l1 < List.length left + List.length right then
        let z = fresh_var ~ty:(T.ty x1) in
        (* offset trick: we need [z] in both contexts sc_1 and sc_2, so we
           bind it so that (z,sc_2) -> (z,sc_1), and use (z,sc_1) to continue
           the matching *)
        let subst' = S.bind subst z sc_2 z sc_1 in
        let x2' = T.mk_node ~tyargs f [x2; z] in
        let subst' = S.bind subst' x1 sc_1 x2' sc_2 in
        unif_ac ~tyargs subst' f (z::l1') sc_1 left right' sc_2 k
    | x1::l1', left, [] -> ()
    | [], _, _ -> ()  (* failure, some terms are not matched *)
  (* eliminate common occurrences of terms in [l1] and [l2] *)
  and eliminate_common l1 l2 = l1, l2 (* TODO *)
  in
  (* sequence of solutions. Substsitutions are restricted to the variables
     of [a]. *)
  let seq k =
    Util.enter_prof prof_ac_matching;
    try
      unif subst a sc_a b sc_b k;
      Util.exit_prof prof_ac_matching
    with e ->
      Util.exit_prof prof_ac_matching;
      raise e
  in
  Sequence.from_iter seq
end
