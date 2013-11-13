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

module FO : S with type term = FOTerm.t

module Untyped : S with type term = Untyped.FO.t

module Make( T : TERM ) : S with type term = T.t
