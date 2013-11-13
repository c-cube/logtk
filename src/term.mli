
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

(** {1 Common Term representation}

We use Higher-Order terms to represent terms and types, based on the
lambda-pi calculus. See "./data/slides_dedukti.pdf" (without the
rewrite system) for the typing rules.
*)

type t = private {
  term : term_cell;             (** the term itself *)
  ty : Type.t option lazy;      (** type *)
  mutable tsize : int;          (** size (number of subterms) *)
  mutable flags : int;          (** boolean flags about the term *)
  mutable tag : int;            (** hashconsing tag *)
}
(** content of the term *)
and term_cell = private
  | TType                       (** the "type" of types *)
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Const of Symbol.t           (** Constant *)
  | At of t * t list            (** function application. *)
  | Lambda of t * t             (** lambda abstraction over one variable. *)
  | Pi of t * t                 (** quantification on types *)

type term = t

(** {2 Basics} *)

val eq : t -> t -> bool             (** standard equality on terms *)
val compare : t -> t -> int         (** a simple order on terms *)
val hash : t -> int

(** {2 Boolean flags} *)

val flag_db_closed : int
val flag_normal_form : int
val flag_ground : int
val flag_db_closed_computed : int

val set_flag : int -> t -> bool -> unit
  (** set or reset the given flag of the term to bool *)

val get_flag : int -> t -> bool
  (** read the flag *)

val new_flag : unit -> int
  (** New flag, different from all other flags *)

(** {2 Global terms table (hashconsing)} *)

module H : Hashcons.S with type elt = t

(** {2 Constructors}

Term constructors perform hashconsing and always reduce terms to
their normal form.
*)

val tTtype : t
  (** Type *)

val var : ty:t -> int -> t
  (** Create a variable. The index must be >= 0 *)

val bound_var : int -> t
  (** De Bruijn index, must be >= 0. No type is needed
      since the type appears at binder position. *)

exception DependentType

val pi : varty:t -> t -> t
  (** [pi ~varty t] creates the pi type with type [pi x:varty. t]
      @raise DependentType if the resulting type is dependent *)

exception SystemF

val lambda : varty:t -> t -> t
  (** [lambda ~varty t'] creates the lambda function with
      type [varty -> t'.ty]
      @raise SystemF if the result is a type *)

val arrow : t -> t -> t
  (** [arrow] is a restriction of [pi] for arrow types *)

val const : ty:Type.t -> Symbol.t -> t
  (** Constant, with a given type *)

exception TypeError of string
  (** Type error, raised when types do not match *)

val at : t -> t list -> t
  (** Apply a term to other terms. Type can be deduced from arguments.
      @raise Failure if types do not match. *)

(** {2 Variable environment}

Environment for De Bruijn variables, used for beta reduction
*)

module Env : sig
  type t
  
  val empty : t
    (** Empty env *)

  val depth : t -> int
    (** Depth of the environment (number of binders met) *)

  val push : t -> term -> t
    (** Enter a scope, binding the variable to the given term *)

  val push_none : t -> t
    (** Enter a scope without binding the variable *)

  val pop : t -> t
    (** Exit the scope.
        @raise Invalid_argument if the scope is empty *)

  val find : t -> int -> term option
    (** Find the value the [n]-th variable is bound to *)
end

(** {2 Properties} *)

val is_var : t -> bool
val is_bound_var : t -> bool
val is_const : t -> bool
val is_at : t -> bool
val is_lambda : t -> bool
val is_pi : t -> bool
val is_tType : t -> bool

val is_type : t -> bool   (** is the term a type (ie : tType) *)

(** {2 IO} *)

val print_all_types : bool ref

val pp_debug : Buffer.t -> t -> unit
val pp_tstp : Buffer.t -> t -> unit

val pp : Buffer.t -> t -> unit
val set_default_pp : (Buffer.t -> t -> unit) -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val bij : t Bij.t

(** {2 Positions} *)

(* TODO 

val at_pos : t -> Position.t -> t 
  (** retrieve subterm at pos, or raise Invalid_argument*)

val replace_pos : t -> Position.t -> t -> t
  (** replace t|_p by the second term *)

val replace : t -> old:t -> by:t -> t
  (** [replace t ~old ~by] syntactically replaces all occurrences of [old]
      in [t] by the term [by]. *)

val at_cpos : t -> int -> t
  (** retrieve subterm at the compact pos, or raise Invalid_argument*)

val max_cpos : t -> int
  (** maximum compact position in the term *)

val depth : t -> int          (** depth of the term *)
val head : t -> Symbol.t      (** head symbol (or Invalid_argument) *)
val size : t -> int

val arg_ty : t -> t
  (** [arg_ty t] returns the argument type that [t] expects, assuming
      [t] is a Lambda or a Pi.
      @raise Invalid_argument if [t] is not a Lambda or a Pi. *)

(** {2 AC} *)

val symbols : t Sequence.t -> Symbol.Set.t
  (** Symbols occurring in the terms *)

val contains_symbol : Symbol.t -> t -> bool
  (** Checks whether the symbol occurs in the term *)

val flatten_ac : Symbol.t -> t list -> t list
  (** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
      elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
      with f="+", this will return [1;2;3;4;5], perhaps in a different order *)
*)
