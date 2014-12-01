
(*
Copyright (c) 2013-2014, Simon Cruanes
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

module type S = sig
  type term

  type eval_fun = tyargs:LogtkType.t list -> LogtkSymbol.t -> term list -> term option
    (** An  evaluation function takes a symbol application, and a list of
        arguments, and can decide to return a new term based on that.
        If it returns None, it means that the term is already evaluated. *)

  type t
    (** An evaluator. It maps symbols to evaluators *)

  val create : unit -> t
    (** New evaluator *)

  val copy : t -> t
    (** Copy the evaluator *)

  val register : t -> LogtkSymbol.t -> eval_fun -> unit
    (** Add an evaluation function to the evaluator. *)

  val register_list : t -> (LogtkSymbol.t * eval_fun) list -> unit

  val interpreted : t -> LogtkSymbol.t -> bool
    (** Is there a registered evaluation function for this symbol? *)

  val eval_head : t -> term -> term
    (** Evaluate the term's root, but not subterms *)

  val eval : t -> term -> term
    (** Recursively evaluate the term *)
end
