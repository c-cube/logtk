
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

(** {1 Utils linked to monads} *)

(** {2 Signature of a monad} *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t
    (** Functorial map *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(** {2 Option Monad} *)

module Opt = struct
  type 'a t = 'a option

  let maybe opt x = match opt with
    | Some y -> y
    | None -> x

  let (>>=) opt f = match opt with
  | None -> None
  | Some x -> f x

  let return x = Some x

  let map x f = match x with
    | None -> None
    | Some x' -> Some (f x')

  let get = function
  | Some x -> x
  | None -> invalid_arg "Opt.get"

  let is_some = function | Some _ -> true | None -> false
  let is_none = function | None -> true | Some _ -> false
end

(** {2 Error monad} *)

module Err = struct
  type 'a err =
    | Ok of 'a
    | Error of string

  type 'a t = 'a err

  let return x = Ok x

  let map x f = match x with
    | Ok x -> Ok (f x)
    | Error s -> Error s

  let guard ?(print=Printexc.to_string) f =
    try
      Ok (f ())
    with e ->
      Error (print e)

  let fail s = Error s
  let fail_exn ex = Error (Printexc.to_string ex)

  let (>>=) x f = match x with
    | Ok x' -> f x'
    | Error s -> Error s

  let to_opt = function
    | Ok x -> Some x
    | Error _ -> None
end

(** {2 List monad} *)

module L = struct
  type 'a t = 'a list

  let return x = [x]

  let map x f = List.map f x

  let (>>=) l f =
    let rec expand l = match l with
    | [] -> []
    | x::l' ->
      List.rev_append (f x) (expand l')
    in
    expand l
end

(** {2 Monadic traversal}
This functor allows to build fold and map functions with a monadic interface.
*)

module type TRAVERSE = sig
  module M : S
    (** monad used for traversal *)

  val fold : 'a Sequence.t -> 'b M.t -> ('b -> 'a -> 'b M.t) -> 'b M.t
  val fold_l : 'a list -> 'b M.t -> ('b -> 'a -> 'b M.t) -> 'b M.t

  val map_l : 'a list -> ('a -> 'b M.t) -> 'b list M.t

  val seq : 'a M.t list -> 'a list M.t
end

module Traverse(M : S) = struct
  module M = M

  let fold seq acc f =
    let open M in
    Sequence.fold
      (fun acc x -> acc >>= fun acc -> f acc x)
      acc seq

  let fold_l l acc f =
    let open M in
    List.fold_left
      (fun acc x -> acc >>= fun acc -> f acc x)
      acc l

  let map_l l f =
    let open M in
    let rec map l = match l with
      | [] -> M.return []
      | x::l' ->
        f x >>= fun x' ->
        map l' >>= fun l' ->
        M.return (x' :: l')
    in
    map l

  open M

  let rec seq l = match l with
    | [] -> M.return []
    | x::l' ->
      x >>= fun x' ->
      seq l' >>= fun l'' ->
      M.return (x' :: l'')
end

module TraverseOpt = Traverse(Opt)
module TraverseErr = Traverse(Err)