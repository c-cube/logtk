
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

(** Test typing *)

open Logtk
open QCheck

module T = Term

let check_infer_all_symbs =
  let gen = Arbitrary.(list T.arbitrary) in
  let name = "type_infer_all_symbols" in
  let pp = PP.(list T.to_string) in
  (* check that after type inference, all symbols apppear in the signature *)
  let prop terms =
    let ctx = TypeInference.Ctx.create () in
    List.iter (fun t -> ignore (TypeInference.infer ctx t)) terms;
    let signature = TypeInference.Ctx.to_signature ctx in
    let symbols = T.symbols (Sequence.of_list terms) in
    Symbol.SSet.for_all (Signature.mem signature) symbols
  in
  mk_test ~pp ~name gen prop

let check_infer_types_closed =
  let gen = Arbitrary.(list T.arbitrary) in
  let name = "type_infer_types_closed" in
  let pp = PP.(list T.to_string) in
  (* check that after type inference, all types are closed *)
  let prop terms =
    let ctx = TypeInference.Ctx.create () in
    List.iter (fun t -> ignore (TypeInference.infer ctx t)) terms;
    let signature = TypeInference.Ctx.to_signature ctx in
    Symbol.SMap.for_all
      (fun _ ty -> Type.is_closed ty)
      signature
  in
  mk_test ~pp ~name gen prop

let props =
  [ check_infer_all_symbs
  ; check_infer_types_closed
  ]