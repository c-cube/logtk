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

module T = Term

let files = ref []
let from_stdin = ref false
let ord = ref "lpo"
let timeout = ref 0.

let options =
  [ "-time", Arg.Set_float timeout, "timeout (in s)"
  ; "-stdin", Arg.Set from_stdin, "read from stdin"
  ; "-ord", Arg.Set_string ord, "ord to use (lpo | kbo)"
  ; "-level", Arg.Set_int Util.level, "verbosity level (default: 1)"
  ]

let parse_args () =
  let anon_fun f =
    files := !files @ [f] in
  let help_msg = "Orient: finds orderings for rewriting rules" in
  Arg.parse options anon_fun help_msg;
  ()

let pp_rule fmt (l,r) =
  Format.fprintf fmt "@[<h>%a -> %a@]" T.fmt l T.fmt r

let parse_file ic =
  let lexbuf = Lexing.from_channel ic in
  try
    let rules = Parse_rewrite.parse_rules Lex_rewrite.token lexbuf in
    rules
  with e ->
    Printf.eprintf "error while parsing (at %s): %s\n"
      (Util.pp_pos (Lexing.lexeme_start_p lexbuf))
      (Printexc.get_backtrace ());
    []

(* process rules: try to orient the rules *)
let process_rules rules =
  (if !Util.level > 0 then begin
    Printf.printf "%% rules:\n";
    List.iter (Format.printf "%%   %a@." pp_rule) rules;
    end);
  let constraints = match !ord with
  | "lpo" -> Lpo.orient_list rules
  | "kbo" -> Kbo.orient_list rules
  | _ -> failwith ("unknown ordering: " ^ !ord)
  in
  (if !Util.level > 0 then begin
    Util.printf "%% constraints: \n";
    List.iter
      (fun c -> Util.printf "%%   %a\n" Solve.Constraint.pp c)
      constraints;
    end);
  let solutions = Solve.solve_multiple constraints in
  Stream.iter
    (fun solution ->
      Util.printf "%% solution:\n";
      Util.printf "    %a" Solve.Solution.pp solution)
    solutions;
  Util.printf "%% no other solution\n";
  ()

(* process file by parsing it, and orienting its rules *)
let process_file filename =
  Printf.printf "%% process file %s\n" filename;
  let ic = open_in filename in
  let rules = parse_file ic in
  process_rules rules;
  close_in ic;
  ()

(* process stdin *)
let process_stdin () =
  Printf.printf "%% process stdin\n";
  flush stdout;
  let rules = parse_file stdin in
  process_rules rules;
  ()

let main () =
  Printf.printf "%% start orient\n";
  parse_args ();
  if !from_stdin
    then process_stdin ()
    else List.iter process_file !files;
  ()

let _ =
  main ()
