
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

(** {1 Parser for HO} *)

{
  open Parse_ho
}

let printable_char = [^ '\n']
let comment_line = ['%' '#'] printable_char*

let numeric = ['0' - '9']
let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*
let interrogation_word = '?' alpha_numeric*

rule token = parse
  | eof { EOI }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | comment_line { token lexbuf }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '{' { LEFT_BRACE }
  | '}' { RIGHT_BRACE }
  | '|' { PIPE }
  | ',' { COMMA }
  | '.' { DOT }
  | '_' { WILDCARD }
  | "->" { RIGHT_ARROW }
  | "<-" { LEFT_ARROW }
  | "<=>" { EQUIV }
  | "=>" { IMPLY }
  | "<~>" { XOR }
  | "&&" { AND }
  | "||" { OR }
  | "=" { EQ }
  | "!=" { NEQ }
  | ":" { COLUMN }
  | "val" { VAL }
  | lower_word { LOWER_WORD(Lexing.lexeme lexbuf) }
  | upper_word { UPPER_WORD(Lexing.lexeme lexbuf) }
  | interrogation_word { INTERROGATION_WORD(Lexing.lexeme lexbuf) }
  | _ as c { failwith (Printf.sprintf "lexer fails on char '%c'" c) }


{
  let decl_of_string s =
    Parse_ho.parse_decl token (Lexing.from_string s)

  let decls_of_string s =
    Parse_ho.parse_decls token (Lexing.from_string s)
}