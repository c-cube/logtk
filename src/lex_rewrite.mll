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

(** {1 Input lexer for rewriting systems} *)

{
  open Parse_rewrite
}

let printable_char = [^ '\n']
let not_star_slash = ([^ '*']* '*'+ [^ '/' '*'])* [^ '*']*
let comment_line = ['%' '#'] printable_char*
let comment_block = '/' '*' not_star_slash '*' '/'
let comment = comment_line | comment_block

let sq_char = [^ '\\' '''] | "\\\\" | "\\'"
let do_char = [^ '"' '\\' ] |  "\\\\" | "\\\""
let single_quoted = ''' sq_char+ '''
let distinct_object = '"' do_char* '"'

let zero_numeric = '0'
let non_zero_numeric = ['1' - '9']
let numeric = ['0' - '9']
let sign = ['+' '-']

let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'

let upper_word = upper_alpha alpha_numeric*
let lower_word = lower_alpha alpha_numeric*

rule token = parse
  | comment { Lexing.new_line lexbuf; token lexbuf }
  | comment_block { token lexbuf }  (* TODO: count new lines in lexeme lexbuf *)
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | eof { EOI }
  | '^' { LAMBDA }
  (* | ';' { SEMICOLUMN } *)
  | ':' { COLUMN }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | "-->" { GENTZEN_ARROW }
  | '=' { EQUAL }
  | ',' { COMMA }
  | '.' { DOT }
  | '_' { UNDERSCORE }
  | lower_word { LOWER_WORD(Lexing.lexeme lexbuf) }
  | upper_word { UPPER_WORD(Lexing.lexeme lexbuf) }
  | single_quoted { SINGLE_QUOTED(Lexing.lexeme lexbuf) }
  | distinct_object { DISTINCT_OBJECT(Lexing.lexeme lexbuf) }
  | _ as c { failwith (Printf.sprintf "lexer fails on char %c\n" c) }

{

}
