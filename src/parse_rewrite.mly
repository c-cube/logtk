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

(** {1 Input parser for rewriting systems} *)

%{
  module T = Term

  let remove_quotes s =
    assert (s.[0] = '\'' && s.[String.length s - 1] = '\'');
    String.sub s 1 (String.length s - 2)

  let __table = Hashtbl.create 5
  let __count = ref 0

  let clear_table () =
    Hashtbl.clear __table;
    __count := 0

  (** Get variable associated with this name *)
  let get_var ?ty name =
    try Hashtbl.find __table name
    with Not_found ->
      let v = T.mk_var ?ty !__count in
      incr __count;
      Hashtbl.add __table name v;
      v

  (** Underscore : new variable *)
  let get_underscore ?ty () =
    let v = T.mk_var ?ty !__count in
    incr __count;
    v

  (** Clear everything in the current context *)
  let clear_ctx () =
    clear_table ();
    ()
%}

%token EOI

%token DOT
%token LEFT_PAREN
%token RIGHT_PAREN

%token COLUMN
%token GENTZEN_ARROW
%token EQUAL
%token UNDERSCORE

%token COMMA
%token LAMBDA

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT

%nonassoc low
%nonassoc high

%nonassoc EQUAL

%start <(Term.t * Term.t) list> parse_rules
%start <Term.t * Term.t> parse_rule
%start <Term.t> parse_term

%%

/* start rules */

parse_term: t=term EOI { clear_ctx (); t }
parse_rule: r=rule_reset EOI { r }
parse_rules: l=rule_reset* EOI { l }

rule_reset: l=rule { clear_ctx (); l }

/* grammar rules */

rule:
  l=term GENTZEN_ARROW r=term DOT { l, r }

term:
  | c=const LEFT_PAREN l=terms RIGHT_PAREN { T.mk_node c l }
  | c=const { T.mk_const c }
  | v=variable { v }
  | LEFT_PAREN t=term RIGHT_PAREN { t }
  | l=term EQUAL r=term %prec low { T.mk_eq l r }
  | LAMBDA v=vars DOT t=term %prec high { T.mk_lambda_var v t }

terms:
  l=separated_nonempty_list(COMMA, term) { l }

vars:
  l=separated_nonempty_list(COMMA, variable) { l }

%inline variable:
  | UNDERSCORE { get_underscore () }
  | v=UPPER_WORD { get_var v }
  | v=UPPER_WORD COLUMN ty=term { get_var ~ty v }

%inline const:
  | w=LOWER_WORD { Symbol.mk_symbol w }
  | w=SINGLE_QUOTED { Symbol.mk_symbol (remove_quotes w) }
  | w=DISTINCT_OBJECT { Symbol.mk_distinct w }
