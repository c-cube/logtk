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

(** {1 TPTP Parser} *)

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

  (* TODO: check same type, otherwise make new var *)
  (** Get variable associated with this name *)
  let get_var ?ty name =
    try Hashtbl.find __table name
    with Not_found ->
      let v = T.mk_var ?ty !__count in
      incr __count;
      Hashtbl.add __table name v;
      v

  (** Clear everything in the current context *)
  let clear_ctx () =
    clear_table ();
    ()
%}

%token EOI

%token DOT
/* %token SEMICOLUMN */
%token COMMA
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token FOF
%token CNF
%token TFF
%token INCLUDE

%token NOT

%token COLUMN
%token STAR
%token ARROW

%token AND
%token NOTAND
%token VLINE
%token NOTVLINE
%token IMPLY
%token LEFT_IMPLY
%token EQUIV
%token XOR
%token GENTZEN_ARROW
%token EQUAL
%token NOT_EQUAL

%token FORALL
%token EXISTS
%token LAMBDA
%token AT

%token UNDERSCORE

%token <string> LOWER_WORD
%token <string> UPPER_WORD
%token <string> SINGLE_QUOTED
%token <string> DISTINCT_OBJECT
%token <string> DOLLAR_WORD
%token <string> DOLLAR_DOLLAR_WORD
%token <string> REAL
%token <string> RATIONAL
%token <string> INTEGER

%left VLINE
%left AND
%left AT
%right ARROW
%nonassoc EQUIV
%nonassoc XOR
%nonassoc IMPLY
%nonassoc LEFT_IMPLY
%nonassoc NOTVLINE
%nonassoc NOTAND

%start <Term.t> parse_term
%start <Term.t> parse_formula
%start <Ast_tptp.declaration> parse_declaration
%start <Ast_tptp.declaration list> parse_declarations
%start <Term.t list list> parse_answer_tuple

%%

/* top-level */

parse_term: t=term EOI { clear_ctx (); t }
parse_formula: f=fof_formula EOI { clear_ctx (); f }
parse_declaration: d=declaration EOI { clear_ctx (); d }
parse_declarations: l=declarations EOI { clear_ctx (); l }
parse_answer_tuple: t=answer_tuples EOI { clear_ctx (); t }

/* TPTP grammar */

declarations:
  | l=declaration* { l }

declaration:
  | d=declaration_reset
    { clear_ctx ();  (* cleanup variable table *)
      d }

declaration_reset:
  | FOF LEFT_PAREN name=name COMMA role=role COMMA f=fof_formula info=annotations RIGHT_PAREN DOT
    { Ast_tptp.FOF (name, role, f, info) }
  | TFF LEFT_PAREN name=name COMMA role=role COMMA f=fof_formula info=annotations RIGHT_PAREN DOT
    { Ast_tptp.TFF (name, role, f, info) }
  | TFF LEFT_PAREN name=name COMMA role COMMA tydecl=type_decl info=annotations RIGHT_PAREN DOT
    { let s, ty = tydecl in
      if Type.eq ty Type.tType
        then Ast_tptp.NewType (name, Symbol.name_symbol s)
        else Ast_tptp.TypeDecl (name, s, ty)
    }
  | CNF LEFT_PAREN name=name COMMA role=role COMMA f=cnf_formula info=annotations RIGHT_PAREN DOT
    { Ast_tptp.CNF (name, role, f, info) }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED RIGHT_PAREN DOT
    { Ast_tptp.Include (remove_quotes x) }
  | INCLUDE LEFT_PAREN x=SINGLE_QUOTED COMMA names=name_list RIGHT_PAREN DOT
    { Ast_tptp.IncludeOnly (x, names) }

role: w=LOWER_WORD { Ast_tptp.role_of_string w }

answer_tuples:
  | LEFT_BRACKET l=separated_nonempty_list(VLINE,answer_tuple) RIGHT_BRACKET
    { List.fold_left  (* remove underscores *)
        (fun acc opt -> match opt with | None -> acc | Some tup -> tup :: acc)
        [] l  }

answer_tuple:
  | LEFT_BRACKET l=separated_nonempty_list(COMMA,term) RIGHT_BRACKET { Some l }
  | UNDERSCORE { None }

type_decl:
  | LEFT_PAREN tydecl=type_decl RIGHT_PAREN { tydecl }
  | s=atomic_word COLUMN ty=tff_type { Symbol.mk_symbol s, ty }
  | s=DOLLAR_WORD COLUMN ty=tff_type { Symbol.mk_symbol s, ty }

cnf_formula:
  | LEFT_PAREN c=disjunction RIGHT_PAREN { c }
  | c=disjunction { c }

disjunction:
  | l=separated_nonempty_list(VLINE, literal) { T.mk_or_list l }

literal:
  | f=atomic_formula { f }
  | NOT f=atomic_formula { T.mk_not f }
  | f=fol_infix_unary { f }

fof_formula:
  | fof_logic_formula { $1 }
  | fof_sequent { $1 } 

fof_sequent:
  | l=fof_tuple GENTZEN_ARROW r=fof_tuple
    { T.mk_imply (T.mk_and_list l) (T.mk_or_list r) }
  | LEFT_PAREN seq=fof_sequent RIGHT_PAREN
    { seq }

fof_tuple:
  LEFT_BRACKET l=separated_list(COMMA, fof_logic_formula) RIGHT_BRACKET { l } 

fof_logic_formula:
  | f=fof_unitary_formula { f }
  | l=fof_logic_formula o=binary_connective r=fof_logic_formula
    { o l r }

fof_unitary_formula:
  | fof_quantified_formula { $1 }
  | fof_unary_formula { $1 } 
  | atomic_formula { $1 } 
  | LEFT_PAREN f=fof_logic_formula RIGHT_PAREN { f }

fof_quantified_formula:
  | q=fol_quantifier LEFT_BRACKET vars=variables RIGHT_BRACKET COLUMN f=fof_unitary_formula
    { q vars f }

fof_unary_formula:
  | o=unary_connective f=fof_unitary_formula { o f }
  | f=fol_infix_unary { f }

fol_infix_unary:
  | l=term o=infix_inequality r=term
    { o l r }
  
%inline binary_connective:
  | EQUIV { T.mk_equiv }
  | IMPLY { T.mk_imply }
  | LEFT_IMPLY { fun x y -> T.mk_imply y x }
  | XOR { T.mk_xor }
  | NOTVLINE { fun x y -> T.mk_not (T.mk_or x y) }
  | NOTAND { fun x y -> T.mk_not (T.mk_and x y) }
  | AND { T.mk_and }
  | VLINE { T.mk_or }
%inline fol_quantifier:
  | FORALL { T.mk_forall_var }
  | EXISTS { T.mk_exists_var }
%inline unary_connective:
  | NOT { T.mk_not }
%inline infix_inequality:
  | NOT_EQUAL { fun x y -> T.mk_not (T.mk_eq x y) }

atomic_formula:
  | plain_atomic_formula { $1 }
  | defined_atomic_formula { $1 }
  | system_atomic_formula { $1 }

plain_atomic_formula: plain_term { $1 }

defined_atomic_formula:
  | defined_plain_formula { $1 }
  | defined_infix_formula { $1 }

defined_infix_formula:
  | l=term o=defined_infix_pred r=term  { o l r }

%inline defined_infix_pred:
  | EQUAL { T.mk_eq }

defined_plain_formula:
  | p=defined_prop
    { T.mk_const p }
  | p=defined_pred LEFT_PAREN args=arguments RIGHT_PAREN
    { T.mk_node p args }

/* includes $true and $false */
defined_prop: atomic_defined_word { $1 } 
defined_pred: atomic_defined_word { $1 }

system_atomic_formula: system_term { $1 }
  
/* Terms */

term:
  | function_term { $1 }
  | variable { $1 }
  | l=term AT r=term { T.mk_at l r }
  | LAMBDA LEFT_BRACKET vars=variables RIGHT_BRACKET COLUMN t=term
    { T.mk_lambda_var vars t }
  /* | conditional_term { $1 }  for TFF */
  /* | let_term { $1 } */

function_term:
  | plain_term { $1 }
  | defined_term { $1 }
  | system_term { $1 }

plain_term:
  | s=constant { T.mk_const s }
  | f=functor_ LEFT_PAREN args=arguments RIGHT_PAREN { T.mk_node f args }

constant:
| s=atomic_word { Symbol.mk_symbol s }
| s=atomic_defined_word { s }
functor_: f=atomic_word { Symbol.mk_symbol f }

defined_term:
  | defined_atom { T.mk_const $1 }
  | defined_atomic_term { $1 }

defined_atom:
  | n=INTEGER { Symbol.mk_int (int_of_string n) }
  | n=RATIONAL { Symbol.mk_num (Num.num_of_string n) }
  | n=REAL { Symbol.mk_real (float_of_string n) }
  | s=DISTINCT_OBJECT { Symbol.mk_distinct s }

defined_atomic_term:
  | defined_plain_term { $1 }
  /* | defined_infix_term { $1 } */

defined_plain_term:
  | s=defined_constant { T.mk_const s }
  | f=defined_functor LEFT_PAREN args=arguments RIGHT_PAREN { T.mk_node f args }

defined_constant: defined_functor { $1 }
defined_functor: s=atomic_defined_word { s }

system_term:
  | c=system_constant { T.mk_const c }
  | f=system_functor LEFT_PAREN args=arguments RIGHT_PAREN { T.mk_node f args }

system_constant: system_functor { $1 }
system_functor: s=atomic_system_word { s }

tff_type:
  | ty=tff_atom_type { ty }
  | l=tff_atom_type ARROW r=tff_atom_type
    { Type.mk_fun r [l] }
  | LEFT_PAREN args=tff_ty_args RIGHT_PAREN ARROW r=tff_atom_type
    { Type.mk_fun r args }

tff_atom_type:
  | w=UPPER_WORD { Type.var w }
  | w=type_const { Type.const w }
  | LEFT_PAREN ty=tff_type RIGHT_PAREN { ty }

tff_ty_args:
  | ty=tff_atom_type { [ty] }
  | hd=tff_atom_type STAR tl=tff_ty_args { hd :: tl }
  
type_const:
  | w=LOWER_WORD { w }
  | w=DOLLAR_WORD { w }

arguments: separated_nonempty_list(COMMA, term) { $1 }

variables:
  | l=separated_nonempty_list(COMMA, variable) { l }

variable:
  | x=UPPER_WORD { get_var x }
  | x=UPPER_WORD COLUMN ty=tff_type { get_var ~ty x }

atomic_word:
  | s=SINGLE_QUOTED { remove_quotes s }
  | s=LOWER_WORD { s }

atomic_defined_word:
  | w=DOLLAR_WORD { Symbol.mk_symbol w }

atomic_system_word:
  | w=DOLLAR_DOLLAR_WORD { Symbol.mk_symbol w }

name_list:
  l=separated_list(COMMA, name) { l }

name:
  | w=atomic_word { Ast_tptp.NameString w }
  | i=INTEGER { Ast_tptp.NameInt (int_of_string i) }

annotations:
  | { [] }
  | COMMA l=separated_list(COMMA, general_term) { l }

general_term:
  | general_data { $1 }
  | l=general_data COLUMN r=general_term { Ast_tptp.GColumn (l,r) }
  | general_list { $1 }

general_data:
  | w=atomic_word { Ast_tptp.GString w }
  | general_function { $1 }
  | INTEGER { Ast_tptp.GInt (int_of_string $1) }
  | v=UPPER_WORD { Ast_tptp.GVar v }
  | w=DISTINCT_OBJECT { Ast_tptp.GQuote w }

general_function:
  | f=atomic_word LEFT_PAREN l=separated_nonempty_list(COMMA, general_term) RIGHT_PAREN
    { Ast_tptp.GNode (f, l) }

general_list:
  | LEFT_BRACKET l=separated_list(COMMA, general_term) RIGHT_BRACKET
    { Ast_tptp.GList l }

%%
