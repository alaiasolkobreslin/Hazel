%{
    open Lexing
    open Ast

    let get_pos_string p =
      string_of_int ( p.pos_lnum ) ^ ":"
      ^ string_of_int ( p.pos_cnum - p.pos_bol + 1 )

    let get_return_value p str =
      (get_pos_string p) ^ " " ^ str ^ "\n"
%}

%token <string> INT
%token TIMES PLUS MINUS DIVIDE MOD HMUL

%token <string> ID
%token <string * Lexing.position> STRING
%token <string * Lexing.position> CHAR

%token TBOOL TINT TSTRING TCHAR TUNIT

%token UNIT

%token TRUE FALSE
%token AND OR EQ NEQ PEQ PNEQ LT LEQ GT GEQ NOT

%token LEFT_BRACE RIGHT_BRACE LEFT_BRACK RIGHT_BRACK LEFT_PAREN RIGHT_PAREN BEGIN END

%token COLON
%token COMMA
%token EOF
%token SEMICOLON
%token CONS
%token OPEN

%token LET REC MUTUAL_REC IN
%token FUN ARROW CONSTRAINT

%token REFERENCE
%token DEREFERENCE
%token ASSIGNREF

%token IF THEN ELSE
%token MATCH WITH WHEN

%token <string> CONSTRUCTOR
%token TYPE OF
%token VERTBAR

%token ANY

%start lexer
%type <string option> lexer
%%

lexer:
  | tok = token { Some tok }
  | EOF { None }
;

token:
  | i = INT
    { get_return_value $startpos ("integer " ^ i) }
  | TIMES
    { get_return_value $startpos "*" }
  | PLUS
    { get_return_value $startpos "+" }
  | MINUS
    { get_return_value $startpos "-" }    
  | DIVIDE
    { get_return_value $startpos "/" }
  | MOD
    { get_return_value $startpos "mod" }
  | HMUL
    { get_return_value $startpos ">>*" }
  | i = ID
    { get_return_value $startpos ("id " ^ i) }
  | STRING
    { let s,p = $1 in
      get_return_value p ("string " ^ (Util.escape_string s))}
  | CHAR
    { let s,p = $1 in
      get_return_value p ("character " ^ (Util.escape_string s))}
  | TRUE
    { get_return_value $startpos "true" }
  | FALSE
    { get_return_value $startpos "false" }
  | AND
    { get_return_value $startpos "&&" }
  | OR
    { get_return_value $startpos "||" }
  | EQ
    { get_return_value $startpos "=" }
  | NEQ
    { get_return_value $startpos "<>" }
  | PEQ
    { get_return_value $startpos "==" }
  | PNEQ
    { get_return_value $startpos "!=" }    
  | LT
    { get_return_value $startpos "<" }
  | LEQ
    { get_return_value $startpos "<=" }
  | GT
    { get_return_value $startpos ">" }
  | GEQ
    { get_return_value $startpos ">=" }
  | NOT
    { get_return_value $startpos "not" }
  | LEFT_BRACE
    { get_return_value $startpos "{" }
  | RIGHT_BRACE
    { get_return_value $startpos "}" }
  | LEFT_BRACK
    { get_return_value $startpos "[" }
  | RIGHT_BRACK
    { get_return_value $startpos "]" }
  | LEFT_PAREN
    { get_return_value $startpos "(" }
  | RIGHT_PAREN
    { get_return_value $startpos ")" }
  | BEGIN
    { get_return_value $startpos "begin" }
  | END
    { get_return_value $startpos "end" }
  | COLON
    { get_return_value $startpos ":" }
  | COMMA
    { get_return_value $startpos "," }
  | SEMICOLON
    { get_return_value $startpos ":" }
  | CONS
    { get_return_value $startpos "::" }
  | OPEN
    { get_return_value $startpos "open" }
  | LET
    { get_return_value $startpos "let" }
  | REC
    { get_return_value $startpos "rec" }
  | MUTUAL_REC
    { get_return_value $startpos "and" }
  | IN
    { get_return_value $startpos "in" }
  | FUN
    { get_return_value $startpos "lambda" }
  | ARROW
    { get_return_value $startpos "->" }
  | CONSTRAINT
    { get_return_value $startpos "constraint" }
  | REFERENCE
    { get_return_value $startpos "ref" }
  | DEREFERENCE
    { get_return_value $startpos "!" }
  | ASSIGNREF
    { get_return_value $startpos ":=" }
  | IF
    { get_return_value $startpos "if" }
  | THEN
    { get_return_value $startpos "then" }
  | ELSE
    { get_return_value $startpos "else" }
  | MATCH
    { get_return_value $startpos "match" }
  | WITH
    { get_return_value $startpos "with" }
  | WHEN
    { get_return_value $startpos "when" }
  | c = CONSTRUCTOR
    { get_return_value $startpos ("constructor " ^ c)}
  | TYPE
    { get_return_value $startpos "type" }
  | OF
    { get_return_value $startpos "of" }
  | VERTBAR
    { get_return_value $startpos "|" }
  | TBOOL
    { get_return_value $startpos "bool" }
  | TINT
    { get_return_value $startpos "int" }
  | TSTRING
    { get_return_value $startpos "string" }
  | TCHAR
    { get_return_value $startpos "char" }
  | TUNIT
    { get_return_value $startpos "unit" }
  | UNIT
    { get_return_value $startpos "()" }
;

types:
  | i=TUNIT                     {i, TUNIT}
  | i=TBOOL                     {i, TBOOL}
  | i=TINT                      {i, TINT}
  | i=TCHAR                     {i, TCHAR}
  | i=TSTRING                   {i, TSTRING}
;

variant:
  | c = CONSTRUCTOR OF i = ID                       {c}
  | c = CONSTRUCTOR                                 {c}
  | v = variant VERTBAR c = CONSTRUCTOR OF i = ID   {v}
  | v = variant VERTBAR c = CONSTRUCTOR             {v}
;