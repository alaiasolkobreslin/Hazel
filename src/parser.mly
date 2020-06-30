%{
    open Lexing
    open Ast
    open Ast_factory

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

%token TBOOL TINT TSTRING TCHAR TUNIT TLIST

%token UNIT

%token TRUE FALSE
%token AND OR EQ NEQ PEQ PNEQ LT LEQ GT GEQ NOT

%token LEFT_BRACE RIGHT_BRACE LEFT_BRACK RIGHT_BRACK LEFT_PAREN RIGHT_PAREN UNDERSCORE BEGIN END

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

%start prog
%type <Ast.parsed Ast.prog> prog
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
  | UNDERSCORE
    { get_return_value $startpos "_" }
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
  | TLIST
    { get_return_value $startpos "list" }
  | TUNIT
    { get_return_value $startpos "unit" }
  | UNIT
    { get_return_value $startpos "()" }
;

prog:
  | l1=list(open_stmnt); l2=list(alias); e=expr; EOF   { make_prog l1 l2 e $startpos }
;

open_stmnt:
  | OPEN i=ID                               { make_open i $startpos }

expr:
  | LEFT_PAREN RIGHT_PAREN                  { make_unit $startpos }
  | LEFT_PAREN e=expr RIGHT_PAREN           { e }
  | BEGIN e=expr END                        { e }
  | i=INT                                   { make_int (Int64.of_string i) $startpos }
  | TRUE                                    { make_bool true $startpos }
  | FALSE                                   { make_bool false $startpos }
  | s=STRING                                { let (st, p) = s in make_string st p }
  | c=CHAR                                  { let (ch, p) = c in make_char ch p }
  | i=ID                                    { make_var i $startpos }
  | e=expr COMMA t=tuple                    { make_tup (e::t) $startpos }  
  | IF e1=expr THEN e2=expr ELSE e3=expr    { make_if_then e1 e2 e3 $startpos }
  | e1=expr e2=expr                         { make_app e1 e2 $startpos }
  | e1=expr b=bop e2=expr                   { make_binop b e1 e2 $startpos }
  | u=uop e=expr                            { make_unop u e $startpos }
  (* let goes here *)
  (* let rec goes here *)
  (*etc. *)
  | c=CONSTRUCTOR t=types                   {make_variant c t $startpos}
  | CONSTRAINT i=ID EQ e=expr               {make_constraint i e $startpos}
;

bop:
  | PLUS                                    { Plus }
  | MINUS                                   { Minus }
  | TIMES                                   { Mult }
  | DIVIDE                                  { Div }
  | MOD                                     { Mod }
  | HMUL                                    { HMult }
  | CONS                                    { Cons }
  | SEMICOLON                               { Seq }
  | GT                                      { GT }
  | LT                                      { LT }
  | GEQ                                     { GEQ }
  | LEQ                                     { LEQ }
  | EQ                                      { EQ }
  | NEQ                                     { NEQ }
  | PEQ                                     { PEQ }
  | PNEQ                                    { PNEQ }
  | AND                                     { And }
  | OR                                      { Or }
  | ASSIGNREF                               { Ass }
;

uop:
  | NOT                                     { Not }
  | MINUS                                   { Neg }
  | REFERENCE                               { Ref }
  | DEREFERENCE                             { Deref }
;

tuple:
  | e=expr COMMA t=tuple                    { e::t }
  | e=expr                                  { e::[] }
;

pattern:
  | LEFT_PAREN RIGHT_PAREN                  { make_unit_pat $startpos }
  | UNDERSCORE                              { make_wild_pat $startpos }
  | i=INT                                   { make_int_pat (Int64.of_string i) $startpos }
  | TRUE                                    { make_bool_pat true $startpos }
  | FALSE                                   { make_bool_pat false $startpos }
  | s=STRING                                { make_string_pat s $startpos }
  | i=ID                                    { make_var_pat i $startpos }
  | p1=pattern COMMA p2=pattern             { make_pair_pat p1 p2 $startpos }
  | LEFT_PAREN p=pattern RIGHT_PAREN        { p }
  | i=ID p=pattern                          { make_sum_pat i p $startpos }
  | LEFT_BRACK RIGHT_BRACK                  { make_nil_pat $startpos }
  | p1=pattern CONS p2=pattern              { make_cons_pat p1 p2 $startpos }

alias:
  | TYPE i=ID EQ v=variant                     {make_talias i v $startpos}
  | TYPE i=ID EQ g=generic                     {make_talias i g $startpos}
  | TYPE i=ID EQ r=record                      {make_talias i r $startpos}

types:
  | i=TUNIT                     {TUnit}
  | i=TBOOL                     {TBool}
  | i=TINT                      {TInt}
  | i=TCHAR                     {TChar}
  | i=TSTRING                   {TString}

pre_tuplet:
  | LEFT_PAREN g=generic                           {[g]}
  | t=pre_tuplet TIMES g=generic                   {g::t}

tuplet:
  | t=pre_tuplet RIGHT_PAREN                       {TProd (List.rev t)}

pre_record:
  |LEFT_BRACK i1=ID COLON g=generic           {[(i1, g)]}
  |r=pre_record SEMICOLON i1=ID COLON g=generic   {(i1, g)::r}
  
 record: 
  |r=pre_record RIGHT_BRACK                       {TRecord r}

pre_variant:
  | c=CONSTRUCTOR OF g=generic                        {[(c, Some g)]}
  | c=CONSTRUCTOR                                     {[(c, None)]}
  | v=pre_variant VERTBAR c=CONSTRUCTOR OF g=generic  {(c, Some g)::v}
  | v=pre_variant VERTBAR c=CONSTRUCTOR               {(c, None)::v}

variant:
  | v=pre_variant END                                {TSum v}

list_t:
  |g=generic TLIST                               {TCons g}

reference:
  |g=generic REFERENCE                           {TRef g}

function_t:
  |g1 = generic ARROW g2 = generic             {TFun (g1, g2)}

generic:
  |r = reference            {r}
  |l = list_t               {l}
  |t = types                {t}
  |tu = tuplet              {tu}
  |f = function_t           {f}
  |i = ID                   { TPlaceholder i }