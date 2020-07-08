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
%token AND OR EQ NEQ PEQ PNEQ LT LEQ GT GEQ NOT PIPE CAT

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

/* Precedence and Associativity */


%right ARROW

%nonassoc IN ELSE

%left SEMICOLON

%nonassoc ASSIGNREF

%left PIPE
%right CONS
%left OR
%left AND
%left EQ NEQ PEQ PNEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left HMUL DIVIDE MOD TIMES
%left CAT

%right NOT
%right DEREFERENCE
%right REFERENCE

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
  | PIPE
    { get_return_value $startpos "|>" }
  | CAT
    { get_return_value $startpos "^"}
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
  | l1=list(open_stmnt); l2=list(alias); e=option(expr); EOF   { make_prog l1 l2 e $startpos }
;

open_stmnt:
  | OPEN i=ID                               { make_open i $startpos }

expr:
  | IF e1=expr THEN e2=expr ELSE e3=expr         { make_if_then e1 e2 e3 $startpos }
  | a=app                                        { a }
  | e1=expr b=bop e2=expr                        { make_binop b e1 e2 $startpos }
  | u=uop e=expr                                 { make_unop u e $startpos }
  | c=CONSTRUCTOR e=value                        { make_variant c e $startpos }
  | CONSTRAINT i=ID EQ e=expr                    { make_constraint i e $startpos }
  | FUN a=arg ARROW e=expr                       { make_fun a e $startpos }
  | LET p=pattern EQ e=expr IN e2=expr           { make_let_notf p e e2 $startpos }
  | LET p=pattern a=arg EQ e=expr IN e2=expr     { make_let_f p a e e2 $startpos }
  | m=mutualrec                                  { m }
  | p=pmatch                                     { p }
;

value:
  | i=INT                                   { make_int (Int64.of_string i) $startpos }
  | s=STRING                                { let (st, p) = s in make_string st p }
  | c=CHAR                                  { let (ch, p) = c in make_char ch p }
  | i=ID                                    { make_var i $startpos }
  | TRUE                                    { make_bool true $startpos }
  | FALSE                                   { make_bool false $startpos }
  | LEFT_PAREN RIGHT_PAREN                  { make_unit $startpos }
  | LEFT_PAREN e=expr RIGHT_PAREN           { e }
  | t=tuple                                 { make_tup t $startpos }
  | r=vrecord                               { r }
  | LEFT_BRACK RIGHT_BRACK                  { make_nil $startpos }
  | LEFT_BRACK a = arr                      { a }
  | BEGIN e=expr END                        { e }

app:
  | a=app v=value                           { make_app a v $startpos }
  | v=value                                 { v }

arg:
  | a=arg i=pattern       { make_args i a }
  | i=pattern             { make_args i [] }

mutualrec:
  |LET REC p=pattern EQ e=expr                { make_letrec_notf p e $startpos }
  |LET REC p=pattern a=arg EQ e=expr          { make_letrec_f p a e $startpos }
  |m=mutualrec MUTUAL_REC p=pattern EQ e=expr        { make_and_notf m p e $startpos }
  |m=mutualrec MUTUAL_REC p=pattern a=arg EQ e=expr  { make_and_f m p a e $startpos}
  |m=mutualrec IN e=expr                      { complete_m_rec m e }

pmatch:
  |MATCH e=expr WITH                                        { make_init_pmatch e $startpos }
  |pm=pmatch VERTBAR pt=pattern ARROW e=expr                { make_update_pmatch pm pt e None $startpos }
  |pm=pmatch VERTBAR pt=pattern WHEN e1=expr ARROW e2=expr  { make_update_pmatch pm pt e1 (Some e2) $startpos }

vrecord:
  |LEFT_BRACE i=ID EQ e=expr            { make_init_record i e $startpos }
  |v=vrecord SEMICOLON i=ID EQ e=expr   { make_update_record v i e $startpos }
  |v=vrecord RIGHT_BRACE                { v }

%inline bop:
  | PLUS                                    { Plus }
  | MINUS                                   { Minus }
  | TIMES                                   { Mult }
  | DIVIDE                                  { Div }
  | MOD                                     { Mod }
  | HMUL                                    { HMult }
  | CONS                                    { ConsBop }
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
  | CAT                                     { Cat } 
  | PIPE                                    { Pipe }
;

%inline uop:
  | NOT                                     { Not }
  | MINUS                                   { Neg }
  | REFERENCE                               { Ref }
  | DEREFERENCE                             { Deref }
;

tuple:
  | LEFT_PAREN e1=expr l=list(COMMA; e2=expr { e2 }) RIGHT_PAREN   { e1::l }

arr:
  | e = expr SEMICOLON a = arr              { make_arr e a $startpos }
  | e = expr RIGHT_BRACK                    { make_arr (e) (make_nil $startpos) $startpos }

pattern:
  | LEFT_PAREN RIGHT_PAREN                  { make_unit_pat $startpos }
  | UNDERSCORE                              { make_wild_pat $startpos }
  | i=INT                                   { make_int_pat (Int64.of_string i) $startpos }
  | TRUE                                    { make_bool_pat true $startpos }
  | FALSE                                   { make_bool_pat false $startpos }
  | s=STRING                                { let (str, pos) = s in make_string_pat str pos }
  | i=ID                                    { make_var_pat i $startpos }
  | t=tuple_pat                             { make_tup_pat t $startpos }
  | LEFT_PAREN p=pattern RIGHT_PAREN        { p }
  | i=CONSTRUCTOR p=pattern                 { make_sum_pat i p $startpos }
  | LEFT_BRACK RIGHT_BRACK                  { make_nil_pat $startpos }
  | p1=pattern CONS p2=pattern              { make_cons_pat p1 p2 $startpos }

tuple_pat:
  | LEFT_PAREN p1=pattern l=list(COMMA; p2=pattern { p2 }) RIGHT_PAREN   { p1::l }

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
  |LEFT_BRACE i1=ID COLON g=generic               {[(i1, g)]}
  |r=pre_record SEMICOLON i1=ID COLON g=generic   {(i1, g)::r}
  
record: 
  |r=pre_record RIGHT_BRACE                       {TRecord (List.rev r)}

pre_variant:
  | c=CONSTRUCTOR OF g=generic                        {[(c, Some g)]}
  | c=CONSTRUCTOR                                     {[(c, None)]}
  | v=pre_variant VERTBAR c=CONSTRUCTOR OF g=generic  {(c, Some g)::v}
  | v=pre_variant VERTBAR c=CONSTRUCTOR               {(c, None)::v}

variant:
  | v=pre_variant END                                {TSum (List.rev v)}

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