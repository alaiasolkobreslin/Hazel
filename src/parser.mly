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

expr:
  | LEFT_PAREN RIGHT_PAREN                  { make_unit $startpos }
  | LEFT_PAREN e=expr RIGHT_PAREN           { e }
  | i=INT                                   { make_int (Int64.of_string i) $startpos }
  | TRUE                                    { make_bool true $startpos }
  | FALSE                                   { make_bool false $startpos }
  | s=STRING                                { make_string s $startpos }
  | c=CHAR                                  { make_char c $startpos }
  | i=ID                                    { make_var i $startpos }
  | LEFT_PAREN t=tuple                      { make_tup t $startpos }
  | IF e1=expr THEN e2=expr ELSE e3=expr    { make_if_then e1 e2 e3 $startpos }
  | e1=expr b=bop e2=expr                   { make_binop b e1 e2 $startpos }
  | u=uop e=expr                            { make_unop u e $startpos }
  (* let goes here *)
  (* let rec goes here *)
  (*etc. *)
  | c=CONSTRUCTOR e=expr                    {make_variant c e $startpos}
  | c=CONSTRAINT e=expr                     {make_constraint c e $startpos}
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
;

uop:
  | NOT                                     { Not }
  | MINUS                                   { Neg }
  | REFERENCE                               { Ref }
  | DEREFERENCE                             { Deref }
;

tuple:
  | e=expr RIGHT_PAREN          { e::[] }
  | e=expr COMMA t=tuple        { e::t }
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

types:
  | i=TUNIT                     {make_tunit i $startpos}
  | i=TBOOL                     {make_tbool i $startpos}
  | i=TINT                      {make_tint i $startpos}
  | i=TCHAR                     {make_tchar i $startpos}
  | i=TSTRING                   {make_tstring i $startpos}

tuplet:
  | LEFT_PAREN i=ID                           {[TPlaceholder i]}
  | LEFT_PAREN t=types                        {[t]}
  | LEFT_PAREN r=record                       {[r]}
  | LEFT_PAREN l=list                         {[l]}
  | t=tuplet TIMES i=ID                       {(TPlaceholder i)::t}
  | tu=tuplet TIMES t=types                   {t::tu}
  | t=tuplet TIMES r=record                   {r::t}
  | t=tuplet TIMES l=list                     {t::l}
  | t=tuplet RIGHT_PAREN                      {make_tprod t $startpos}

record:
  |LEFT_BRACK i1=ID COLON i2=ID               {[(i1, TPlaceholder i2)]}
  |LEFT_BRACK i1=ID COLON t=types             {[(i1, t)]}
  |LEFT_BRACK i1=ID COLON t=tuplet            {[(i1, t)]}
  |LEFT_BRACK i1=ID COLON l=list              {[(i1, l)]}
  |r=record SEMICOLON i1=ID COLON i2=ID       {(i1, TPlaceholder i2)::r}
  |r=record SEMICOLON i1=ID COLON t=types     {(i1, t)::r}
  |r=record SEMICOLON i1=ID COLON t=tuplet    {(i1, t)::r}
  |r=record SEMICOLON i1=ID COLON l=list      {(i1, l)::r}
  |r=record RIGHT_BRACK                       {make_trecord r}

variant:
  | c=CONSTRUCTOR OF i = ID                      {[(c, Some TPlaceholder i)]}
  | c=CONSTRUCTOR OF t=types                     {[(c, Some t)]}
  | c=CONSTRUCTOR OF t=tuplet                    {[(c, Some t)]}
  | c=CONSTRUCTOR OF r=record                    {[(c, Some r)]}
  | c=CONSTRUCTOR OF l=list                      {[(c, Some l)]}
  | c=CONSTRUCTOR                                {[(c, None)]}
  | v=variant VERTBAR c=CONSTRUCTOR OF i=ID      {(c, Some TPlaceholder i)::v}
  | v=variant VERTBAR c=CONSTRUCTOR OF t=types   {(c, Some t)::v}
  | v=variant VERTBAR c=CONSTRUCTOR OF t=tuplet  {(c, Some t)::v}
  | v=variant VERTBAR c=CONSTRUCTOR OF r=record  {(c, Some r)::v}
  | v=variant VERTBAR c=CONSTRUCTOR OF l=list    {(c, Some l)::v}
  | v=variant VERTBAR c=CONSTRUCTOR              {(c, None)::v}
  | v=variant END                                {make_tsum v $startpos}

list:
  |t=types TLIST                               {make_tlist t $startpos}
  |tu=tuplet TLIST                             {make_tlist tu $startpos}
  |r=record TLIST                              {make_tlist r $startpos}
  |i=ID TLIST                                  {make_tlist (TPlaceholder i) $startpos}

reference:
  |t=types REFERENCE                           {make_tref t $startpos}
  |tu=tuplet REFERENCE                         {make_tref tu $startpos}
  |r=record REFERENCE                          {make_tref r $startpos}
  |i=ID REFERENCE                              {make_tref (TPlaceholder i) $startpos}

function_t:
  |t1=types ARROW t2=types                      {make_tfun (t1, t2) $startpos}
  |t=types ARROW tu=tuplet                      {make_tfun (t, tu) $startpos}
  |t=types ARROW r=record                       {make_tfun (t, r) $startpos}
  |t=types ARROW l=list                         {make_tfun (t, l) $startpos}
  |t=types ARROW r=reference                    {make_tfun (t, r) $startpos}
  |t=types ARROW f=function_t                   {make_tfun (t, f) $startpos}
  |t=types ARROW i=ID                           {make_tfun (t, i) $startpos}

  |t1=tuplet ARROW t2=types                     {make_tfun (t1, t2) $startpos}
  |t=tuplet ARROW tu=tuplet                     {make_tfun (t, tu) $startpos}
  |t=tuplet ARROW r=record                      {make_tfun (t, r) $startpos}
  |t=tuplet ARROW l=list                        {make_tfun (t, l) $startpos}
  |t=tuplet ARROW r=reference                   {make_tfun (t, r) $startpos}
  |t=tuplet ARROW f=function_t                  {make_tfun (t, f) $startpos}
  |t=tuplet ARROW i=ID                          {make_tfun (t, i) $startpos}

  |r=record ARROW t2=types                      {make_tfun (r, t2) $startpos}
  |r=record ARROW tu=tuplet                     {make_tfun (r, tu) $startpos}
  |r1=record ARROW r2=record                    {make_tfun (r1, r2) $startpos}
  |r=record ARROW l=list                        {make_tfun (r, l) $startpos}
  |r=record ARROW r2=reference                  {make_tfun (r, r2) $startpos}
  |r=record ARROW f=function_t                  {make_tfun (r, f) $startpos}
  |r=record ARROW i=ID                          {make_tfun (r, i) $startpos}

  |l=list ARROW t2=types                        {make_tfun (l, t2) $startpos}
  |l=list ARROW tu=tuplet                       {make_tfun (l, tu) $startpos}
  |l=list ARROW r=record                        {make_tfun (l, r) $startpos}
  |l=list ARROW l2=list                         {make_tfun (l, l2) $startpos}
  |l=list ARROW r=reference                     {make_tfun (l, r) $startpos}
  |l=list ARROW f=function_t                    {make_tfun (l, f) $startpos}
  |l=list ARROW i=ID                            {make_tfun (l, i) $startpos}

  |r=reference ARROW t2=types                   {make_tfun (r, t2) $startpos}
  |r=reference ARROW tu=tuplet                  {make_tfun (r, tu) $startpos}
  |r=reference ARROW r2=record                  {make_tfun (r, r2) $startpos}
  |r=reference ARROW l=list                     {make_tfun (r, l) $startpos}
  |r=reference ARROW r2=reference               {make_tfun (r, r2) $startpos}
  |r=reference ARROW f=function_t               {make_tfun (r, f) $startpos}
  |r=reference ARROW i=ID                       {make_tfun (r, i) $startpos}

  |f=function_t ARROW t2=types                   {make_tfun (f, t2) $startpos}
  |f=function_t ARROW tu=tuplet                  {make_tfun (f, tu) $startpos}
  |f=function_t ARROW r2=record                  {make_tfun (f, r2) $startpos}
  |f=function_t ARROW l=list                     {make_tfun (f, l) $startpos}
  |f=function_t ARROW r2=reference               {make_tfun (f, r2) $startpos}
  |f=function_t ARROW f2=function_t              {make_tfun (f, f2) $startpos}
  |f=function_t ARROW i=ID                       {make_tfun (f, i) $startpos}

  |i=ID ARROW t2=types                         {make_tfun (i, t2) $startpos}
  |i=ID ARROW tu=tuplet                        {make_tfun (i, tu) $startpos}
  |i=ID ARROW r2=record                        {make_tfun (i, r2) $startpos}
  |i=ID ARROW l=list                           {make_tfun (i, l) $startpos}
  |i=ID ARROW r2=reference                     {make_tfun (i, r2) $startpos}
  |i=ID ARROW f2=function_t                    {make_tfun (i, f2) $startpos}
  |i=ID ARROW i2=ID                            {make_tfun (i, i2) $startpos}

alias:
  | TYPE i=ID EQ v=variant                     {make_alias i v $startpos}
  | TYPE i=ID EQ t=types                       {make_alias i t $startpos}
  | TYPE i=ID EQ p=tuplet                      {make_alias i p $startpos}
  | TYPE i=ID EQ l=list                        {make_alias i l $startpos}
  | TYPE i=ID EQ r=record                      {make_alias i r $startpos}
  | TYPE i=ID EQ r2=reference                  {make_alias i r2 $startpos}
  | TYPE i=ID EQ f=function_t                  {make_alias i f $startpos}