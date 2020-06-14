{
  open Parser
  open Lexing 

  exception LexingError of Lexing.position * string
}

let new_line = '\n' | '\r''\n'

let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let constructor = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_']*
let int = ['0'-'9'] ['0'-'9']*
let str = ['"'] [^'"']* ['"']
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
|new_line         {Lexing.new_line lexbuf; token lexbuf}
|whitespace+      {token lexbuf}
|int              {INT (Lexing.lexeme lexbuf)}
|"*"              {TIMES}
|"+"              {PLUS}
|"-"              {MINUS}
|"/"              {DIVIDE}
|"mod"            {MOD}

|"true"           {TRUE}
|"false"          {FALSE}
|"&&"             {AND}
|"||"             {OR}
|"="              {EQ}
|"<>"             {NEQ}
|"!="             {PNEQ}
|"=="             {PEQ}
|"<"              {LT}
|"<="             {LEQ}
|">"              {GT}
|">="             {GEQ}
|"not"            {NOT}

|"if"             {IF}
|"then"           {THEN}
|"else"           {ELSE}

|"match"          {MATCH}
|"with"           {WITH}
|"when"           {WHEN}

|"|"              {VERTBAR}
|"{"              {LEFT_BRACE}
|"}"              {RIGHT_BRACE}
|"["              {LEFT_BRACK}
|"]"              {RIGHT_BRACK}
|"("              {LEFT_PAREN}
|")"              {RIGHT_PAREN}
|"begin"          {BEGIN}
|"end"            {END}

|":"              {COLON}
|";"              {SEMICOLON}
|"::"             {CONS}
|","              {COMMA}
|"open"           {OPEN}

|"let"            {LET}
|"rec"            {REC}
|"in"             {IN}
|"and"            {MUTUAL_REC}

|"type"           {TYPE}
|"of"             {OF}
|"constraint"     {CONSTRAINT}
|constructor      {CONSTRUCTOR (Lexing.lexeme lexbuf)}

|"ref"            {REFERENCE}
|"!"              {DEREFERENCE}
|":="             {ASSIGNREF}

|"lambda"         {FUN}
|"->"             {ARROW}

|"any"            {ANY}

|"(*"             { comment lexbuf }

|var              {ID (Lexing.lexeme lexbuf)}

|eof              {EOF}
| _               {raise (LexingError (lexbuf.lex_start_p, "unrecognized token"))}


and comment = parse
  | "*)" { Lexing.new_line lexbuf; token lexbuf }
  | eof
      { EOF }
  | _ { comment lexbuf }