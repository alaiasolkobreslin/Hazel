{
  open Parser
  open Lexing 

  exception LexingError of Lexing.position * string

  (* let convert_to_unicode str pos = failwith "" 
     *)

  let string_to_char str pos =
    match str with
    | "\\\\" -> "\\"
    | "\\n" -> "\n"
    | "\\t" -> "\t"
    | "\\r" -> "\r"
    | "\\\"" -> "\""
    | "\\\'" -> "\'"
    | _ ->
      begin
        let chop_str = String.sub str 1 (String.length str - 1) in
        let hex_str = "0" ^ chop_str in
        print_endline hex_str;
        let decimal = int_of_string hex_str in
        if Uchar.is_valid decimal then
          decimal |> Uchar.of_int |> Uchar.to_char |> Char.escaped else
        raise (LexingError (pos, "unrecognized hex sequence"))
      end
}

let new_line = '\n' | '\r''\n'

let single_int = ['0'-'9']
let single_char = ['a'-'f' 'A' - 'F']
let hex_char = single_int | single_char
let hex = '\\' 'x' hex_char (hex_char)? (hex_char)? (hex_char)?

let escape_char = '\\' ['n' 't' 'r' '\\' '\"' '\'']

let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let constructor = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_']*
let int = ['0'-'9'] ['0'-'9']*
let str = ['"'] [^'"']* ['"']
let whitespace = [' ' '\t']

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
|"\""             { STRING (str "" lexbuf.lex_start_p lexbuf) }
|"\'"             { CHAR (chr "" lexbuf.lex_start_p lexbuf) }

|var              {ID (Lexing.lexeme lexbuf)}

|eof              {EOF}
| _               {raise (LexingError (lexbuf.lex_start_p, "unrecognized token"))}


and comment = parse
  | "*)" { token lexbuf }
  | eof
      { raise (LexingError (lexbuf.lex_start_p, "unclosed comment")) }
  | _ { comment lexbuf }

and str buf pos = parse
  | escape_char 
      { let uni = string_to_char (Lexing.lexeme lexbuf) pos in
        str (buf ^ uni) pos lexbuf }
  | hex
      { let uni = string_to_char (Lexing.lexeme lexbuf) pos in
        str (buf ^ uni) pos lexbuf }
  | "\"" { buf, pos }
  | eof | "\n"
      { raise (LexingError (lexbuf.lex_start_p, "unclosed string")) }
  | "\\"
      { raise (LexingError (lexbuf.lex_start_p, "invalid escape sequence")) }
  | _ { raise (LexingError (lexbuf.lex_start_p, "unrecognized character"))}

and chr buf pos = parse
  | escape_char 
      { let uni = string_to_char (Lexing.lexeme lexbuf) pos in
        chr (buf ^ uni) pos lexbuf }
  | hex
      { let uni = string_to_char (Lexing.lexeme lexbuf) pos in
        chr (buf ^ uni) pos lexbuf }
  | "\'" { (string_to_char buf pos, pos) }
  | eof | "\n"
      { raise (LexingError (lexbuf.lex_start_p, "unclosed string")) }
  | "\\"
      { raise (LexingError (lexbuf.lex_start_p, "invalid escape sequence")) }
  | _ { raise (LexingError (lexbuf.lex_start_p, "unrecognized character"))}