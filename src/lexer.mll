{
  open Parser
  open Lexing 

  exception LexingError of Lexing.position * string

  let convert_to_unicode str pos = 
    match str with
        | "\\n" -> "\n"
        | "\\\\" -> "\\"
        | "\\\'" -> "\'"
        | "\\\"" -> "\""
        | "\\t" -> "\t"
        | "\\r" -> "\r"
        | _ -> let len_of_s = String.length str in
               let hex = String.sub str 1 (len_of_s -1) in
    try
      begin
        (* parsing hex to int and making sure it's a valid unicode character *)
        let i = int_of_string ("0"^hex) |> Uchar.of_int |> Uchar.to_int in
        (* making a bytes of size 2 because with only 4 hex digits we can
           only get to 2 bytes *)
        let b = Bytes.create 2 in
        Bytes.set_uint16_be b 0 i;
        let pow n p =
          (float_of_int n) ** (float_of_int p) |> int_of_float in
        let start =
          (* if ASCII, then only one byte *)
          if i < pow 2 8 then 1
          (* otherwise start at index 0 because it takes up two bytes *)
          else if i < pow 2 16 then 0
          else raise (LexingError (pos, "parser error"))  in
        Bytes.sub_string b start (2 - start)
      end
    with 
    | _ -> raise (LexingError (pos, "invalid character")) 

  (** [str_to_char str pos] returns [(str, pos)] if [str] is a valid unicode
      character *)
  let string_to_char str pos = 
    let bytes = Bytes.of_string str in
    match Bytes.length bytes with
    | 0 -> raise (LexingError (pos, "empty character")) 
    | 1 -> (str, pos)
    | 2 ->  let b1 = Char.code (Bytes.get bytes 0) in
            let b2 = Char.code (Bytes.get bytes 1) in
            if b1 lsr 5 = 6 && b2 lsr 6 = 2 then (str, pos) else
            raise (LexingError (pos, "invalid character")) 
    | 3 ->  let b1 = Char.code (Bytes.get bytes 0) in
            let b2 = Char.code (Bytes.get bytes 1) in
            let b3 = Char.code (Bytes.get bytes 2) in
            if b1 lsr 4 = 14 && b2 lsr 6 = 2 && b3 lsr 6 = 2 then (str, pos) else
            raise (LexingError (pos, "invalid character")) 
    | 4 ->  let b1 = Char.code (Bytes.get bytes 0) in
            let b2 = Char.code (Bytes.get bytes 1) in
            let b3 = Char.code (Bytes.get bytes 2) in
            let b4 = Char.code (Bytes.get bytes 3) in
            if b1 lsr 3 = 30 && b2 lsr 6 = 2 &&
                b3 lsr 6 = 2 && b4 lsr 6 = 2 then (str, pos) else
            raise (LexingError (pos, "invalid character")) 
    | _ -> raise (LexingError (pos, "invalid character")) 
}

let new_line = '\n' | '\r''\n'

let hex_char = ['a'-'f' 'A'-'F' '0'-'9']
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

|"bool"           {TBOOL}
|"int"            {TINT}
|"string"         {TSTRING}
|"char"           {TCHAR}
|"unit"           {TUNIT}
|"list"           {TLIST}

|"()"             {UNIT}

|"*"              {TIMES}
|"+"              {PLUS}
|"-"              {MINUS}
|"/"              {DIVIDE}
|"mod"            {MOD}
|">>*"            {HMUL}

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
|"_"              {UNDERSCORE}
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
|"'"              { CHAR (chr "" lexbuf.lex_start_p lexbuf) }

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
      { let uni = convert_to_unicode (Lexing.lexeme lexbuf) pos in
        str (buf ^ uni) pos lexbuf }
  | hex
      { let uni = convert_to_unicode (Lexing.lexeme lexbuf) pos in
        str (buf ^ uni) pos lexbuf }
  | "\"" { buf, pos }
  | eof | "\n"
      { raise (LexingError (lexbuf.lex_start_p, "unclosed string")) }
  | "\\"
      { raise (LexingError (lexbuf.lex_start_p, "invalid escape sequence")) }
  | _ { str (buf^(Lexing.lexeme lexbuf)) pos lexbuf }

and chr buf pos = parse
  | escape_char 
      { let uni = convert_to_unicode (Lexing.lexeme lexbuf) pos in
        chr (buf ^ uni) pos lexbuf }
  | hex
      { let uni = convert_to_unicode (Lexing.lexeme lexbuf) pos in
        chr (buf ^ uni) pos lexbuf }
  | "'" { string_to_char buf pos }
  | eof | "\n"
      { raise (LexingError (lexbuf.lex_start_p, "unclosed string")) }
  | "\\"
      { raise (LexingError (lexbuf.lex_start_p, "invalid escape sequence")) }
  | _ { chr (buf^(Lexing.lexeme lexbuf)) pos lexbuf }