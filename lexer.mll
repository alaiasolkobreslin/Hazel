{
  open Parser
  exception LexingError of string info

 (*Lifted from the RML assignment. I assume it just starts crawling through a 
 file for the lexer. *)
  let info_of_buf lexbuf = 
    let open Lexing in
    let start = lexbuf.lex_start_p in
    let curr = lexbuf.lex_curr_p in {
      filename = curr.pos_fname;
      start_lin = curr.pos_lnum;
      end_lin = curr.pos_lnum;
      start_col = start.pos_cnum - start.pos_bol + 1;
      end_col = curr.pos_cnum - start.pos_bol + 1;
    }
}

let var = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']*
let constructor = ['A'-'Z'] ['a'-'z' 'A'-'Z' '_']*
let int = ['0'-'9'] ['0'-'9']*
let str = ['"'] [^'\n''"']* ['"']
let whitespace = [' ' '\t' '\r' '\n']

rule token = parse
|'\n'             {Lexing.new_line lexbuf; token lexbuf}
|int as n         {INT (info_of_buf lexbuf, int_of_string n)}
|whitespace       {token lexbuf}
|constructor as c {CONSTRUCTOR (info_of_buf lexbuf, c)}
|var as v         {ID (info_of_buf lexbuf)}

|"true"           {TRUE (info_of_buf lexbuf)}
|"false"          {FALSE (info_of_buf lexbuf)}
|"&&"             {AND (info_of_buf lexbuf)}
|"||"             {OR (info_of_buf lexbuf)}
|"="              {EQ (info_of_buf lexbuf)}
|"<>"             {NEQ (info_of_buf lexbuf)}
|"!="
|"<"              {LT (info_of_buf lexbuf)}
|">"              {GT (info_of_buf lexbuf)}

|"if"             {IF (info_of_buf lexbuf)}
|"then"           {THEN (info_of_buf lexbuf)}
|"else"           {ELSE (info_of_buf lexbuf)}

|"{"              {LEFT_BRACE (info_of_buf lexbuf)}
|"}"              {RIGHT_BRACE (info_of_buf lexbuf)}
|"["              {LEFT_BRACK (info_of_buf lexbuf)}
|"]"              {RIGHT_BRACK (info_of_buf lexbuf)}
|"("              {LEFT_PAREN (info_of_buf lexbuf)}
|")"              {RIGHT_PAREN (info_of_buf lexbuf)}
|"begin"          {BEGIN (info_of_buf lexbuf)}
|"end"            {END (info_of_buf lexbuf)}

|":"              {COLON (info_of_buf lexbuf)}
|"::"             {CONS (info_of_buf lexbuf)}
|","              {COMMA (info_of_buf lexbuf)}

|"let"            {LET (info_of_buf lexbuf)}
|"rec"            {REC (info_of_buf lexbuf)}
|"in"             {IN (info_of_buf lexbuf)}
|"and"            {MUTUAL_REC (info_of_buf lexbuf)}

|"type"           {TYPE (info_of_buf lexbuf)}
|"of"             {OF (info_of_buf lexbuf)}
