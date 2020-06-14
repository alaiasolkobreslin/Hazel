open Cli
open Lexing
open Lexer
open Parser

let report_lex_error file_name out_channel pos message =
  let l_num = pos.pos_lnum |> string_of_int in
  let c_num = pos.pos_cnum |> string_of_int in
  let error_msg = l_num ^ ":" ^ c_num ^ " error:" ^ message in
  if out_channel = stdout then () else
    output_string out_channel error_msg;
  prerr_endline ("Lexical error beginning at "
                 ^file_name^":"^l_num^":"^c_num^": "^message)

let lex_channel write_out in_channel out_channel file_name =
  try
    let lexbuf = from_channel in_channel in
    let rec step_lexer op =
      match op with
      | None -> ()
      | Some tok -> 
        begin
          if write_out then
            output_string out_channel tok;
          Parser.lexer Lexer.token lexbuf |> step_lexer
        end in
    step_lexer (Some "");
    close_out out_channel
  with
  | Lexer.LexingError (p, msg) ->
    report_lex_error file_name out_channel p msg
  | Parsing.Parse_error -> failwith "unimplemented"

let perform_commands commands files out_dir = 
  failwith "unimplemented"

let _ = match parse_command () with
  | {commands; files; out_dir} -> perform_commands commands files out_dir