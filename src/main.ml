open Cli
open Lexing
open Lexer
open Parser
open Typecheck

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
    close_out out_channel; None
  with
  | Lexer.LexingError (p, msg) ->
    report_lex_error file_name out_channel p msg; None
  | Parsing.Parse_error -> failwith "unimplemented"

let parse_channel write_out in_channel out_channel file_name = 
  try
    let lexbuf = from_channel in_channel in
    let fmt = Format.formatter_of_out_channel out_channel in
    if write_out then
      begin
        let ast = Parser.prog Lexer.token lexbuf in
        ast
        |> Ast.prog_to_sexpr
        |> Sexpr.pp_print_sexpr fmt;
        Format.pp_force_newline fmt ();
        Format.pp_print_flush fmt ();
        Some ast
      end
    else Some (Parser.prog Lexer.token lexbuf)
  with
  | Lexer.LexingError (p, msg) ->
    report_lex_error file_name out_channel p msg; None
  | Parsing.Parse_error -> failwith "unimplemented"

let chop_file file_name ext =
  print_endline file_name;
  if Filename.check_suffix file_name ".haze" then
    Some ((Filename.chop_suffix file_name ".haze" ) ^ ext) else None

let rec mkdir_path path =
  if Sys.file_exists path then ()
  else 
    (mkdir_path (Filename.dirname path);
     Unix.mkdir path 0o777)

let write_out_file func out_dir file_name ext =
  let file_out = (Filename.concat out_dir file_name 
                  |> chop_file) ext in
  match file_out with 
  | None -> print_endline ("Invalid file extension for file: " ^ file_name)
  | Some file -> 
    begin
      try Filename.dirname file |> mkdir_path with
      | _ -> print_endline "Filed to make directory"
    end;
    begin
      try func true (open_in file_name) (open_out file) file_name |> ignore with
      | Sys_error msg -> prerr_endline msg
    end

let perform_commands commands files out_dir = 
  if List.exists (fun c -> c = Lex) commands then
    List.map (fun f -> write_out_file lex_channel out_dir f ".lexed") files
    |> ignore else
  if List.exists (fun c -> c = Parse) commands then
    List.map (fun f -> write_out_file parse_channel out_dir f ".parsed") files
    |> ignore else ()

let _ = match parse_command () with
  | {commands; files; out_dir} -> perform_commands commands files out_dir