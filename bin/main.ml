open Hazel.Cli
open Lexing
open Hazel.Lexer
open Parsing
open Hazel.Ast
open Hazel.Parser
open Hazel.Sexpr

let report_lex_error file_name out_channel pos message =
  let l_num = pos.pos_lnum |> string_of_int in
  let c_num = pos.pos_cnum |> string_of_int in
  let error_msg = l_num ^ ":" ^ c_num ^ " error:" ^ message in
  if out_channel = stdout then () else
    output_string out_channel error_msg;
  prerr_endline ("Lexical error beginning at "
                 ^file_name^":"^l_num^":"^c_num^": "^message)

let lex_channel write_out in_channel out_channel file_name print_std =
  try
    let lexbuf = from_channel in_channel in
    let rec step_lexer op =
      match op with
      | None -> ()
      | Some tok -> 
        begin
          if write_out then
            output_string out_channel tok;
            begin
              if print_std then output_string stdout tok
            end;
          lexer token lexbuf |> step_lexer
        end in
    step_lexer (Some "");
    close_out out_channel; None
  with
  | LexingError (p, msg) ->
    report_lex_error file_name out_channel p msg; None
  | Parse_error -> failwith "unimplemented"

let parse_channel write_out in_channel out_channel file_name print_std = 
  try
    let lexbuf = from_channel in_channel in
    let fmt = Format.formatter_of_out_channel out_channel in
    if write_out then
      begin
        let ast = prog token lexbuf in
        let sexpr = ast |> prog_to_sexpr in
        pp_print_sexpr fmt sexpr;
        if print_std then
        begin
          let std_fmt = Format.formatter_of_out_channel stdout in 
          pp_print_sexpr std_fmt sexpr;
          Format.pp_force_newline std_fmt ();
          Format.pp_print_flush std_fmt ()
        end;
        Format.pp_force_newline fmt ();
        Format.pp_print_flush fmt ();
        Some ast
      end
    else Some (prog token lexbuf)
  with
  | LexingError (p, msg) ->
    report_lex_error file_name out_channel p msg; None
  | Parse_error -> failwith "unimplemented"

let chop_file file_name ext =
  (* print_endline file_name; *)
  if Filename.check_suffix file_name ".haze" then
    Some ((Filename.chop_suffix file_name ".haze" ) ^ ext) else None

let rec mkdir_path path =
  if Sys.file_exists path then ()
  else 
    (mkdir_path (Filename.dirname path);
     Unix.mkdir path 0o777)

let write_out_file func out_dir file_name ext print_std =
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
      try func true (open_in file_name) (open_out file) file_name print_std |> ignore with
      | Sys_error msg -> prerr_endline msg
    end

let perform_commands commands files out_dir print_std = 
  if List.exists (fun c -> c = Lex) commands then
    List.map (fun f -> write_out_file lex_channel out_dir f ".lexed" print_std) files
    |> ignore else
  if List.exists (fun c -> c = Parse) commands then
    List.map (fun f -> write_out_file parse_channel out_dir f ".parsed" print_std) files
    |> ignore else ()

let _ = match parse_command () with
  | {commands; files; out_dir; print_std} -> perform_commands commands files out_dir print_std