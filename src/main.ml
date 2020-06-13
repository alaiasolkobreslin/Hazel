open Cli

let perform_commands commands files out_dir = 
  failwith "unimplemented"

let _ = match parse_command () with
  | {commands; files; out_dir} -> perform_commands commands files out_dir