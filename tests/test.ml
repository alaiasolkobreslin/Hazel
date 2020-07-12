open OUnit2
open Main

let compare_files f1 f2 = 
  let in_chan_f1 = open_in f1 in
  let in_chan_f2 = open_in f2 in
  let lines_f1 = ref [] in
  let lines_f2 = ref [] in
  try
    while true; do
      lines_f1 := input_line in_chan_f1 :: !lines_f1;
      lines_f2 := input_line in_chan_f2 :: !lines_f2
    done; lines_f1 = lines_f2
  with End_of_file ->
    close_in in_chan_f1;
    close_in in_chan_f2;
    lines_f1 = lines_f2

let make_lex_file_test
    (name : string)
    (input_file : string)
    (expected_file : string) : test = 
  name >:: (fun _ ->
      write_out_file lex_channel "" input_file ".lexed";
      match chop_file input_file ".lexed" with
      | Some file -> assert_equal file 
                       expected_file ~cmp:compare_files
      | None -> assert_equal true false)

let lexer_tests = [
  make_lex_file_test "functions.haze" "pa1/functions.haze" "pa1/functions.lexedsol"
]

let _ = List.flatten [lexer_tests]