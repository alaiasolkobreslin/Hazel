open Hazel.Type
open OUnit2

let make_fresh_var_test (name : string) (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (fresh ())

let fresh_tests = [ make_fresh_var_test "first is a" "a" ]
let suite = List.flatten [ fresh_tests ]