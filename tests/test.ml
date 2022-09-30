open OUnit2

let suite =
  (* "full test suite" >::: List.flatten [ Typechecker.suite; Type.suite ] *)
  "full test suite" >::: List.flatten [ Type.suite ]

let () = run_test_tt_main suite