open Lexing
open Hazel.Ast
open Hazel.Typecheck
open OUnit2

let default_position =
  { pos_fname = "default position"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let default_env = []

let make_typed_expr_test (name : string) (expr : parsed expr_ann)
    (env : (constructor * types) list) (expected_output : typed expr_ann) : test
    =
  name >:: fun _ -> assert_equal expected_output (typecheck_expr expr [] env)

let basic_expr_tests =
  [
    make_typed_expr_test "true is bool"
      ({ parsed_pos = default_position; ptype = None }, Bool true)
      default_env
      ({ typed_pos = default_position; ttype = TBool }, Bool true);
    make_typed_expr_test "false is bool"
      ({ parsed_pos = default_position; ptype = None }, Bool false)
      default_env
      ({ typed_pos = default_position; ttype = TBool }, Bool false);
    make_typed_expr_test "not true is bool"
      ( { parsed_pos = default_position; ptype = None },
        Unaop (Not, ({ parsed_pos = default_position; ptype = None }, Bool true))
      )
      default_env
      ( { typed_pos = default_position; ttype = TBool },
        Unaop (Not, ({ typed_pos = default_position; ttype = TBool }, Bool true))
      );
    make_typed_expr_test "5 is int"
      ({ parsed_pos = default_position; ptype = None }, Int (Int64.of_int 5))
      default_env
      ({ typed_pos = default_position; ttype = TInt }, Int (Int64.of_int 5));
    make_typed_expr_test "-5 is int"
      ( { parsed_pos = default_position; ptype = None },
        Unaop
          ( Neg,
            ( { parsed_pos = default_position; ptype = None },
              Int (Int64.of_int 5) ) ) )
      default_env
      ( { typed_pos = default_position; ttype = TInt },
        Unaop
          ( Neg,
            ( { typed_pos = default_position; ttype = TInt },
              Int (Int64.of_int 5) ) ) );
  ]

let suite = List.flatten [ basic_expr_tests ]
