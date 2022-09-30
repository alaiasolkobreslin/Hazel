open Lexing
open Hazel.Ast
open Hazel.Type
open OUnit2

let default_position =
  { pos_fname = "default position"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }

let default_env = []

let make_type_expr_test (name : string) (expr : parsed expr_ann)
    (expected_output : types) : test =
  name >:: fun _ -> assert_equal expected_output (type_expr expr)

let make_type_expr_raises_exn_test (name : string) (expr : parsed expr_ann)
    (env : (constructor * types) list) (msg : string) : test =
  name >:: fun _ ->
  assert_raises
    (TypingError ((fst expr).parsed_pos, msg))
    (fun () -> type_expr expr)

let basic_expr_tests =
  [
    make_type_expr_test "true is bool"
      ({ parsed_pos = default_position; ptype = None }, Bool true)
      TBool;
    make_type_expr_test "true is bool"
      ({ parsed_pos = default_position; ptype = None }, Bool false)
      TBool;
    make_type_expr_test "not true is bool"
      ( { parsed_pos = default_position; ptype = None },
        Unaop (Not, ({ parsed_pos = default_position; ptype = None }, Bool true))
      )
      TBool;
    make_type_expr_test "5 is int"
      ({ parsed_pos = default_position; ptype = None }, Int (Int64.of_int 5))
      TInt;
    make_type_expr_test "-5 is int"
      ( { parsed_pos = default_position; ptype = None },
        Unaop
          ( Neg,
            ( { parsed_pos = default_position; ptype = None },
              Int (Int64.of_int 5) ) ) )
      TInt;
    make_type_expr_test "1 + 1 is int"
      ( { parsed_pos = default_position; ptype = None },
        Binop
          ( Plus,
            ( { parsed_pos = default_position; ptype = None },
              Int (Int64.of_int 1) ),
            ( { parsed_pos = default_position; ptype = None },
              Int (Int64.of_int 1) ) ) )
      TInt;
    make_type_expr_test "0 < 1 is bool"
      ( { parsed_pos = default_position; ptype = None },
        Binop
          ( LT,
            ( { parsed_pos = default_position; ptype = None },
              Int (Int64.of_int 0) ),
            ( { parsed_pos = default_position; ptype = None },
              Int (Int64.of_int 1) ) ) )
      TBool;
    make_type_expr_test "true and true is bool"
      ( { parsed_pos = default_position; ptype = None },
        Binop
          ( And,
            ({ parsed_pos = default_position; ptype = None }, Bool true),
            ({ parsed_pos = default_position; ptype = None }, Bool true) ) )
      TBool;
    make_type_expr_test "unit applied to fn is unit"
      ( { parsed_pos = default_position; ptype = None },
        App
          ( ( { parsed_pos = default_position; ptype = None },
              Fun
                ( ({ parsed_pos = default_position; ptype = None }, PUnit),
                  ({ parsed_pos = default_position; ptype = None }, Unit) ) ),
            ({ parsed_pos = default_position; ptype = None }, Unit) ) )
      TUnit;
  ]

let expr_exn_tests =
  [ (* make_typed_expr_raises_exn_test "false + 1 raises exn"
         ( { parsed_pos = default_position; ptype = None },
           Binop
             ( Plus,
               ({ parsed_pos = default_position; ptype = None }, Bool false),
               ( { parsed_pos = default_position; ptype = None },
                 Int (Int64.of_int 1) ) ) )
         default_env "Binary operator expects two integers";
       make_typed_expr_raises_exn_test "false < 1 raises exn"
         ( { parsed_pos = default_position; ptype = None },
           Binop
             ( LT,
               ({ parsed_pos = default_position; ptype = None }, Bool false),
               ( { parsed_pos = default_position; ptype = None },
                 Int (Int64.of_int 1) ) ) )
         default_env "Binary operator expects two integers";
       make_typed_expr_raises_exn_test "5; 5 raises exn"
         ( { parsed_pos = default_position; ptype = None },
           Binop
             ( Seq,
               ( { parsed_pos = default_position; ptype = None },
                 Int (Int64.of_int 5) ),
               ( { parsed_pos = default_position; ptype = None },
                 Int (Int64.of_int 5) ) ) )
         default_env "Sequence expects unit type"; *) ]

let suite = List.flatten [ basic_expr_tests; expr_exn_tests ]
