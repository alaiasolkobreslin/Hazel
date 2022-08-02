open Ast
open Z3.Smtlib

let solver () = make_solver "z3"

let rec expr_to_term e =
  match e with
  | Binop (bop, (_, e1), (_, e2)) -> binop_to_term bop e1 e2
  | Unaop (unop, (_, e1)) -> unop_to_term unop e1
  | _ -> failwith ""

and unop_to_term unop e =
  let t = expr_to_term e in
  match unop with
  | Not -> not_ t
  | Neg -> sub (int_to_term 0) t
  | _ -> failwith "operation not supported"

and binop_to_term bop e1 e2 = 
  let t1 = expr_to_term e1 in 
  let t2 = expr_to_term e2 in
  match bop with
  | Plus -> add t1 t2
  | Minus -> sub t1 t2
  | Mult -> mul t1 t2
  | GT -> gt t1 t2
  | LT -> lt t1 t2
  | GEQ -> gte t1 t2
  | LEQ -> lte t1 t2
  | EQ -> equals t1 t2
  | NEQ -> equals t1 t2 |> not_
  | And -> and_ t1 t2
  | Or -> or_ t1 t2
  | _ -> failwith "operation not supported"

let is_subtype t1 t2 = failwith "unimplemented"

let typecheck_refinement args = failwith "unimplemented"