open Ast
open Z3
open Z3.Solver
open Z3.Arithmetic
open Z3.Boolean
open Z3.Symbol

(* let solver () = make_solver "z3" *)

let rec expr_to_term ctx e =
  match e with
  | Binop (bop, (_, e1), (_, e2)) -> binop_to_term ctx bop e1 e2
  | Unaop (unop, (_, e1)) -> unop_to_term ctx unop e1
  | _ -> failwith ""

and unop_to_term ctx unop e =
  let t = expr_to_term ctx e in
  match unop with
  | Not -> mk_not ctx t
  | Neg -> mk_sub ctx [mk_int ctx 0 |> mk_const ctx; t]
  | _ -> failwith "operation not supported"

and binop_to_term ctx bop e1 e2 = 
  let t1 = expr_to_term ctx e1 in 
  let t2 = expr_to_term ctx e2 in
  match bop with
  | Plus -> mk_add ctx [t1; t2]
  | Minus -> mk_sub ctx [t1; t2]
  | Mult -> mk_mul ctx [t1; t2]
  | GT -> mk_gt ctx t1 t2
  | LT -> mk_lt ctx t1 t2
  | GEQ -> mk_ge ctx t1 t2
  | LEQ -> mk_le ctx t1 t2
  | EQ -> mk_eq ctx t1 t2
  | NEQ -> mk_eq ctx t1 t2 |> mk_not ctx
  | And -> mk_and ctx [t1; t2]
  | Or -> mk_or ctx [t1; t2]
  | _ -> failwith "operation not supported"

let (>>=) pat = failwith ""

let rec bind_pats ctx pats exprs =
  match pats, exprs with
  | (_, p)::t1, (_, e)::t2 ->
    begin
      let rhs = bind_pats ctx t1 t2 in
      let lhs = bind_pat ctx p e in
      mk_implies ctx lhs rhs
    end
  | [], [] -> mk_true ctx
  | _ -> failwith "patterns don't match"

and bind_pat ctx pat e =
  match pat, e with
  | (PVar v, Bool b) ->
    begin
      let b' = if b then mk_true ctx else mk_false ctx in
      let v' = mk_const ctx (mk_string ctx v) in
      mk_eq ctx v' b'
    end
  | (PVar v, Int i) ->
    begin
      let i' = mk_int ctx (Int64.to_int i) |> mk_const ctx in
      let v' = mk_const ctx (mk_string ctx v) in
      mk_eq ctx v' i'
    end
  | (PTup pats, Tuple exprs) -> bind_pats ctx pats exprs
  | _ -> failwith "unimplemented"

(* let let_defn_to_term ctx defn = match defn with
  | (_, pat, e) ->
    begin
      match (pat, e) with
      | 
    end *)

let is_subtype t1 t2 = failwith "unimplemented"

let typecheck_refinement args = 
  let solver = mk_context [] |> fun env -> mk_solver env None in
  failwith "unimplemented"