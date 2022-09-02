open Ast
open Z3
open Z3.Solver
open Z3.Arithmetic
open Z3.Boolean
open Z3.Symbol
open Z3.Z3List

exception RefinementError of (Lexing.position * string)

let get_pos e = (fst e).parsed_pos

let rec expr_to_term ctx e =
  match snd e with
  | Binop (bop, e1, e2) -> binop_to_term ctx bop (fst e) e1 e2
  | Unaop (unop, e1) -> unop_to_term ctx unop e1
  | Bool b -> if b then mk_true ctx else mk_false ctx
  | Int i -> mk_const ctx (mk_int ctx (Int64.to_int i))
  | Var s -> mk_const ctx (mk_string ctx s)
  | Nil -> failwith "unimplemented" (* nil ... *)
  | App (e1, e2) -> failwith "unimplemented" (* mk_app ... *)
  | _ -> failwith "unimplemented"

and unop_to_term ctx unop e =
  let t = expr_to_term ctx e in
  match unop with
  | Not -> mk_not ctx t
  | Neg -> mk_sub ctx [ mk_int ctx 0 |> mk_const ctx; t ]
  | Ref ->
      raise (RefinementError (get_pos e, "Reference operation not supported"))
  | Deref ->
      raise (RefinementError (get_pos e, "Dereference operation not supported"))

and binop_to_term ctx bop pos (e1 : 'a expr_ann) (e2 : 'a expr_ann) =
  let t1 = expr_to_term ctx e1 in
  let t2 = expr_to_term ctx e2 in
  match bop with
  | Plus -> mk_add ctx [ t1; t2 ]
  | Minus -> mk_sub ctx [ t1; t2 ]
  | Mult -> mk_mul ctx [ t1; t2 ]
  | GT -> mk_gt ctx t1 t2
  | LT -> mk_lt ctx t1 t2
  | GEQ -> mk_ge ctx t1 t2
  | LEQ -> mk_le ctx t1 t2
  | EQ -> mk_eq ctx t1 t2
  | NEQ -> mk_eq ctx t1 t2 |> mk_not ctx
  | And -> mk_and ctx [ t1; t2 ]
  | Or -> mk_or ctx [ t1; t2 ]
  | _ ->
      raise (RefinementError (pos.parsed_pos, "Binary operation not supported"))

let rec bind_pats ctx pos pats exprs =
  match (pats, exprs) with
  | (_, p) :: t1, e :: t2 ->
      let rhs = bind_pats ctx pos t1 t2 in
      let lhs = bind_pat ctx p e in
      mk_implies ctx lhs rhs
  | [], [] -> mk_true ctx
  | _ -> raise (RefinementError (pos.parsed_pos, "Patterns don't match"))

and bind_pat ctx pat e =
  match (pat, snd e) with
  | PVar v, Bool b ->
      let b' = if b then mk_true ctx else mk_false ctx in
      let v' = mk_const ctx (mk_string ctx v) in
      mk_eq ctx v' b'
  | PVar v, Int i ->
      let i' = mk_int ctx (Int64.to_int i) |> mk_const ctx in
      let v' = mk_const ctx (mk_string ctx v) in
      mk_eq ctx v' i'
  | PTup pats, Tuple exprs -> bind_pats ctx (fst e) pats exprs
  | _ -> failwith "unimplemented"

let let_defn_to_term ctx defn =
  match defn with _, pat, e -> bind_pat ctx pat e

let prog_to_term (ctx : context) (prog : 'a prog) : Expr.expr =
  let start : Expr.expr = failwith "unimplemented" in
  match prog with _, _, _, defns, expr -> failwith "unimplemented"
(* List.fold_right
   (fun acc defn -> mk_implies ctx (let_defn_to_term ctx defn) acc)
   defns start *)

let is_subtype solver t1 t2 = failwith "unimplemented"

let typecheck_refinement args =
  let solver = mk_context [] |> fun env -> mk_solver env None in
  failwith "unimplemented"