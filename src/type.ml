open Ast

exception TypingError of (Lexing.position * string)

type id = string
(*type typ = Var of id | Int | Bool | Fun of typ * typ | Subst of typ ref*)

type schema = id list * types
type env = (id * schema) list

let lookup env key =
  match List.find_opt (fun (k, _) -> k = key) env with
  | Some (_, v) -> Some v
  | None -> None

(* collapse all Subst's except Vars *)
let rec resolve (t : types) : types =
  match t with
  | (TInt | TBool | TVar _ | Subst {contents = TVar _}) -> t
  | TFun (t1, t2) -> TFun (resolve t1, resolve t2)
  | Subst ({contents = t} as r) -> r := resolve t; !r
  | _ -> failwith "unimplemented"

let rec unify (t1 : types) (t2 :types) : unit =
  match (resolve t1, resolve t2) with
  | (TInt, TInt) -> ()
  | (TBool, TBool) -> ()
  | (TFun (s1, t1), TFun (s2, t2)) -> (unify s1 s2; unify t1 t2)
  | (Subst {contents = TVar a}, Subst {contents = TVar b}) when a = b -> ()
  | ((Subst ({contents = TVar a} as r), t) | (t, Subst ({contents = TVar a} as r))) -> r := t
  | _ -> failwith "cannot unify"

let type_expr expr var_env typ_env =
  let (pos, e) = expr in
  match e with
  | Unit -> TUnit
  | Nil -> failwith "unimplemented" (* TCons *)
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Char _ -> TChar
  | Var id ->
    begin
      match lookup var_env id with
      | Some typ -> typ
      | None -> raise (TypingError (pos, "unbound variable " ^ id))
    end
  | _ -> failwith "unimplemented"

let type_prog parsed_ast = failwith "unimplemented"