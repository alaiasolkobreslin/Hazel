open Ast

exception TypingError of (Lexing.position * string)

type id = string
(*type typ = Var of id | Int | Bool | Fun of typ * typ | Subst of typ ref*)
type checked = Resolved | Unresolved of types

type schema = id list * types
type env = (id * schema) list
type constructor = string
type constructors = (constructor * (types option * string)) list 

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

let fresh_var = failwith "unimplemented"

let rec label_ast (expr) (var_env : (id * types) list) (cons : constructors) : types expr_ann = 
  let (pos, e) = expr in
  match e with
  | Unit -> (TUnit, Unit)
  | Nil -> (TPlaceholder fresh_var, Nil)
  | Int n -> (TInt, Int n)
  | Bool b -> (TBool, Bool b)
  | String s -> (TString, String s)
  | Char c -> (TChar, Char c)
  | Var id ->
    begin
      match lookup var_env id with
      | Some typ -> (typ, Var id)
      | None -> failwith "Variable referenced before defined"
    end
  | Tuple [e1; e2] -> (TPlaceholder fresh_var, Tuple [label_ast e1 var_env cons; label_ast e2 var_env cons])
  | IfThen (b, e1, e2) -> (TPlaceholder fresh_var, IfThen (label_ast b var_env cons, label_ast e1 var_env cons, label_ast e2 var_env cons))
  | Let (pat, e1, e2) -> 
    let (_, pat') = pat in
    begin
      match pat' with
      | PUnit -> TPlaceholder fresh_var, Let((TUnit, PUnit), label_ast e1 var_env cons, label_ast e2 var_env cons)
      | PWild -> TPlaceholder fresh_var, Let((TPlaceholder fresh_var, PWild), label_ast e1 var_env cons, label_ast e2 var_env cons)
      | PBool b -> TPlaceholder fresh_var, Let((TBool, PBool b), label_ast e1 var_env cons, label_ast e2 var_env cons)
      | PInt n -> TPlaceholder fresh_var, Let((TInt, PInt n), label_ast e1 var_env cons, label_ast e2 var_env cons)
      | PString s -> TPlaceholder fresh_var, Let((TString, PString s), label_ast e1 var_env cons, label_ast e2 var_env cons)
      | PVar s -> 
        let name = TPlaceholder fresh_var in
        let new_env = (s, name)::(List.remove_assoc s var_env) in
        TPlaceholder fresh_var, Let((name, PVar s), label_ast e1 var_env cons, label_ast e2 new_env cons)
      | PTup lst -> failwith "Honestly it's probably better to move this to a helper, but I'm already started"
      | PSum (cons, pat) -> failwith "Again, go to helper"
      | PNil -> TPlaceholder fresh_var, Let((TPlaceholder fresh_var, PNil), label_ast e1 var_env cons, label_ast e2 var_env cons)
      | PCons (pat1, pat2) -> failwith "again, helper"
    end
  | _ -> failwith "unimplemented"

and label_pats pat var_env cons = 
failwith "unimplemented"
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
      | None -> failwith "Need fresh variable using TyVarVar here"
    end
  | _ -> failwith "unimplemented"

let type_prog parsed_ast = failwith "unimplemented"