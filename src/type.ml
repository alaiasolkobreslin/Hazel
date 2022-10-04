open Ast
open Util

exception TypingError of (Lexing.position * string)

type id = string
type checked = Resolved | Unresolved of types
type schema = id list * types
type env = (id * schema) list
type constructor = string
type constructors = (constructor * (types * types option)) list
(* constructor is the name, first type is the variant type, second type is the container type
   i.e. type example = bruh of Int, constructor is first, int is second, example is first*)

let rec lookup (x : id) (gamma : env) =
  try List.assoc x gamma with Not_found -> failwith "lookup error"

(* collapse all Subst's *)
let rec collapse (t : types) : types =
  match t with
  | TInt | TBool | TVar _ | TPlaceholder _ | TString | TChar | TUnit
  | TConstraint _ ->
      t
  | TFun (t1, t2) -> TFun (collapse t1, collapse t2)
  | TProd lst -> TProd (List.map collapse lst)
  | TCons typ -> TCons (collapse typ)
  | TRef typ -> TRef (collapse typ)
  | TRecord lst -> TRecord (List.map (fun (s, typ) -> (s, collapse typ)) lst)
  | TSum lst ->
      TSum
        (List.map
           (fun (s, typ_opt) ->
             match typ_opt with
             | Some typ -> (s, Some (collapse typ))
             | None -> (s, None))
           lst)
  | Subst { contents = t } -> collapse t

(* collapse all Subst's except Vars *)
let rec resolve (t : types) : types =
  match t with
  | TInt | TBool | TVar _
  | Subst { contents = TVar _ }
  | TPlaceholder _ | TString | TChar | TUnit | TConstraint _ ->
      t
  | TFun (t1, t2) -> TFun (resolve t1, resolve t2)
  | TProd lst -> TProd (List.map resolve lst)
  | TCons typ -> TCons (resolve typ)
  | TRef typ -> TRef (resolve typ)
  | TRecord lst -> TRecord (List.map (fun (s, typ) -> (s, resolve typ)) lst)
  | TSum lst ->
      TSum
        (List.map
           (fun (s, typ_opt) ->
             match typ_opt with
             | Some typ -> (s, Some (resolve typ))
             | None -> (s, None))
           lst)
  | Subst ({ contents = t } as r) ->
      r := resolve t;
      !r

let rec unify (t1 : types) (t2 : types) : unit =
  print_endline "got here";
  match (resolve t1, resolve t2) with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TUnit, TUnit -> ()
  | TChar, TChar -> ()
  | TString, TString -> ()
  | TProd t1, TProd t2 -> List.iter2 unify t1 t2
  | TCons t1, TCons t2 -> unify t1 t2
  | TRef t1, TRef t2 -> unify t1 t2
  | TFun (s1, t1), TFun (s2, t2) ->
      unify s1 s2;
      unify t1 t2
  | Subst { contents = TVar a }, Subst { contents = TVar b } when a = b -> ()
  | Subst ({ contents = TVar a } as r), t
  | t, Subst ({ contents = TVar a } as r) ->
      r := t
  (* TODO: finish rest of cases *)
  | _ -> failwith "cannot unify"

let fresh =
  let source = Fresh.make (HashSet.make ()) in
  fun () -> Subst (ref (TVar (Fresh.next source)))

(* instantiate a schema with fresh variables *)
let instantiate ((vars, t) : schema) : types =
  (* simultaneous substitution *)
  let rec subst (s : (id * types) list) (t : types) : types =
    match t with
    | TInt -> TInt
    | TBool -> TBool
    | TChar -> TChar
    | TString -> TString
    | TUnit -> TUnit
    | TProd typs -> TProd (List.map (subst s) typs)
    | TCons typ -> TCons (subst s typ)
    | TRef typ -> TRef (subst s typ)
    | TPlaceholder str -> TPlaceholder str
    (* I think these next two are correct? Double check *)
    | TVar str -> TVar str
    | TConstraint x -> TConstraint x
    | TSum typs ->
        TSum
          (List.map
             (fun (str, typ) ->
               match typ with
               | Some typ' -> (str, Some (subst s typ'))
               | None -> (str, None))
             typs)
    | TRecord typs ->
        TRecord (List.map (fun (str, typ) -> (str, subst s typ)) typs)
    | TFun (t1, t2) -> TFun (subst s t1, subst s t2)
    | Subst { contents = TVar a } -> (
        try Subst (ref (List.assoc a s)) with Not_found -> t)
    | Subst { contents = t } -> subst s t
  in
  let new_vars = List.fold_left (fun v _ -> fresh () :: v) [] vars in
  subst (List.combine vars new_vars) t

let rec type_expr (expression : parsed expr_ann) : types =
  let rec check (gamma : env) (expr : parsed expr_ann) : types =
    let pos, e = expr in
    match e with
    | Unit -> TUnit
    | Nil -> TCons (fresh ())
    | Int _ -> TInt
    | Bool _ -> TBool
    | String _ -> TString
    | Char _ -> TChar
    | Tuple tup ->
        let tup_types = List.map (check gamma) tup in
        TProd tup_types
    | Var id -> instantiate (lookup id gamma)
    | Binop (bop, e1, e2) -> check_bop gamma bop e1 e2
    | Unaop (unop, e1) -> check_unop gamma unop e1
    | IfThen (e1, e2, e3) ->
        let t1 = check gamma e1 in
        let t2 = check gamma e2 in
        let t3 = check gamma e3 in
        let _ = unify t1 TBool in
        let _ = unify t2 t3 in
        t2
    | App (e1, e2) ->
        let t1 = check gamma e1 in
        let t2 = check gamma e2 in
        let fr = fresh () in
        let _ = unify t1 (TFun (t2, fr)) in
        fr
    | Cons (e1, e2) ->
        let t1 = check gamma e1 in
        let t2 = check gamma e2 in
        let _ = unify t2 (TCons t1) in
        TCons t1
    | MatchWithWhen (e, lst) ->
        let t1 = check gamma e in
        let fr = fresh () in
        check_match_with_when gamma t1 fr lst
    | Fun (pat, e1) -> failwith "unimplemented"
    | Let (pat, e1, e2) -> failwith "unimplemented"
    | LetRec (lst, e1) -> failwith "unimplemented"
    | Constructor (str, e1) -> failwith "unimplemented"
    | Record lst -> failwith "unimplemented"
  and check_bop (gamma : env) bop e1 e2 =
    let t1 = check gamma e1 in
    let t2 = check gamma e2 in
    match bop with
    | Plus | Minus | Mult | Div | Mod | HMult ->
        let _ = unify t1 TInt in
        let _ = unify t2 TInt in
        TInt
    | GT | GEQ | LT | LEQ ->
        let _ = unify t1 TInt in
        let _ = unify t2 TInt in
        TBool
    | And | Or ->
        let _ = unify t1 TBool in
        let _ = unify t2 TBool in
        TBool
    | Seq ->
        let _ = unify t1 TUnit in
        t2
    | Cat ->
        let _ = unify t1 TString in
        let _ = unify t2 TString in
        TString
    | Ass ->
        let _ = unify t1 (TRef t2) in
        TUnit
    | EQ | NEQ | PEQ | PNEQ ->
        let _ = unify t1 t2 in
        TBool
    | Pipe ->
        let fr = fresh () in
        let _ = unify t2 (TFun (t1, fr)) in
        fr
    | ConsBop ->
        let _ = unify t2 (TCons t1) in
        TCons t1
  and check_unop gamma unop e =
    let t = check gamma e in
    match unop with
    | Not ->
        let _ = unify t TBool in
        TBool
    | Neg ->
        let _ = unify t TInt in
        TInt
    | Ref -> TRef t
    | Deref ->
        let fr = fresh () in
        let _ = unify t (TRef fr) in
        fr
  and check_pat pat =
    match pat with
    | PUnit -> TUnit
    | PWild -> fresh ()
    | PBool _ -> TBool
    | PString _ -> TString
    | PInt _ -> TInt
    | PVar x -> fresh ()
    | PTup lst -> TProd (List.map (fun (_, x) -> check_pat x) lst)
    | PNil -> TCons (fresh ())
    | PCons ((_, pat1), (_, pat2)) ->
        let t1 = check_pat pat1 in
        let t2 = check_pat pat2 in
        let _ = unify (TCons t1) t2 in
        t2
    | PSum (str, pat) -> failwith "unimplemented"
  and check_match_with_when gamma e_typ ret_typ lst =
    match lst with
    | [] -> ret_typ
    | (e, e_opt, (_, pat)) :: tl ->
        let t = check gamma e in
        let pat_t = check_pat pat in
        let _ = unify pat_t e_typ in
        let _ = unify ret_typ t in
        check_match_with_when gamma e_typ ret_typ tl
  in

  collapse (check [] expression)

let type_prog parsed_ast = failwith "unimplemented"