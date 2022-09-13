open Ast
open Util

exception TypingError of (Lexing.position * string)

type id = string

(*type typ = Var of id | Int | Bool | Fun of typ * typ | Subst of typ ref*)
type checked = Resolved | Unresolved of types
type env = (id * types) list
type constructor = string
type constructors = (constructor * (types * types option)) list
(* constructor is the name, first type is the variant type, second type is the container type
   i.e. type example = bruh of Int, constructor is first, int is second, example is first*)

let lookup env key =
  match List.find_opt (fun (k, _) -> k = key) env with
  | Some (_, v) -> Some v
  | None -> None

(* collapse all Subst's except Vars *)

let rec resolve (t : types) : types =
  match t with
  | TInt | TBool | TVar _ | Subst { contents = TVar _ } -> t
  | TFun (t1, t2) -> TFun (resolve t1, resolve t2)
  | Subst ({ contents = t } as r) ->
      r := resolve t;
      !r
  | _ -> failwith "unimplemented"

let rec unify (t1 : types) (t2 : types) : unit =
  match (resolve t1, resolve t2) with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TFun (s1, t1), TFun (s2, t2) ->
      unify s1 s2;
      unify t1 t2
  | Subst { contents = TVar a }, Subst { contents = TVar b } when a = b -> ()
  | Subst ({ contents = TVar a } as r), t
  | t, Subst ({ contents = TVar a } as r) ->
      r := t
  | _ -> failwith "cannot unify"

let fresh =
  let source = Fresh.make (HashSet.make ()) in
  (* fun () -> Subst (ref (TVar (Fresh.next source))) *)
  fun () -> Fresh.next source

let rec label_ast expr (var_env : (id * types) list) (cons : constructors) :
    (env * types) expr_ann =
  let pos, e = expr in
  match e with
  | Unit -> ((var_env, TUnit), Unit)
  | Nil -> ((var_env, TPlaceholder (fresh ())), Nil)
  | Int n -> ((var_env, TInt), Int n)
  | Bool b -> ((var_env, TBool), Bool b)
  | String s -> ((var_env, TString), String s)
  | Char c -> ((var_env, TChar), Char c)
  | Var id -> (
      match lookup var_env id with
      | Some typ -> ((var_env, typ), Var id)
      | None -> failwith "Variable referenced before defined")
  | Tuple lst ->
      ( (var_env, TPlaceholder (fresh ())),
        Tuple (List.map (fun e -> label_ast e var_env cons) lst) )
  | IfThen (b, e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        IfThen
          ( label_ast b var_env cons,
            label_ast e1 var_env cons,
            label_ast e2 var_env cons ) )
  | Let (pat, e1, e2) -> (
      let _, pat' = pat in
      match pat' with
      | PUnit ->
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TUnit), PUnit),
                label_ast e1 var_env cons,
                label_ast e2 var_env cons ) )
      | PWild ->
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TPlaceholder (fresh ())), PWild),
                label_ast e1 var_env cons,
                label_ast e2 var_env cons ) )
      | PBool b ->
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TBool), PBool b),
                label_ast e1 var_env cons,
                label_ast e2 var_env cons ) )
      | PInt n ->
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TInt), PInt n),
                label_ast e1 var_env cons,
                label_ast e2 var_env cons ) )
      | PString s ->
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TString), PString s),
                label_ast e1 var_env cons,
                label_ast e2 var_env cons ) )
      | PVar s ->
          let name = TPlaceholder (fresh ()) in
          let new_env = (s, name) :: List.remove_assoc s var_env in
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, name), PVar s),
                label_ast e1 var_env cons,
                label_ast e2 new_env cons ) )
      | PTup lst ->
          let new_env = label_pat pat var_env cons in
          (* I'm a little unsure about the second fresh variable placeholder, but I think we can take care of it in
             unification *)
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TPlaceholder (fresh ())), PTup lst),
                label_ast e1 var_env cons,
                label_ast e2 new_env cons ) )
      | PSum (con, pat'') ->
          let new_env = label_pat pat var_env cons in
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TPlaceholder (fresh ())), PSum (con, pat'')),
                label_ast e1 var_env cons,
                label_ast e2 new_env cons ) )
      | PNil ->
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TPlaceholder (fresh ())), PNil),
                label_ast e1 var_env cons,
                label_ast e2 var_env cons ) )
      | PCons (pat1, pat2) ->
          let new_env = label_pat pat var_env cons in
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ((var_env, TPlaceholder (fresh ())), PCons (pat1, pat2)),
                label_ast e1 var_env cons,
                label_ast e2 new_env cons ) ))
  | LetRec (lst, exp) ->
      let cum_env =
        List.fold_right (fun (a, b) acc -> label_pat a acc cons) lst var_env
      in
      let new_lst =
        List.map (fun (a, b) -> (a, label_ast b cum_env cons)) lst
      in
      ( (cum_env, TPlaceholder (fresh ())),
        LetRec (new_lst, label_ast exp cum_env cons) )
  | MatchWithWhen (exp, lst) ->
      let new_exp = label_ast exp var_env cons in
      let new_lst =
        List.map
          (fun (a, b, c) ->
            match b with
            | None ->
                let new_env = label_pat c var_env cons in
                (label_ast a new_env cons, b, c)
            | Some e ->
                let new_env = label_pat c var_env cons in
                (label_ast a new_env cons, Some (label_ast e new_env cons), c))
          lst
      in
      ((var_env, TPlaceholder (fresh ())), MatchWithWhen (new_exp, new_lst))
  | Fun (pat, exp) ->
      let new_env = label_pat pat var_env cons in
      ((var_env, TPlaceholder (fresh ())), Fun (pat, label_ast exp new_env cons))
  | App (e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        App (label_ast e1 var_env cons, label_ast e2 var_env cons) )
  | Binop (op, e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        Binop (op, label_ast e1 var_env cons, label_ast e2 var_env cons) )
  | Unaop (op, e) ->
      ((var_env, TPlaceholder (fresh ())), Unaop (op, label_ast e var_env cons))
  | Cons (e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        Cons (label_ast e1 var_env cons, label_ast e2 var_env cons) )
  | Constructor (str, exp) -> (
      match List.assoc_opt str cons with
      | None -> failwith "Constructor doesn't exist"
      | Some t ->
          ((var_env, fst t), Constructor (str, label_ast exp var_env cons)))
  | Record lst ->
      ( (var_env, TPlaceholder (fresh ())),
        Record (List.map (fun (a, b) -> (a, label_ast b var_env cons)) lst) )

and label_pat pat var_env cons =
  let _, pat' = pat in
  match pat' with
  | PUnit -> var_env
  | PWild -> var_env
  | PBool b -> var_env
  | PInt n -> var_env
  | PString s -> var_env
  | PVar s ->
      let name = TPlaceholder (fresh ()) in
      let new_env = (s, name) :: List.remove_assoc s var_env in
      new_env
  | PTup lst ->
      List.fold_left (fun env tern -> label_pat tern env cons) var_env lst
  | PSum (con, pat'') -> label_pat pat'' var_env cons
  | PNil -> var_env
  | PCons (pat1, pat2) ->
      List.fold_left
        (fun env tern -> label_pat tern env cons)
        var_env [ pat1; pat2 ]

and pat_labler pat var_env cons =
  let _, pat' = pat in
  match pat' with
  | PUnit -> TUnit
  | PWild -> TPlaceholder (fresh ())
  | PBool b -> TBool
  | PInt n -> TInt
  | PString s -> TString
  | PVar s -> var_env s
  | PTup lst -> TProd (List.map (fun p -> pat_labler p var_env cons) lst)
  | PSum (con, pat'') ->
      let var_typ = List.assoc con cons in
      fst var_typ
  | PNil -> TPlaceholder (fresh ())
  | PCons (pat1, pat2) -> TCons (pat_labler pat var_env cons)

(* Generates set of constraints for substitution for unification. Substitution substitutes first element out for the second *)

(*TODO for Chris - make sure all of the constraints are being generated under the right environments*)

let rec generate_constraints (exp : (env * types) expr_ann) var_env cons :
    (types * types) list =
  let reuse_root e = generate_constraints e var_env cons in
  match exp with
  | (_, TUnit), Unit -> []
  | (_, TPlaceholder _), Nil -> []
  | (_, TInt), Int n -> []
  | (_, TBool), Bool b -> []
  | (_, TString), String s -> []
  | (_, TChar), Char c -> []
  | (_, typ), Var id -> []
  | (lenv, typ), Tuple lst ->
      let lst_cons =
        List.fold_right
          (fun e acc -> generate_constraints e lenv cons @ acc)
          lst []
      in
      let binding =
        List.fold_right (fun ((lenv2, typ), exp) acc -> typ :: acc) lst []
      in
      (typ, TProd binding) :: lst_cons
  | ( (lenv1, typ),
      IfThen (((lenv2, btyp), b), ((lenv3, typ1), e1), ((lenv4, typ2), e2)) ) ->
      (typ, typ1) :: (typ2, typ1) :: (btyp, TBool)
      :: reuse_root ((lenv2, btyp), b)
      @ reuse_root ((lenv3, typ1), e1)
      @ reuse_root ((lenv4, typ2), e2)
  | ( (lenv, typ),
      Let (((lenvp, typp), pat), ((lenv1, typ1), e1), ((lenv2, typ2), e2)) ) ->
      (typp, typ1) :: (typ, typ2)
      :: generate_constraints ((lenv1, typ1), e1) var_env cons
      @ generate_constraints ((lenv2, typ2), e2) lenv2 cons
  | (lenv, typ), LetRec (lst, ((lenv2, typ2), e)) ->
      let cum_cons =
        List.fold_right
          (fun (pats, e') acc -> generate_constraints e' lenv cons @ acc)
          lst []
      in
      ((typ, typ2) :: cum_cons)
      @ generate_constraints ((lenv2, typ2), e) lenv cons
  | (lenv, typ), MatchWithWhen (e, lst) ->
      let fold_helper (((lenv1, typ1), out), b, c) acc =
        match b with
        | None ->
            ((typ1, typ) :: generate_constraints ((lenv1, typ1), out) lenv1 cons)
            @ acc
        | Some ((lenvb, typb), b) ->
            (typb, TBool) :: (typ1, typ)
            :: generate_constraints ((lenv1, typ1), out) lenv1 cons
            @ generate_constraints ((lenvb, typb), b) lenvb cons
            @ acc
      in
      let lst_cons = List.fold_right fold_helper lst [] in
      lst_cons @ generate_constraints e lenv cons
  | (lenv, typ), Fun (((lenvp, typp), pat), ((lenv2, typ2), e2)) ->
      (typ, TFun (typp, typ2))
      :: generate_constraints ((lenv2, typ2), e2) lenv2 cons
  | (lenv, typ), App (((lenv1, typ1), e1), ((lenv2, typ2), e2)) ->
      (typ1, TFun (typ2, typ))
      :: generate_constraints ((lenv1, typ1), e1) lenv1 cons
      @ generate_constraints ((lenv2, typ2), e2) lenv2 cons
  | _ -> failwith "ah fuck"

let rec type_expr expr var_env typ_env =
  let pos, e = expr in
  match e with
  | Unit -> TUnit
  | Nil -> TCons (TPlaceholder (fresh ()))
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Char _ -> TChar
  | Tuple tup ->
      let tup_types = List.map (fun elt -> type_expr elt var_env typ_env) tup in
      TProd tup_types
  | Var id -> (
      match lookup var_env id with
      | Some typ -> typ
      | None -> failwith "Need fresh variable using TyVarVar here")
  | Binop (bop, e1, e2) -> type_bop bop e1 e2 var_env typ_env
  | Unaop (unop, e) -> type_unop unop e var_env typ_env
  | IfThen (e1, e2, e3) ->
      let t1 = type_expr e1 var_env typ_env in
      let t2 = type_expr e2 var_env typ_env in
      let t3 = type_expr e3 var_env typ_env in
      let _ = unify t1 TBool in
      let _ = unify t2 t3 in
      t2
  | App (e1, e2) ->
      let t1 = type_expr e1 var_env typ_env in
      let t2 = type_expr e2 var_env typ_env in
      let fr = TPlaceholder (fresh ()) in
      let _ = unify t1 (TFun (t2, fr)) in
      fr
  | _ -> failwith "unimplemented"

and type_bop bop e1 e2 var_env typ_env =
  let t1 = type_expr e1 var_env typ_env in
  let t2 = type_expr e2 var_env typ_env in
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
      let fr = TPlaceholder (fresh ()) in
      let _ = unify t2 (TFun (t1, fr)) in
      fr
  | ConsBop ->
      let _ = unify t2 (TCons t1) in
      TCons t1

and type_unop unop e var_env typ_env =
  let t = type_expr e var_env typ_env in
  match unop with
  | Not ->
      let _ = unify t TBool in
      TBool
  | Neg ->
      let _ = unify t TInt in
      TInt
  | Ref -> TRef t
  | Deref ->
      let fr = TPlaceholder (fresh ()) in
      let _ = unify t (TRef fr) in
      fr

let type_prog parsed_ast = failwith "unimplemented"