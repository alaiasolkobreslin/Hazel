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

let rec label_ast (expr : 'a expr_ann) (var_env : (id * types) list)
    (cons : constructors) : (env * types) expr_ann =
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
              ( annotate_pat pat new_env cons,
                label_ast e1 var_env cons,
                label_ast e2 new_env cons ) )
      | PSum (con, pat'') ->
          let new_env = label_pat pat var_env cons in
          ( (var_env, TPlaceholder (fresh ())),
            Let
              ( ( (var_env, TPlaceholder (fresh ())),
                  PSum (con, annotate_pat pat'' new_env cons) ),
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
              ( ( (var_env, TPlaceholder (fresh ())),
                  PCons
                    ( annotate_pat pat1 new_env cons,
                      annotate_pat pat2 new_env cons ) ),
                label_ast e1 var_env cons,
                label_ast e2 new_env cons ) ))
  | LetRec (lst, exp) ->
      let cum_env =
        List.fold_right (fun (a, b) acc -> label_pat a acc cons) lst var_env
      in
      let new_lst =
        List.map
          (fun (a, b) ->
            (annotate_pat a cum_env cons, label_ast b cum_env cons))
          lst
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
                (label_ast a new_env cons, None, annotate_pat c new_env cons)
            | Some e ->
                let new_env = label_pat c var_env cons in
                ( label_ast a new_env cons,
                  Some (label_ast e new_env cons),
                  annotate_pat c new_env cons ))
          lst
      in
      ((var_env, TPlaceholder (fresh ())), MatchWithWhen (new_exp, new_lst))
  | Fun (pat, exp) ->
      let new_env = label_pat pat var_env cons in
      ( (var_env, TPlaceholder (fresh ())),
        Fun (annotate_pat pat new_env cons, label_ast exp new_env cons) )
  | App (e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        App (label_ast e1 var_env cons, label_ast e2 var_env cons) )
  | Binop (op, e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        Binop (op, label_ast e1 var_env cons, label_ast e2 var_env cons) )
  | Unaop (op, e) ->
      ((var_env, TPlaceholder (fresh ())), Unaop (op, label_ast e var_env cons))
  (* | Cons (e1, e2) ->
      ( (var_env, TPlaceholder (fresh ())),
        Cons (label_ast e1 var_env cons, label_ast e2 var_env cons) ) *)
  | Constructor (str, exp) -> (
      match List.assoc_opt str cons with
      | None -> failwith "Constructor doesn't exist"
      | Some t ->
          ((var_env, fst t), Constructor (str, label_ast exp var_env cons)))
  | Record lst ->
      ( (var_env, TPlaceholder (fresh ())),
        Record (List.map (fun (a, b) -> (a, label_ast b var_env cons)) lst) )

and annotate_pat pat var_env cons =
  let _, pat' = pat in
  match pat' with
  | PUnit -> ((var_env, TUnit), PUnit)
  | PWild -> ((var_env, TPlaceholder (fresh ())), PWild)
  | PBool b -> ((var_env, TBool), PBool b)
  | PInt n -> ((var_env, TInt), PInt n)
  | PString s -> ((var_env, TString), PString s)
  | PVar s -> (
      match List.assoc_opt s var_env with
      | None -> ((var_env, TPlaceholder (fresh ())), PVar s)
      | Some ty -> ((var_env, ty), PVar s))
  | PTup lst ->
      ( (var_env, TPlaceholder (fresh ())),
        PTup (List.map (fun p -> annotate_pat p var_env cons) lst) )
  | PSum (con, pat'') ->
      ( (var_env, TPlaceholder (fresh ())),
        PSum (con, annotate_pat pat'' var_env cons) )
  | PNil -> ((var_env, TCons (TPlaceholder (fresh ()))), PNil)
  | PCons (pat1, pat2) ->
      ( (var_env, TPlaceholder (fresh ())),
        PCons (annotate_pat pat1 var_env cons, annotate_pat pat2 var_env cons)
      )

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
  | (lenv, typ), Binop (bop, e1, e2) -> bop_helper typ bop e1 e2 lenv cons
  | (lenv, typ), Unaop (op, e) -> unaop_helper typ op e lenv cons
  | (lenv, typ), Constructor (iden, ((lenv1, typ1), e1)) -> (
      match List.assoc_opt iden cons with
      | None -> failwith "Constructor not found"
      | Some (t1, Some t2) ->
          (typ, t1) :: (typ1, t2)
          :: generate_constraints ((lenv1, typ1), e1) lenv1 cons
      | Some (t1, None) -> failwith "We need to fix the AST")
  | (lenv, typ), Record lst ->
      let cons_lst =
        List.map
          (fun (c, ((lenv, typ), e)) ->
            (c, typ, generate_constraints ((lenv, typ), e) lenv cons))
          lst
      in
      let reco, cum_cons =
        List.fold_left
          (fun (reco, con) (c, typ, cons) -> ((c, typ) :: reco, cons @ con))
          ([], []) cons_lst
      in
      (typ, TRecord reco) :: cum_cons
  | _ -> failwith "ah fuck"

and unaop_helper tvar uop e var_env cons =
  let (lenv, typ), e' = e in
  match uop with
  | Not -> (tvar, TBool) :: (typ, TBool) :: generate_constraints e lenv cons
  | Neg -> (tvar, TInt) :: (typ, TInt) :: generate_constraints e lenv cons
  | Ref -> (tvar, TRef typ) :: generate_constraints e lenv cons
  | Deref -> (typ, TRef tvar) :: generate_constraints e lenv cons

and bop_helper tvar bop e1 e2 var_env cons =
  let (lenv1, typ1), e1' = e1 in
  let (lenv2, typ2), e2' = e2 in
  match bop with
  | Plus | Minus | Mult | Div | Mod | HMult ->
      (tvar, TInt) :: (typ1, TInt) :: (typ2, TInt)
      :: generate_constraints e1 lenv1 cons
      @ generate_constraints e2 lenv2 cons
  | GT | GEQ | LT | LEQ ->
      (tvar, TBool) :: (typ1, TInt) :: (typ2, TInt)
      :: generate_constraints e1 lenv1 cons
      @ generate_constraints e2 lenv2 cons
  | And | Or ->
      (tvar, TBool) :: (typ1, TBool) :: (typ2, TBool)
      :: generate_constraints e1 lenv1 cons
      @ generate_constraints e2 lenv2 cons
  | Seq ->
      ((tvar, typ2) :: (typ1, TUnit) :: generate_constraints e1 lenv1 cons)
      @ generate_constraints e2 lenv2 cons
  | Cat ->
      (tvar, TString) :: (typ1, TString) :: (typ2, TString)
      :: generate_constraints e1 lenv1 cons
      @ generate_constraints e2 lenv2 cons
  | Ass ->
      ((tvar, TUnit) :: (typ1, TRef typ2) :: generate_constraints e1 lenv1 cons)
      @ generate_constraints e2 lenv2 cons
  | EQ | NEQ | PEQ | PNEQ ->
      ((tvar, TBool) :: (typ1, typ2) :: generate_constraints e1 lenv1 cons)
      @ generate_constraints e2 lenv2 cons
  | Pipe ->
      ((typ1, TFun (typ2, tvar)) :: generate_constraints e1 lenv1 cons)
      @ generate_constraints e2 lenv2 cons
  | ConsBop ->
      ((typ2, TCons typ1) :: generate_constraints e1 lenv1 cons)
      @ generate_constraints e2 lenv2 cons

(* Returns true iff variable x appears in typ*)
let rec contains_var x typ =
  match typ with
  | TPlaceholder y -> y = x
  | TBool -> false
  | TInt -> false
  | TString -> false
  | TChar -> false
  | TProd lst -> List.fold_left (fun b ty -> contains_var x typ || b) false lst
  | TSum lst ->
      List.fold_left
        (fun b (con, opt) ->
          match opt with None -> b | Some typ' -> b || contains_var x typ')
        false lst
      (* variants - first string removed so we can combine with alias *)
  | TCons ty -> contains_var x ty
  | TUnit -> false
  | TRef ty -> contains_var x ty
  | TRecord lst ->
      List.fold_left (fun b (con, typ') -> b || contains_var x typ') false lst
  | TVar y -> y = x
  | TConstraint _ -> false
  | TFun (ty1, ty2) -> contains_var x ty1 || contains_var x ty2
  | Subst _ -> failwith "???"

let rec subst_type typ substout substin =
  match substout with
  | TVar s | TPlaceholder s -> (
      match typ with
      | TVar x when x = s -> substin
      | TPlaceholder x when x = s -> substin
      | TBool -> TBool
      | TInt -> TInt
      | TString -> TString
      | TChar -> TChar
      | TProd lst ->
          TProd (List.map (fun x -> subst_type x substout substin) lst)
      | TSum lst ->
          TSum
            (List.map
               (fun (x, y) ->
                 match y with
                 | None -> (x, None)
                 | Some t -> (x, Some (subst_type t substout substin)))
               lst)
      | TCons ty -> TCons (subst_type ty substout substin)
      | TUnit -> TUnit
      | TRef ty -> TRef (subst_type ty substout substin)
      | TRecord lst ->
          TRecord
            (List.map (fun (x, y) -> (x, subst_type y substout substin)) lst)
      | TVar y -> TVar y
      | TConstraint t -> TConstraint t
      | TFun (ty1, ty2) ->
          TFun (subst_type ty1 substout substin, subst_type ty2 substout substin)
      | _ -> failwith "invalid substitution")
  | _ -> failwith "invalid substitution"

let rec unify_constraints lst =
  let remove_id =
    List.filter_map (fun (x, y) -> if x = y then None else Some (x, y)) lst
  in
  let find_help (x, y) =
    match x with
    | TVar s | TPlaceholder s -> not (contains_var s y)
    | _ -> false
  in
  let find_subst = List.find_opt find_help remove_id in
  match find_subst with
  | Some (x, y) ->
      let new_lst =
        List.map
          (fun (t1, t2) -> (subst_type t1 x y, subst_type t2 x y))
          remove_id
      in
      (x, y) :: unify_constraints new_lst
  | None -> (
      let find_help_func x =
        match x with TFun (a, b), TFun (a', b') -> true | _ -> false
      in
      match List.find_opt find_help_func remove_id with
      | Some (TFun (a, b), TFun (a', b')) ->
          let removed =
            List.filter_map
              (fun (x, y) ->
                if (x, y) = (TFun (a, b), TFun (a', b')) then None
                else Some (x, y))
              remove_id
          in
          (a, a') :: (b, b') :: removed
      | _ -> remove_id)

(*| Unit
  | Nil
  | Int of Int64.t
  | Bool of bool
  | String of string
  | Char of string
  | Var of string
  | Tuple of 'a expr_ann list
  | IfThen of ('a expr_ann * 'a expr_ann * 'a expr_ann)
  | Let of ('a pattern_ann * 'a expr_ann * 'a expr_ann)
  | LetRec of (('a pattern_ann * 'a expr_ann) list * 'a expr_ann)
  | MatchWithWhen of
      ('a expr_ann * ('a expr_ann * 'a expr_ann option * 'a pattern_ann) list)
    (*extra expr for when *)
  | Fun of ('a pattern_ann * 'a expr_ann)
  | App of ('a expr_ann * 'a expr_ann)
  | Binop of (bop * 'a expr_ann * 'a expr_ann)
  | Unaop of (unop * 'a expr_ann)
  | Constructor of (string * 'a expr_ann)
  | Record of (string * 'a expr_ann) list *)
let rec app_subst sub exp =
  let sout, sin = sub in
  match exp with
  | (lenv, typ), Unit -> ((lenv, subst_type typ sout sin), Unit)
  | (lenv, typ), Bool b -> ((lenv, subst_type typ sout sin), Bool b)
  | (lenv, typ), String s -> ((lenv, subst_type typ sout sin), String s)
  | (lenv, typ), Nil -> ((lenv, subst_type typ sout sin), Nil)
  | (lenv, typ), Char c -> ((lenv, subst_type typ sout sin), Char c)
  | (lenv, typ), Var x -> ((lenv, subst_type typ sout sin), Var x)
  | (lenv, typ), Tuple lst ->
      ( (lenv, subst_type typ sout sin),
        Tuple (List.map (fun x -> app_subst sub x) lst) )
  | (lenv, typ), IfThen (b, e1, e2) ->
      ( (lenv, subst_type typ sout sin),
        IfThen (app_subst sub b, app_subst sub e1, app_subst sub e2) )
  | (lenv, typ), Let (pat, e1, e2) ->
      ( (lenv, subst_type typ sout sin),
        Let (pat, app_subst sub e1, app_subst sub e2) )
  | (lenv, typ), LetRec (lst, e2) ->
      ( (lenv, subst_type typ sout sin),
        LetRec (List.map (fun (x, y) -> (x, app_subst sub y)) lst, e2) )
  | (lenv, typ), MatchWithWhen (e1, lst) ->
      let mapper (e1, e2_opt, pat) =
        match e2_opt with
        | Some e -> (app_subst sub e1, Some (app_subst sub e), pat)
        | None -> (app_subst sub e1, None, pat)
      in
      ( (lenv, subst_type typ sout sin),
        MatchWithWhen (app_subst sub e1, List.map mapper lst) )
  | (lenv, typ), Fun (pat, e) ->
      ((lenv, subst_type typ sout sin), Fun (pat, app_subst sub e))
  | (lenv, typ), App (e1, e2) ->
      ((lenv, subst_type typ sout sin), App (app_subst sub e1, app_subst sub e2))
  | (lenv, typ), Binop (op, e1, e2) ->
      ( (lenv, subst_type typ sout sin),
        Binop (op, app_subst sub e1, app_subst sub e2) )
  | (lenv, typ), Unaop (op, e) ->
      ((lenv, subst_type typ sout sin), Unaop (op, app_subst sub e))
  | (lenv, typ), Constructor (con, e) ->
      ((lenv, subst_type typ sout sin), Constructor (con, app_subst sub e))
  | (lenv, typ), Record lst ->
      ( (lenv, subst_type typ sout sin),
        Record (List.map (fun (s, e) -> (s, app_subst sub e)) lst) )
  (* 今、これは大変ですね〜〜〜 *)
  | _ -> failwith "unimplemented"

let type_expr subst exp =
  List.fold_left (fun exp sub -> app_subst sub exp) exp subst

(* let rec type_expr expr var_env typ_env =
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
*)

let type_prog parsed_ast = failwith "unimplemented"