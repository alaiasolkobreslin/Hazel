open Ast
type constructor = string
type constructors = (constructor * (types option * string)) list 
type env = (string * types) list

let rec interleave_list acc x y = 
  match x,y with
  |h::t, h2::t2 -> interleave_list ((h,h2)::acc) t t2
  |[], [] -> List.rev acc
  |_ -> failwith "Lists not of same length"

let rec retrieve_constructors lst acc str = 
  match lst with
  |(constructor, typ)::t -> 
    retrieve_constructors t ((constructor, typ, str)::acc) str
  |[] ->                 acc

let rec make_constr_lst_helper lst acc = 
  match lst with
  |(_, als, TSum x)::t -> 
    make_constr_lst_helper t (retrieve_constructors x acc als)
  |h::t                -> 
    make_constr_lst_helper t acc
  |_                   -> 
    acc

let make_constr_lst prog = 
  (* TODO: figure out how to work this with open statements *)
  match prog with
  |_,opn_list,typ_list,_ -> make_constr_lst_helper typ_list []

let rec typecheck_expr (exp) (constr) (environ) : typed expr_ann = 
  match exp with
  |(parsed, Unit) -> 
    {typed_pos = parsed.parsed_pos; ttype = TUnit}, Unit
  |(parsed, Nil) -> 
    ({typed_pos = parsed.parsed_pos; ttype = TCons (TVar "'a")}, Nil)
  |(parsed, Bool b) -> 
    ({typed_pos = parsed.parsed_pos; ttype = TBool}, Bool b)
  |(parsed, String str) -> 
    ({typed_pos = parsed.parsed_pos; ttype = TString}, String str)
  |(parsed, Char c) -> 
    ({typed_pos = parsed.parsed_pos; ttype = TChar}, Char c)
  |(parsed, Var x) -> 
    ({typed_pos = parsed.parsed_pos; ttype = (List.assoc x environ)}, Var x)
  |(parsed, Tuple lst) -> 
    ({typed_pos = parsed.parsed_pos; ttype = TProd (List.map (fun x -> (fst (typecheck_expr x constr environ)).ttype) lst)}, Tuple (List.map (fun x -> typecheck_expr x constr environ) lst))
  |(parsed, IfThen (b, e1, e2)) -> 
    let e1_typ = typecheck_expr e1 constr environ in
    let e2_typ = typecheck_expr e1 constr environ in 
    let b_typ = typecheck_expr b constr environ in
    if (fst b_typ).ttype = TBool && (fst e1_typ).ttype = (fst e2_typ).ttype 
    then ({typed_pos = parsed.parsed_pos; ttype = (fst e1_typ).ttype}, IfThen(b_typ, e1_typ, e2_typ)) 
    else failwith "typecheck error"
  |(parsed, Let (pat, e1, e2)) -> 
    let new_pat = parsed_to_typed_pat pat in
    let e1_typed = typecheck_expr e1 constr environ in
    let updated_environ = (update_environ pat (fst e1_typed).ttype constr environ)@environ in
    let e2_typed = typecheck_expr e2 constr updated_environ in
    ({typed_pos = parsed.parsed_pos; ttype = (fst e2_typed).ttype}, Let (new_pat, e1_typed, e2_typed))
  |(parsed, LetRec _) -> failwith "finish later"
  |(parsed, MatchWithWhen (init, lst)) -> 
    let init_typed = typecheck_expr init constr environ in
    let typed_cases = p_match_helper (fst init_typed).ttype lst [] constr environ in
    begin
      match typed_cases with
      |(e, o, p)::t -> 
        ({typed_pos = parsed.parsed_pos; ttype = (fst e).ttype}, MatchWithWhen (init_typed, typed_cases))
      |_ -> failwith "typecheck error"
    end
  |(parsed, Fun (pat, e)) -> failwith "need unification"
  |(parsed, App (f, e)) -> 
    let f_typed = typecheck_expr f constr environ in
    let e_typed = typecheck_expr f constr environ in
    begin
      match (fst f_typed).ttype, (fst e_typed).ttype with
      |TFun (arg_t, e_t), t when arg_t = t -> 
        ({typed_pos = parsed.parsed_pos; ttype = e_t}, App (f_typed, e_typed))
      |_ -> failwith "argument type does not match function"
    end
  |(parsed, Binop (b, e1, e2)) -> typecheck_bop (parsed, Binop (b, e1, e2)) constr environ
  |parsed, Unaop (u, e) -> typecheck_unop e constr environ
  |_ -> failwith "typecheck error"

and p_match_helper typ lst acc constr env = 
  match lst, acc with
  |(exp, None, pat)::t, []-> 
    let new_pat = parsed_to_typed_pat pat in
    let new_environ = update_environ pat typ constr env in
    let typed_exp = typecheck_expr exp constr new_environ in
    p_match_helper typ t [(typed_exp, None, new_pat)] constr env
  |(exp, Some b, pat)::t, (e, o, p)::t2 -> 
    let new_pat = parsed_to_typed_pat pat in
    let new_environ = update_environ pat typ constr env in
    let typed_exp = typecheck_expr exp constr new_environ in
    let typed_b = typecheck_expr b constr new_environ in
    if ((fst e).ttype = (fst typed_exp).ttype) && ((fst typed_b).ttype = TBool) 
    then p_match_helper typ t ((typed_exp, Some typed_b, new_pat)::acc) constr env
    else failwith "typecheck error"
  |([], _) -> List.rev acc
  |_ -> failwith "typecheck error"

(*could neaten this up by just passing in the type instead of the expression *)
and update_environ pat (expr_typ : Ast.types) constr environ = 
  begin
    match ((snd pat), expr_typ) with
    |PUnit, TUnit-> []
    |PBool b, TBool -> []
    |PInt i, TInt -> []
    |PString s, TString -> []
    |PVar x, t -> [(x, t)]
    |PTup l1, TProd l2 -> 
      let interleaved = interleave_list [] l1 l2 in
      List.map (fun (x,y) -> update_environ x y constr environ) interleaved |> List.flatten
    |PSum (id, pat'), TSum x -> failwith "save for later"
    |PNil, TCons _ -> []
    |PCons (h, t), TCons typ -> (update_environ h typ constr environ)@(update_environ t (TCons typ) constr environ)
    |_ -> failwith "unimplemented"
  end

and typecheck_bop (exp) (constr) (environ) = 
  let parsed = fst exp in
  let bop, e1, e2 = 
    begin
      match exp with
      |_, Binop (op, e, e') -> (op, (typecheck_expr e constr environ), (typecheck_expr e' constr environ))
      |_ -> failwith "Typecheck error"
    end
  in
  match bop with
  |Plus |Minus |Mult |Div |Mod |HMult ->
    if (fst e1).ttype = TInt && (fst e2).ttype = TInt
    then ({typed_pos = parsed.parsed_pos; ttype = TInt}, Binop (bop, e1, e2))
    else failwith "typecheck error"
  |GT |GEQ |LT |LEQ -> 
    if (fst e1).ttype = TInt && (fst e2).ttype = TInt
    then ({typed_pos = parsed.parsed_pos; ttype = TBool}, Binop (bop, e1, e2))
    else failwith "typecheck error"
  |ConsBop -> 
    begin
      match (fst e1).ttype, (fst e2).ttype with
      |a, TCons b when a = b -> ({typed_pos = parsed.parsed_pos; ttype = TCons b}, Binop (bop, e1, e2))
      |_ -> failwith "Inconsistant types in list"
    end
  |Seq -> 
    if (fst e1).ttype = TUnit
    then ({typed_pos = parsed.parsed_pos; ttype = (fst e2).ttype}, Binop (bop, e1, e2))
    else failwith "typecheck error"
  |EQ |NEQ -> 
    if (fst e1).ttype = (fst e2).ttype
    then ({typed_pos = parsed.parsed_pos; ttype = (fst e1).ttype}, Binop (bop, e1, e2))
    else failwith "typecheck error"
  |PEQ |PNEQ -> 
    begin
      match (fst e1).ttype, (fst e2).ttype with
      |TRef a, TRef b when a = b -> 
        ({typed_pos = parsed.parsed_pos; ttype = TBool}, Binop (bop, e1, e2))
      |_ -> failwith "typecheck error"
    end
  |And |Or -> 
    if (fst e1).ttype = TBool && (fst e2).ttype = TBool
    then ({typed_pos = parsed.parsed_pos; ttype = TBool}, Binop (bop, e1, e2))
    else failwith "typecheck error"
  |Ass -> 
    (* This is a mess. I'll clean it up later *)
    begin
      match snd e1 with
      |Var x -> 
        begin 
          match List.assoc_opt x environ with
          |Some (TRef a) when (fst e2).ttype = a -> 
            ({typed_pos = parsed.parsed_pos; ttype = TUnit}, Binop (bop, e1, e2))
          |_ -> failwith "typecheck error"
        end
      |_ -> failwith "typecheck error"
    end
  |Cat -> 
    if (fst e1).ttype = TString && (fst e2).ttype = TString
    then ({typed_pos = parsed.parsed_pos; ttype = TString}, Binop (bop, e1, e2))
    else failwith "typecheck error"
  |Pipe -> 
    begin
      match (fst e1).ttype, (fst e2).ttype with
      |t, TFun (arg_t, e_t) when arg_t = t -> 
        ({typed_pos = parsed.parsed_pos; ttype = e_t}, App (e1, e2))
      |_ -> failwith "argument type does not match function"
    end
and typecheck_unop exp constr environ = 
  let prsed = fst exp in
  let uop, e = 
    begin 
      match exp with
      |_, Unaop (op, ex) -> (op, typecheck_expr ex constr environ)
      |_ -> failwith "typecheck error"
    end
  in
  begin 
    match uop with
    |Not -> if (fst e).ttype = TBool then ({typed_pos = prsed.parsed_pos; ttype = TBool}, Unaop (uop, e)) else failwith "typecheck error"
    |Neg -> if (fst e).ttype = TInt then ({typed_pos = prsed.parsed_pos; ttype = TInt}, Unaop (uop, e)) else failwith "typecheck error"
    |Ref -> ({typed_pos = prsed.parsed_pos; ttype = TRef (fst e).ttype}, Unaop (uop, e))
    |Deref -> 
      begin match (fst e).ttype with
        |TRef a -> ({typed_pos = prsed.parsed_pos; ttype = a}, Unaop (uop, e))
        |_ -> failwith "typecheck error"        
      end
  end

and parsed_to_typed_pat pat = 
  match pat with
  |(psd, PUnit) -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PUnit)
  |(psd, PBool b) -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PBool b)
  |psd, PInt i -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PInt i)
  |psd, PString s -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PString s)
  |psd, PVar x -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PVar x)
  |psd, PTup l1-> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PTup (List.map parsed_to_typed_pat l1))
  |psd, PSum (id, pat') -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PSum (id, (parsed_to_typed_pat pat')))
  |psd, PNil -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PNil)
  |psd, PCons (h, t) -> ({typed_pos = psd.parsed_pos; ttype = TPlaceholder "N/A"}, PCons (parsed_to_typed_pat h, parsed_to_typed_pat t))
  |_ -> failwith "unimplemented"

(* | Nil
   | Int of Int64.t
   | Bool of bool 
   | String of string
   | Char of string
   | Var of string 
   | Tuple of 'a expr_ann list
   | IfThen of ('a expr_ann * 'a expr_ann * 'a expr_ann)
   | Let of ('a pattern_ann * 'a expr_ann * 'a expr_ann)
   | LetRec of (('a pattern_ann * 'a expr_ann) list * 'a expr_ann)
   | MatchWithWhen of ('a expr_ann * ('a expr_ann * 'a expr_ann option * 'a pattern_ann) list) (*extra expr for when *)
   | Fun of ('a pattern_ann list * 'a expr_ann)
   | App of ('a expr_ann * 'a expr_ann)
   | Binop of (bop * 'a expr_ann * 'a expr_ann)
   | Unaop of (unop * 'a expr_ann)
   | Cons of ('a expr_ann * 'a expr_ann)
   | Constraint of (string * 'a expr_ann)
   | Constructor of (string * 'a expr_ann)
   | Record of (string * 'a expr_ann) list 

   and unop = Not | Neg | Ref | Deref*)
