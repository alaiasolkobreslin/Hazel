open Ast
type constructor = string
type constructors = (constructor * (types option * string)) list 

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
    let e1_typed = typecheck_expr e1 constr environ in
    let updated_environ = (update_environ pat e1_typed constr environ)@environ in
    let e2_typed = typecheck_expr e2 constr updated_environ in
    ({typed_pos = parsed.parsed_pos; ttype = (fst e2_typed).ttype}, Let (pat, e1, e2))
  |(parsed, LetRec _) -> failwith "finish later"
  |
    |_ -> failwith "typecheck error"


(*could neaten this up by just passing in the type instead of the expression *)
and update_environ pat expr_typ constr environ = 
  begin
    match (snd pat), expr_typ with
    |PUnit,TUnit-> []
    |PBool b,TBool -> []
    |PInt i,TInt -> []
    |PString s,TString -> []
    |PVar x,t -> [(x, t)]
    |PTup l1, TProd l2 -> 
      let interleaved = interleave_list l1 l2 in
      List.map (fun (x,y) -> update_environ x y constr environ) |> List.flatten
    |PSum (id, pat'), TSum x -> failwith "save for later"
    |PNil, TCons _ -> []
    |PCons (h::t), TCons typ -> (update_environ h typ constr environ)@(update_environ t (TCons typ) constr environ)
    |_ -> failwith "unimplemented"
  end

(* and 'a pattern = 
   | PUnit
   | PWild
   | PBool of bool
   | PInt of Int64.t
   | PString of string
   | PVar of string
   | PTup of 'a pattern_ann list
   | PSum of string * 'a pattern_ann
   | PNil
   | PCons of 'a pattern_ann * 'a pattern_ann *)

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
   | Record of (string * 'a expr_ann) list *)