open Ast
type constructor = string
type constructors = (constructor * types option * string) list 
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

(* let rec typecheck_expr exp constr environ = 
   match exp with
   |(parsed, Unit) -> ({typed_pos = parsed.parsed_pos; ttype = TUnit; env = environ}, Unit)
   |(parsed, Nil) -> ({typed_pos = parsed.parsed_pos; ttype = TCons (TVar "'a"); env = environ}, Nil)
   |(parsed, Bool b) -> ({typed_pos = parsed.parsed_pos; ttype = TBool; env = environ}, Bool b)
   |(parsed, String str) -> ({typed_pos = parsed.parsed_pos; ttype = TString; env = environ}, String str)
   |(parsed, Char c) -> ({typed_pos = parsed.parsed_pos; ttype = TChar; env = environ}, Char c)
   |(parsed, Var x) -> ({typed_pos = parsed.parsed_pos; ttype = (List.assoc x environ); env = environ}, Var x)
   |(parsed, Tuple lst) -> ({typed_pos = parsed.parsed_pos; ttype = TProd (List.map (fun x -> typecheck_expr x constr environ)); env = environ}, Tuple lst)
   |_ -> failwith "typecheck error" *)


(*could neaten this up by just passing in the type instead of the expression *)
let rec update_environ pat exp constr environ = 
  let expr_typ = typecheck_expr exp constr environ in
  begin
    match pat, expr_typ with
    |(_,PUnit),TUnit-> []
    |(_,PBool b),TBool -> []
    |(_,PInt i),TInt -> []
    |(_,PString s),TString -> []
    |(_,PVar x),t -> [(x, t)]
    |(_,(PTup l1)),(TSum l2) -> failwith "aaargh I really don't want to do this one"
    |_ -> failwith "unimplemented"
  end

and typecheck_expr exp constr environ = 
  match exp with
  |(parsed, Unit) -> TUnit
  |(parsed, Nil) -> TCons (TVar "'a")
  |(parsed, Bool b) -> TBool
  |(parsed, String str) -> TString
  |(parsed, Char c) -> TChar
  |(parsed, Var x) -> (List.assoc x environ)
  |(parsed, Tuple lst) -> TProd (List.map (fun x -> typecheck_expr x constr environ) lst)
  |(parsed, IfThen (b, e1, e2)) -> 
    let e1_typ = typecheck_expr e1 constr environ in
    let e2_typ = typecheck_expr e2 constr environ in 
    if (typecheck_expr b constr environ = TBool) && e1_typ = e2_typ 
    then e1_typ else failwith "typecheck error"
  |(parsed, Let (pat, e1, e2)) -> typecheck_expr e2 constr ((update_environ pat e1 constr environ)@environ)
  |_ -> failwith "typecheck error"

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