open Ast


let make_unit pos : parsed expr_ann = (wrap pos, Unit)

let make_int i pos : parsed expr_ann = (wrap pos, Int i)

let make_bool b pos : parsed expr_ann = (wrap pos, Bool b)

let make_string s pos : parsed expr_ann = (wrap pos, String s)

let make_char c pos : parsed expr_ann = (wrap pos, Char c)

let make_var v pos : parsed expr_ann = (wrap pos, Var v)

let make_tup lst pos : parsed expr_ann = (wrap pos, Tuple lst)

let make_if_then e1 e2 e3 pos : parsed expr_ann = (wrap pos, IfThen (e1, e2, e3))

let make_variant str e pos : parsed expr_ann = (wrap pos, Constructor (str, e))

(* let make_constraint str e pos : parsed expr_ann = (wrap pos, Constraint (str, e)) *)

let make_binop bop e1 e2 pos : parsed expr_ann = (wrap pos, Binop (bop, e1, e2))

let make_unop uop e pos : parsed expr_ann = (wrap pos, Unaop (uop, e))

let make_app e1 e2 pos : parsed expr_ann = (wrap pos, App (e1, e2))

let make_nil pos : parsed expr_ann = (wrap pos, Nil)

let make_arr e1 e2 pos : parsed expr_ann = (wrap pos, Cons (e1, e2))

let make_fun l e pos : parsed expr_ann = (wrap pos, Fun (l, e))

let make_args x a  = (x :: a)

let make_let_notf x e1 e2 pos : parsed expr_ann = 
  (wrap pos, Let (x, e1, e2))

let make_let_defn pat e pos : parsed let_defn =
  (wrap pos, pat, e)

let make_let_f x args e1 e2 pos : parsed expr_ann = 
  (wrap pos, Let (x, (wrap pos, Fun (args, e1)), e2))

let make_letrec_notf i e pos : parsed expr_ann = 
  (wrap pos, LetRec ([(i, e)], e))

let make_letrec_f i args e pos : parsed expr_ann = 
  (wrap pos, LetRec ([(i, (make_fun args e pos))], e))

let make_and_notf pre i e pos : parsed expr_ann = 
  match pre with
  |(loc, LetRec (lst, tail)) -> (loc, LetRec ((i, e)::lst, e))
  |_ -> failwith "owo what's this?"

let make_and_f pre i args e pos : parsed expr_ann = 
  begin 
    match pre with
    |(loc, LetRec (lst, tail)) -> (loc, LetRec ((i, (make_fun args e pos))::lst, e))
    |_ -> failwith "owo how'd you get here?"
  end

let complete_m_rec pre e : parsed expr_ann = 
  match pre with
  | (loc, LetRec (lst, tail)) -> (loc, LetRec (List.rev lst, e))
  | _ -> failwith "This shouldn't have happened"

let make_init_pmatch e pos : parsed expr_ann = (wrap pos, MatchWithWhen (e, []))

let make_update_pmatch pre pt e1 e2 pos : parsed expr_ann = 
  match pre with
  |(loc, MatchWithWhen (m, lst)) -> (loc, MatchWithWhen (m, lst @ [(e1, e2, pt)]))
  |_ -> failwith "How?"

let make_init_record i e pos : parsed expr_ann = (wrap pos, Record [(i, e)])

let make_update_record pre i e pos : parsed expr_ann = 
  match pre with
  | (loc, Record l) -> (loc, Record ((i,e)::l))
  | _ -> failwith "nani?"

let make_open i pos = (wrap pos, i)

let make_prog l1 (l2 : parsed alias list) e pos = (wrap pos, l1, l2, e)

(* TODO: finish exprs  *)

let make_unit_pat pos = (wrap pos, PUnit)

let make_wild_pat pos = (wrap pos, PWild)

let make_bool_pat b pos = (wrap pos, PBool b)

let make_int_pat i pos = (wrap pos, PInt i)

let make_string_pat s pos = (wrap pos, PString s)

let make_var_pat v pos = (wrap pos, PVar v)

let make_tup_pat lst pos = (wrap pos, PTup lst)

let make_nil_pat pos = (wrap pos, PNil)

let make_cons_pat p1 p2 pos = (wrap pos, PCons (p1, p2))

let make_sum_pat s p pos = (wrap pos, PSum (s, p))

let make_talias n t pos : parsed alias = (wrap pos, n, t)
