open Ast


let make_unit pos = (wrap pos, Unit)

let make_int i pos = (wrap pos, Int i)

let make_bool b pos = (wrap pos, Bool b)

let make_string s pos = (wrap pos, String s)

let make_char c pos = (wrap pos, Char c)

let make_var v pos = (wrap pos, Var v)

let make_tup lst pos = (wrap pos, lst)

let make_if_then e1 e2 e3 pos = (wrap pos, IfThen (e1, e2, e3))

let make_variant str e pos = (wrap pos, Constructor (str, e))

let make_constraint str e pos = (wrap pos, Constraint (str, e))

let make_binop bop e1 e2 pos = (wrap pos, Binop (bop, e1, e2))

let make_unop uop e pos = (wrap pos, Unaop (uop, e))

let make_app e1 e2 pos = (wrap pos, App (e1, e2))

let make_open i pos = (wrap pos, i)

let make_prog l1 l2 e pos = (wrap pos, l1, l2, e)

(* TODO: finish exprs  *)

let make_unit_pat pos = (wrap pos, PUnit)

let make_wild_pat pos = (wrap pos, PWild)

let make_bool_pat b pos = (wrap pos, PBool b)

let make_int_pat i pos = (wrap pos, PInt i)

let make_string_pat s pos = (wrap pos, PString s)

let make_var_pat v pos = (wrap pos, PVar v)

let make_pair_pat p1 p2 pos = (wrap pos, PPair (p1, p2))

let make_nil_pat pos = (wrap pos, PNil)

let make_cons_pat p1 p2 pos = (wrap pos, PCons (p1, p2))

let make_sum_pat s p pos = (wrap pos, PSum (s, p))


let make_tunit u pos = (wrap pos, TUnit)

let make_tbool b pos = (wrap pos, TBool)

let make_tint i pos = (wrap pos, TInt)

let make_tchar c pos = (wrap pos, TChar)

let make_tstring s pos = (wrap pos, TString)

let make_tsum lst pos = (wrap pos, TSum lst)

let make_tprod lst pos = (wrap pos, TProd (List.rev lst)) (*List is reversed because order matters for tuples *)

let make_trecord lst pos = (wrap pos, TRecord lst)

let make_tlist t pos = (wrap pos, TCons t)

let make_tref t pos = (wrap pos, TRef t)

let make_tfun (p1,p2) pos = (wrap pos, TFun (p1, p2))

let make_talias n t pos = (wrap pos, TAlias (n,t))

let make_placeholder i pos = (wrap pos, TPlaceholder i)
