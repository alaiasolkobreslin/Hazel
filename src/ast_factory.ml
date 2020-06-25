open Ast

let wrap pos = {parsed_pos = pos; ptype = None}

let make_unit pos = (wrap pos, Unit)

let make_int i pos = (wrap pos, Int i)

let make_bool b pos = (wrap pos, Bool b)

let make_string s pos = (wrap pos, String s)

let make_char c pos = (wrap pos, Char c)

let make_var v pos = (wrap pos, Var v)

let make_tup lst pos = (wrap pos, lst)

let make_if_then e1 e2 e3 pos = (wrap pos, IfThen (e1, e2, e3))

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