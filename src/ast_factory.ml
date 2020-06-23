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

(* TODO: finish ast factory  *)