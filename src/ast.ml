open Sexpr

type parsed = { parsed_pos : Lexing.position; ptype : types option }

and typed = { typed_pos : Lexing.position; ttype : types }

and 'a prog = 'a * 'a open_stmnt list * 'a alias list * 'a expr_ann option

and 'a open_stmnt = 'a * string

and 'a expr = 
  | Unit 
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
  | MatchWithWhen of ('a expr_ann * ('a expr_ann * 'a expr_ann option * 'a pattern_ann) list) (*extra expr for when *)
  | Fun of ('a pattern_ann list * 'a expr_ann)
  | App of ('a expr_ann * 'a expr_ann)
  | Binop of (bop * 'a expr_ann * 'a expr_ann)
  | Unaop of (unop * 'a expr_ann)
  | Cons of ('a expr_ann * 'a expr_ann)
  | Constraint of (string * 'a expr_ann)
  | Constructor of (string * 'a expr_ann)
  | Record of (string * 'a expr_ann) list

and 'a expr_ann = 'a * 'a expr

and 'a mutual_rec = string * 'a expr_ann

and 'a pattern = 
  | PUnit
  | PWild
  | PBool of bool
  | PInt of Int64.t
  | PString of string
  | PVar of string
  | PTup of 'a pattern_ann list
  | PSum of string * 'a pattern_ann
  | PNil
  | PCons of 'a pattern_ann * 'a pattern_ann

and 'a pattern_ann = 'a * 'a pattern

and bop = 
  | Plus 
  | Minus 
  | Mult 
  | Div
  | Mod
  | HMult
  | ConsBop
  | Seq 
  | GT 
  | GEQ
  | LT
  | LEQ 
  | EQ 
  | NEQ 
  | PEQ 
  | PNEQ 
  | And
  | Or
  | Ass
  | Cat
  | Pipe

and unop = Not | Neg | Ref | Deref

and types = 
  | TPlaceholder of string (* used for placeholder before typechecking *)
  | TBool 
  | TInt 
  | TString
  | TChar
  | TProd of types list (* tuples *)
  | TSum of (string * types option) list (* variants - first string removed so we can combine with alias *)
  | TCons of types
  | TUnit
  | TRef of types
  | TRecord of (string * types) list
  | TVar of string
  | TConstraint of types * types 
  | TFun of (types * types)
  (* and type_decl =  *)

(* TODO: why is this 'a alias? *)
and 'a alias = 'a * string * types

let wrap pos = {parsed_pos = pos; ptype = None}

let bop_to_sexpr bop = 
  let str = match bop with
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "mod"
    | HMult -> ">>*"
    | ConsBop -> "::"
    | Seq -> ";"
    | GT -> ">"
    | GEQ -> ">="
    | LT -> "<"
    | LEQ -> "<="
    | EQ -> "="
    | NEQ -> "<>"
    | PEQ -> "=="
    | PNEQ -> "!="
    | And -> "&&"
    | Or -> "||"
    | Ass -> ":="
    | Cat -> "^"
    | Pipe -> "|>" in
  SNode str

let uop_to_sexpr uop =
  let str = match uop with
    | Not -> "not"
    | Neg -> "-"
    | Ref -> "ref"
    | Deref -> "!" in
  SNode str

let rec expr_to_sexpr = function
  | Unit -> SNode ("()")
  | Nil -> SNode ("[]")
  | Int i -> SNode (Int64.to_string i)
  | Bool b -> SNode (string_of_bool b)
  | String s -> SNode ("\"" ^ (Util.escape_string s) ^ "\"")
  | Char c -> SNode ("'" ^ (Util.escape_string c) ^ "'")
  | Var id -> SNode id
  | Tuple lst -> SList (List.map (fun elt -> elt |> snd |> expr_to_sexpr) lst)
  | IfThen (e1, e2, e3) -> 
    SList [SNode "if"; e1 |> snd |> expr_to_sexpr;
           SNode "then"; e2 |> snd |> expr_to_sexpr;
           SNode "else"; e3 |> snd |> expr_to_sexpr]
  | Let (p, e1, e2) -> 
    SList [SNode "let"; p |> snd |> pat_to_sexpr;
           SNode "="; e1 |> snd |> expr_to_sexpr;
           SNode "in"; e2 |> snd |> expr_to_sexpr]
  | LetRec (lst, expr) ->
    begin
      match lst with
      | (p, e)::[] ->  SList [SNode "let rec"; pat_to_sexpr (snd p); 
                              expr_to_sexpr (snd e);
                              SNode "in"; expr_to_sexpr (snd expr)]
      | (p, e)::t -> SList [SNode "let rec"; pat_to_sexpr (snd p); 
                            expr_to_sexpr (snd e);
                            SList (rec_and_to_sexpr t);
                            SNode "in"; expr_to_sexpr (snd expr)]
      | [] -> failwith "rec and parse precondition violated"
    end
  | MatchWithWhen (e, lst) -> 
    SList [SNode "match";
           e |> snd |> expr_to_sexpr;
           SList (match_with_when_to_sexpr lst)]
  | Fun (lst, e) ->
    SList [SNode "lambda"; 
           SList (List.map (fun elt -> elt |> snd |> pat_to_sexpr) lst);
           e |> snd |> expr_to_sexpr]
  | App (e1, e2) ->
    SList [e1 |> snd |> expr_to_sexpr;
           e2 |> snd |> expr_to_sexpr]
  | Binop (bop, e1, e2) ->
    SList [bop_to_sexpr bop;
           e1 |> snd |> expr_to_sexpr;
           e2 |> snd |> expr_to_sexpr]
  | Unaop (uop, e) ->
    SList [uop_to_sexpr uop;
           e |> snd |> expr_to_sexpr]
  | Cons (e1, e2) ->
    SList [SNode "::";
           e1 |> snd |> expr_to_sexpr;
           e2 |> snd |> expr_to_sexpr]
  | Constraint (str, e)
  | Constructor (str, e) ->
    SList [SNode str;
           e |> snd |> expr_to_sexpr]
  | Record lst -> 
    SList (List.map (fun (str, e) -> 
        SList [SNode str; e |> snd |> expr_to_sexpr]) lst)

and match_with_when_to_sexpr = function
  | [] -> []
  | (e1, Some e2, p)::t ->
    [SNode "with"; SNode "|"; pat_to_sexpr (snd p);
     SNode "when"; expr_to_sexpr (snd e2);
     SNode "->"; e1 |> snd |> expr_to_sexpr] @
    (match_with_when_to_sexpr_help t)
  | (e, None, p)::t -> 
    [SNode "with"; SNode "|"; pat_to_sexpr (snd p);
     SNode "->"; e |> snd |> expr_to_sexpr] @
    (match_with_when_to_sexpr_help t)

and match_with_when_to_sexpr_help = function
  | [] -> []
  | (e1, Some e2, p)::t ->
    [SNode "|"; pat_to_sexpr (snd p);
     SNode "when"; expr_to_sexpr (snd e2);
     SNode "->"; e1 |> snd |> expr_to_sexpr] @
    (match_with_when_to_sexpr t)
  | (e, None, p)::t -> 
    [SNode "|"; pat_to_sexpr (snd p);
     SNode "->"; e |> snd |> expr_to_sexpr] @
    (match_with_when_to_sexpr t)

and rec_and_to_sexpr = function
  | [] -> []
  | (p, e)::t -> 
    ([SNode "and"; pat_to_sexpr (snd p); expr_to_sexpr (snd e)] @
     (rec_and_to_sexpr t))

and pat_to_sexpr = function
  | PUnit -> SNode "()"
  | PWild -> SNode "_"
  | PBool b -> SNode (string_of_bool b)
  | PInt i -> SNode (Int64.to_string i)
  | PString s
  | PVar s -> SNode s
  | PTup lst ->  SList (List.map (fun elt -> elt |> snd |> pat_to_sexpr) lst)
  | PSum (str, p) -> 
    SList [SNode str; p |> snd |> pat_to_sexpr]
  | PNil -> SNode "[]"
  | PCons (p1, p2) -> 
    SList [p1 |> snd |> pat_to_sexpr; SNode "::";
           p2 |> snd |> pat_to_sexpr]

and open_to_sexpr = function
  | (_, str) ->
    SList [SNode "open"; SNode str]

and alias_to_sexpr = function
  | (_, str, typ) -> 
    SList [SNode "type"; SNode str; SNode "="; types_to_sexpr typ]

and variant_helper acc (n,t) = 
  match t with
  |None     -> acc@((SNode "|")::[(SNode n)])
  |Some typ -> 
    begin 
      match types_to_sexpr typ with 
      |SNode s    -> acc@((SNode "|")::(SNode n)::(SNode "of")::[(SNode s)])
      |SList lst  -> acc@((SNode "|")::(SNode n)::(SNode "of")::lst)
    end

and record_helper acc (n,t) = 
  begin 
    match types_to_sexpr t with 
    |SNode s    -> acc@((SNode ";")::(SNode n)::(SNode ":")::[(SNode s)])
    |SList lst  -> acc@((SNode ";")::(SNode n)::(SNode ":")::lst)
  end

and types_to_sexpr = function
  | TPlaceholder str -> SNode str
  | TBool -> SNode "bool"
  | TInt -> SNode "int"
  | TString -> SNode "string"
  | TChar -> SNode "char"
  | TProd lst -> SList (List.map types_to_sexpr lst)
  | TSum lst -> SList ((List.fold_left variant_helper [] lst)@([SNode "end"]))
  | TCons typ -> SList [types_to_sexpr typ; SNode "list"]
  | TUnit -> SNode "unit"
  | TRef typ -> SList [types_to_sexpr typ; SNode "ref"]
  | TRecord lst -> 
    let pre_node = List.fold_left record_helper [] lst in 
    begin
      match pre_node with
      |h::t -> SList (SNode "{"::t@([SNode "}"]))
      |_ -> SList (pre_node@([SNode "}"]))
    end
  | TVar str -> SNode str
  | TConstraint (t1, t2) ->
    SList [SNode "constraint"; types_to_sexpr t1; types_to_sexpr t2]
  | TFun (t1, t2) -> SList [types_to_sexpr t1; SNode "->"; types_to_sexpr t2]

let prog_to_sexpr (prog:'a prog) = 
  match prog with
  | (_, l1, l2, e) ->
    begin
      match e with
      | None -> SList [SList (List.map open_to_sexpr l1);
                       SList (List.map alias_to_sexpr l2);]
      | Some e' ->
        SList [SList (List.map open_to_sexpr l1);
               SList (List.map alias_to_sexpr l2);
               expr_to_sexpr (snd e')]
    end