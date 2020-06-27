type parsed = { parsed_pos : Lexing.position; ptype : types option }

and typed = { typed_pos : Lexing.position; ttype : types }


and 'a expr = 
  | Unit 
  | Int of Int64.t
  | Bool of bool 
  | String of string
  | Char of string
  | Var of string 
  | Tuple of 'a expr_ann list
  | IfThen of ('a expr_ann * 'a expr_ann * 'a expr_ann)
  | Let of ('a pattern * 'a expr_ann * 'a expr_ann)
  | LetRec of (('a pattern list * 'a expr_ann) list * 'a expr_ann)
  | MatchWithWhen of ('a expr_ann * ('a expr_ann * 'a expr_ann * 'a pattern) list) (*extra expr for when *)
  | Fun of ('a pattern * 'a expr_ann)
  | App of ('a expr_ann * 'a expr_ann)
  | Ass of ('a expr_ann * 'a expr_ann)
  | Binop of (bop * 'a expr_ann * 'a expr_ann)
  | Unaop of (unop * 'a expr_ann)
  | Constraint of (string * 'a expr_ann)
  | Constructor of (string * types)
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
  | PPair of 'a pattern * 'a pattern
  | PSum of string * 'a pattern
  | PNil
  | PCons of 'a pattern * 'a pattern

and 'a pattern_ann = 'a * 'a pattern

and bop = 
  | Plus 
  | Minus 
  | Mult 
  | Div
  | Mod
  | HMult
  | Cons 
  | Seq 
  | GT 
  | LT 
  | EQ 
  | NEQ 
  | PEQ 
  | PNEQ 
  | And
  | Or

and unop = Not | Neg | Ref | Deref

and types = 
  | TPlaceholder of string (* used for placeholder before typechecking *)
  | TBool 
  | TInt 
  | TString
  | TChar
  | TAlias of string * types
  | TProd of types list (* tuples *)
  | TSum of (string * types option) list (* variants - first string removed so we can combine with alias *)
  | TCons of types
  | TUnit
  | TRef of types
  | TRecord of (string * types) list
  | TVar of string
  | TConstraint of types * types 
  | TFun of types * types
  (* and type_decl =  *)
let wrap pos = {parsed_pos = pos; ptype = None}