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
  | Let of (string * 'a expr_ann * 'a expr_ann)
  | LetRec of ((pattern list * 'a expr_ann) list * 'a expr_ann)
  | MatchWithWhen of ('a expr_ann * ('a expr_ann * 'a expr_ann * pattern) list) (*extra expr for when *)
  | Fun of (pattern * 'a expr_ann)
  | App of ('a expr_ann * 'a expr_ann)
  | Ass of (string * 'a expr_ann)
  | Binop of (bop * 'a expr_ann * 'a expr_ann)
  | Unaop of (unop * 'a expr_ann)
  | Constraint of (string * 'a expr_ann)
  | Constructor of (string * types)
  | Record of (string * 'a expr_ann) list

and 'a expr_ann = 'a * 'a expr

and 'a mutual_rec = string * 'a expr_ann

and pattern = 
  | PUnit
  | PWild
  | PBool of bool
  | PInt of int
  | PString of string
  | PVar of string
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern

and bop = 
  | Plus 
  | Minus 
  | Mult 
  | HMult
  | Cons 
  | Seq 
  | GT 
  | LT 
  | EQ 
  | NEQ 
  | PEQ 
  | PNEQ 
  | Mod 
  | And
  | Or

and unop = Not | Neg | Ref | Deref

and types = 
  | TPlaceholder of string
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