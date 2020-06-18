type expr = 
  | Unit 
  | Int of Int64.t
  | Bool of bool 
  | String of string
  | Char of string
  | Var of string 
  | Tuple of expr list
  | IfThen of (expr * expr * expr)
  | Let of (string * expr * expr)
  | LetRec of ((pattern list * expr) list * expr)
  | MatchWithWhen of (expr * (expr * expr * pattern) list) (*extra expr for when *)
  | Fun of (pattern * expr)
  | App of (expr * expr)
  | Ass of (string * expr)
  | Binop of (bop * expr * expr)
  | Unaop of (unop * expr)
  | Constraint of (string * expr)
  | Constructor of (string * types)
  | Record of (string * expr) list
and mutual_rec = string * expr
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
  | TBool 
  | TInt 
  | TString
  | TChar
  | TAlias of string * types
  | TProd of types list (* tuples *)
  | TSum of string * string * types list (* variants *)
  | TCons of types
  | TUnit
  | TRef of types
  | TRecord of (string * types) list
  | TVar of string
  | TConstraint of types * types 
  | TFun of types * types
