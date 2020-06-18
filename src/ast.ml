type tuple = Placeholder
type expr = 
  | Unit 
  | Int of Int64.t
  | Bool of bool 
  | String of string
  | Char of string
  | Var of string 
  | Tuple of tuple
  | IfThen of (expr * expr * expr)
  | Let of (string * expr * expr)
  | LetRec of (string * expr * expr)
  | And of (string * expr * expr)
  | BeginEnd of expr
  | MatchWithWhen of (expr * (expr * expr * pattern) list) (*extra expr for when *)
  | Function of (string list * expr)
  | Application of (expr * expr)
  | Assignment of (string * expr)
  | Ref of expr
  | Deref of expr
  | Binop of (bop * expr * expr)
  | Unaop of (unop * expr)
  | Constraint of (string * expr)
  | Constructor of (string * types)
and pattern = Placeholder2
and bop = 
  | Plus 
  | Minus 
  | Mult 
  | Cons 
  | Seq 
  | GT 
  | LT 
  | EQ 
  | NEQ 
  | PEQ 
  | PNEQ 
  | Mod 
  | BoolAnd
  | BoolOr
and unop = Not | Neg 
and types = 
  | Boolean 
  | Integer 
  | Product of (types * types) 
  | Sum of (types * types)
  | List of types
  | Terminal
  | Reference of types