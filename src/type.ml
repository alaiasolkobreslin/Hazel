open Ast

exception TypingError of (Lexing.position * string)

let lookup env key =
  match List.find_opt (fun (k, _) -> k = key) env with
  | Some (_, v) -> Some v
  | None -> None

let type_expr expr var_env typ_env =
  let (pos, e) = expr in
  match e with
  | Unit -> TUnit
  | Nil -> failwith "unimplemented" (* TCons *)
  | Int _ -> TInt
  | Bool _ -> TBool
  | String _ -> TString
  | Char _ -> TChar
  | Var id ->
    begin
      match lookup var_env id with
      | Some typ -> typ
      | None -> raise (TypingError (pos, "unbound variable " ^ id))
    end
  | _ -> failwith "unimplemented"

let type_prog parsed_ast = failwith "unimplemented"