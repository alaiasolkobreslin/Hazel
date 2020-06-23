
let escape_string str =
  let b = Bytes.create (2 * String.length str) in
  let pos = ref 0 in
  let add_char c =
    Bytes.set b !pos c; pos := !pos + 1 in
  let f = function
    | '\n' -> add_char '\\'; add_char 'n'
    | '\\' -> add_char '\\'; add_char '\\'
    | '\'' -> add_char '\\'; add_char '\''
    | '"' -> add_char '\\'; add_char '"'
    | '\t' -> add_char '\\'; add_char 't'
    | '\r' -> add_char '\\'; add_char 'r'
    | c -> add_char c in
  String.iter f str; Bytes.sub_string b 0 !pos