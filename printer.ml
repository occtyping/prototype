
open Ast

let string_of_varid = string_of_int

let string_of_const c =
  match c with
  | Magic -> "magic"
  | Unit  -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Char c -> Printf.sprintf "'%c'" c
  | Atom a -> Cduce.string_of_type a

let rec string_of_expr e =
  match e with
  | Const c -> string_of_const c
  | Var v -> Printf.sprintf "v%s" (string_of_varid v)
  | Lambda (_,v,e) ->
    Printf.sprintf "fun %s -> %s" (string_of_varid v) (string_of_expr e)
  | RecLambda (s,_,v,e) ->
    Printf.sprintf "rec %s %s -> %s" (string_of_varid s) (string_of_varid v) (string_of_expr e)
  | Ite (e,t,e1,e2) ->
    Printf.sprintf "if %s is %s then %s else %s" (string_of_expr e) (Cduce.string_of_type t)
      (string_of_expr e1) (string_of_expr e2)
  | App (e1, e2) ->
    Printf.sprintf "(%s) (%s)" (string_of_expr e1) (string_of_expr e2)
  | Let (v, e1, e2) ->
    Printf.sprintf "let %s = %s in %s" (string_of_varid v) (string_of_expr e1) (string_of_expr e2)
  | Pair (e1, e2) ->
    Printf.sprintf "(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | Projection (Fst, e) -> Printf.sprintf "fst (%s)" (string_of_expr e)
  | Projection (Snd, e) -> Printf.sprintf "snd (%s)" (string_of_expr e)
  | Debug (str, e) -> Printf.sprintf "debug \"%s\" (%s)" str (string_of_expr e)
