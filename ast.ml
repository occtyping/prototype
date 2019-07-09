open Types_additions
type typ = Cduce.typ

type varname = string
type varid = int (* It is NOT De Bruijn indexes, but unique IDs *)
type exprid = int

type annotation = exprid Position.located

type const =
| Magic
| Unit
| EmptyRecord
| Bool of bool
| Int of int
| Char of char
| Atom of string

type projection = Fst | Snd | Field of string

type ('a, 'typ, 'v) ast =
| Const of const
| Var of 'v
| Lambda of 'typ * 'v * ('a, 'typ, 'v) t
| RecLambda of 'v * 'typ * 'v * ('a, 'typ, 'v) t
| InfLambda of 'typ * 'v * ('a, 'typ, 'v) t
| Ite of ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| App of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Let of 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Pair of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Projection of projection * ('a, 'typ, 'v) t
| RecordUpdate of ('a, 'typ, 'v) t * string * ('a, 'typ, 'v) t option
| Debug of string * ('a, 'typ, 'v) t

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type annot_expr = (annotation, typ, varid) t
type expr = (unit, typ, varid) t
type parser_expr = (annotation, type_expr, varname) t

module Expr = struct
    type t = expr
    let compare = compare
    let equiv t1 t2 = (compare t1 t2) = 0
end
module ExprMap = Map.Make(Expr)

type id_map = int StrMap.t

let empty_id_map = StrMap.empty

let unique_exprid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )

let unique_varid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )

let identifier_of_expr (a,_) = Position.value a
let position_of_expr (a,_) = Position.position a

let new_annot p =
    Position.with_pos p (unique_exprid ())

let parser_expr_to_annot_expr tenv id_map e =
    let rec aux env (a,e) =
        let e = match e with
        | Const c -> Const c
        | Var str ->
            if StrMap.mem str env
            then Var (StrMap.find str env)
            else Const (Atom str)
        | Lambda (t,str,e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Lambda (type_expr_to_typ tenv t, varid, aux env e)
        | InfLambda (t, str, e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            InfLambda (type_expr_to_typ tenv t, varid, aux env e)
        | RecLambda (recstr,t,str,e) ->
            let recvarid = unique_varid () in
            let varid = unique_varid () in
            let env = StrMap.add recstr recvarid env in
            let env = StrMap.add str varid env in
            RecLambda (recvarid, type_expr_to_typ tenv t, varid, aux env e)
        | Ite (e, t, e1, e2) ->
            Ite (aux env e, type_expr_to_typ tenv t, aux env e1, aux env e2)
        | App (e1, e2) ->
            App (aux env e1, aux env e2)
        | Let (str, e1, e2) ->
            let varid = unique_varid () in
            let env' = StrMap.add str varid env in
            Let (varid, aux env e1, aux env' e2)
        | Pair (e1, e2) ->
            Pair (aux env e1, aux env e2)
        | Projection (p, e) -> Projection (p, aux env e)
        | RecordUpdate (e1, l, e2) ->
            RecordUpdate (aux env e1, l, Utils.option_map (aux env) e2)
        | Debug (str, e) -> Debug (str, aux env e)
        in
        (a,e)
    in
    aux id_map e

let rec unannot (_,e) =
    let e = match e with
    | Const c -> Const c
    | Var v  -> Var v
    | Lambda (t, v, e) -> Lambda (t, v, unannot e)
    | RecLambda (s, t, v, e) -> RecLambda (s, t, v, unannot e)
    | InfLambda (t, v, e) -> InfLambda (t, v, unannot e)
    | Ite (e, t, e1, e2) -> Ite (unannot e, t, unannot e1, unannot e2)
    | App (e1, e2) -> App (unannot e1, unannot e2)
    | Let (v, e1, e2) -> Let (v, unannot e1, unannot e2)
    | Pair (e1, e2) -> Pair (unannot e1, unannot e2)
    | Projection (p, e) -> Projection (p, unannot e)
    | RecordUpdate (e1, l, e2) ->
        RecordUpdate (unannot e1, l, Utils.option_map unannot e2)
    | Debug (str, e) -> Debug (str, unannot e)
    in
    ( (), e )

let rec substitute_var v ve (a,e) =
    let e = match e with
    | Const c -> Const c
    | Var v' when v=v' -> snd ve
    | Var v' -> Var v'
    | Lambda (t, v', e) when v=v' -> Lambda (t, v', e)
    | Lambda (t, v', e) -> Lambda (t, v', substitute_var v ve e)
    | RecLambda (s, t, v', e) when v=v' || v=s -> RecLambda (s, t, v', e)
    | RecLambda (s, t, v', e) -> RecLambda (s, t, v', substitute_var v ve e)
    | InfLambda (t, v', e) when v=v' -> InfLambda (t, v', e)
    | InfLambda (t, v', e) -> InfLambda (t, v', substitute_var v ve e)
    | Ite (e, t, e1, e2) -> Ite (substitute_var v ve e, t, substitute_var v ve e1, substitute_var v ve e2)
    | App (e1, e2) -> App (substitute_var v ve e1, substitute_var v ve e2)
    | Let (v', e1, e2) when v=v' -> Let (v', substitute_var v ve e1, e2)
    | Let (v', e1, e2) -> Let (v', substitute_var v ve e1, substitute_var v ve e2)
    | Pair (e1, e2) -> Pair (substitute_var v ve e1, substitute_var v ve e2)
    | Projection (p, e) -> Projection (p, substitute_var v ve e)
    | RecordUpdate (e1, l, e2) ->
        RecordUpdate (substitute_var v ve e1, l, Utils.option_map (substitute_var v ve) e2)
    | Debug (str, e) -> Debug (str, substitute_var v ve e)
    in
    (a,e)

let const_to_typ c =
    match c with
    | Magic -> Cduce.empty
    | Unit -> Cduce.unit_typ
    | EmptyRecord -> Cduce.empty_closed_record
    | Bool true -> Cduce.true_typ
    | Bool false -> Cduce.false_typ
    | Int i -> Cduce.interval (Some i) (Some i)
    | Char c -> Cduce.single_char c
    | Atom t ->
        failwith (Printf.sprintf "Can't retrieve the type of the atom %s." t)

type parser_element =
| Definition of (string * parser_expr)
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
