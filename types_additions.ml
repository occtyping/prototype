
open Cduce

(* Construction of types *)

type type_base =
    TInt of int option * int option
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty

type type_expr =
| TBase of type_base
| TCustom of string
| TPair of type_expr * type_expr
| TRecord of bool * (string * type_expr * bool) list
| TArrow of type_expr * type_expr
| TCup of type_expr * type_expr
| TCap of type_expr * type_expr
| TDiff of type_expr * type_expr
| TNeg of type_expr

module StrMap = Map.Make(String)
type type_env = node StrMap.t

let empty_tenv = StrMap.empty

let type_base_to_typ t =
    match t with
    | TInt (lb,ub) -> Cduce.interval lb ub
    | TBool -> Cduce.bool_typ
    | TTrue -> Cduce.true_typ | TFalse -> Cduce.false_typ
    | TUnit -> Cduce.unit_typ | TChar -> Cduce.char_typ
    | TAny -> Cduce.any | TEmpty -> Cduce.empty

let type_expr_to_typ env t =
    let rec aux t =
        match t with
        | TBase tb -> cons (type_base_to_typ tb)
        | TCustom k ->
            (try StrMap.find k env with Not_found -> failwith (Printf.sprintf "Type %s undefined!" k))
        | TPair (t1,t2) -> cons (mk_times (aux t1) (aux t2))
        | TRecord (is_open, fields) ->
            let aux' (label,t,opt) =
                let t = descr (aux t) in
                (label, cons (if opt then or_absent t else t))
            in
            let fields = List.map aux' fields in
            cons (mk_record is_open fields)
        | TArrow (t1,t2) -> cons (mk_arrow (aux t1) (aux t2))
        | TCup (t1,t2) ->
            let t1 = descr (aux t1) in
            let t2 = descr (aux t2) in
            cons (cup t1 t2)
        | TCap (t1,t2) ->
            let t1 = descr (aux t1) in
            let t2 = descr (aux t2) in
            cons (cap t1 t2)
        | TDiff (t1,t2) ->
            let t1 = descr (aux t1) in
            let t2 = descr (aux t2) in
            cons (diff t1 t2)
        | TNeg t -> cons (neg (descr (aux t)))
    in descr (aux t)

let define_atom env atom =
    let atom = String.capitalize_ascii atom in
    if StrMap.mem atom env
    then failwith (Printf.sprintf "Atom %s already defined!" atom)
    else StrMap.add atom (cons (mk_atom atom)) env

let define_types env defs =
    let declare_type env (name,_) =
        if StrMap.mem name env
        then failwith (Printf.sprintf "Type %s already defined!" name)
        else StrMap.add name (mk_new_typ ()) env
    in
    let env = List.fold_left declare_type env defs in
    let define_type (name,decl) =
        let t = type_expr_to_typ env decl in
        define_typ (StrMap.find name env) t
    in
    (* TODO: normalize? *)
    List.iter define_type defs ; env

let get_atom env atom =
    let atom = String.capitalize_ascii atom in
    try descr (StrMap.find atom env)
    with Not_found -> failwith (Printf.sprintf "Atom %s undefined!" atom)

(* Operations on types *)

let conj ts = List.fold_left cap any ts
let disj ts = List.fold_left cup empty ts

let square_approx f out =
    let dnf = dnf f in
    let res = dnf |> List.map begin
        fun lst ->
            let is_impossible (_,t) = is_empty (cap out t) in
            let impossibles = List.filter is_impossible lst |> List.map fst in
            neg (disj impossibles)
    end in
    cap (domain f) (disj res)

let rec take_one lst =
    match lst with
    | [] -> []
    | e::lst ->
        (e, lst)::(List.map (fun (e',lst) -> (e',e::lst)) (take_one lst))

let square_exact f out =
    let dnf = dnf f in
    let res = dnf |> List.map begin
        fun lst ->
            let rec impossible_inputs current_set lst =
                let t = List.map snd current_set in
                if subtype out (neg (conj t)) then [conj (List.map fst current_set)]
                else begin
                    let aux (e,lst) = impossible_inputs (e::current_set) lst in
                    List.flatten (List.map aux (take_one lst))
                end
            in
            neg (disj (impossible_inputs [] lst))
    end in
    cap (domain f) (disj res)

let square = square_exact (* You can switch between square_exact and square_approx *)
