
open Ast

type dir =
    | LApp | RApp | LPair | RPair | PFst | PSnd | Dbg of string
    | RProj of string | LRecord of string | RRecord of string

type path = dir list

val global_n0 : unit -> int
val set_global_n0 : int -> unit

exception Invalid_path

val follow_path : ('c, 'b, 'a) t -> path -> ('c, 'b, 'a) t

type env = typ ExprMap.t
val empty_env : env
val is_bottom : env -> bool

val add_atoms_to_env : env -> string list -> Types_additions.type_env -> env

type logs_data = { ignored:int ; visited:int ; position:Position.t }
val get_logs : exprid -> logs_data option
val all_logs : unit -> (exprid * logs_data) Seq.t
val clear_logs : unit -> unit

exception Ill_typed of Position.t * string

val back_typeof : env -> annot_expr -> typ -> path -> typ (* Equivalent to Env in the paper *)
val typeof : env -> annot_expr -> typ
val refine_env : env -> annot_expr -> typ -> env
