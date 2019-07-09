(* Basic builtin functions *)
let incr =
    fun (Int -> Int) i -> magic

let lnot =
    fun ((True -> False) & (False -> True)) x -> magic


let basic_inf = fun (y : Int | Bool) ->
  if y is Int then incr y else lnot y
