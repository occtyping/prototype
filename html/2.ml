let incr =
    fun (Int -> Int) i -> magic

let lnot =
    fun ((True -> False) & (False -> True)) x -> magic

let any_inf = fun (x : Any) ->
  if x is Int then incr x else
  if x is Bool then lnot x else x
