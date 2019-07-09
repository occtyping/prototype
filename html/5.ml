let or_ = fun (x : Any) ->
 if x is True then (fun (y : Any) -> true)
 else fun (y : Any) ->
      if y is True then true else false
