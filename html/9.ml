let not_ = fun (x : Any) ->
 if x is True then false else true

let or_ = fun (x : Any) ->
 if x is True then (fun (y : Any) -> true)
 else fun (y : Any) ->
      if y is True then true else false

let and_ = fun (x : Any) -> fun (y : Any) ->
  if not_ (or_ (not_ x) (not_ y)) is True
  then true else false

let xor_ = fun (x : Any) -> fun (y : Any) ->
 if and_ (or_ x y) (not_ (and_ x y)) is True
 then true else false
