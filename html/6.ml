let is_int = fun (x : Any) ->
 if x is Int then true else false

let is_bool = fun (x : Any) ->
 if x is Bool then true else false

let is_char = fun (x : Any) ->
 if x is Char then true else false

let not_ = fun (x : Any) ->
 if x is True then false else true

let or_ = fun (x : Any) ->
 if x is True then (fun (y : Any) -> true)
 else fun (y : Any) ->
      if y is True then true else false

let and_ = fun (x : Any) -> fun (y : Any) ->
  if not_ (or_ (not_ x) (not_ y)) is True
  then true else false
