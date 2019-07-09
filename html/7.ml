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

let f = fun (x : Any) -> fun (y : Any) ->
  if and_ (is_int x) (is_bool y) is True
  then 1 else
     if or_ (is_char x) (is_int y) is True
     then 2 else 3


let test_1 = f 3 true
let test_2 = f (42,42) 42
let test_3 = f nil nil
