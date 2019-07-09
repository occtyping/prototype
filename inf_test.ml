atom nil
type IntBoolList = Nil | (Int|Bool, IntBoolList)
type String = Nil | (Char, String)


type DocumentNode = { nodeType=9 ..}
and NodeList = Nil | (Node, NodeList)
and ElementNode = { nodeType=1,
                   childNodes = NodeList
                  ..}

and TextNode = { nodeType=3,
                 isElementContentWhiteSpace=Bool
                ..}
and Node = DocumentNode | ElementNode | TextNode

let is_empty_node =
  fun (x : Node) ->
  if x.nodeType is 9 then false
  else if x.nodeType is 3 then
    x.isElementContentWhiteSpace
  else
  if x.childNodes is Nil then true else false


(* Basic functions *)
let incr =
    fun (Int -> Int) i -> magic

let plus =
    fun (Int -> Int -> Int) i -> magic

let lnot =
    fun ((True -> False) & (False -> True)) x -> magic

let land =
    fun (Bool -> Bool -> Bool) x -> magic

let bti =
  fun (Bool -> Int) b -> magic

(* Basic example (first example of the paper) *)

let basic_ok =
    fun ((Int | Bool) -> Int) x ->
        if x is Int then incr x else bti x

let basic_inf =
  fun (y : Int | Bool) ->
  if y is Int then incr y else lnot y

let any_inf =
  fun (x : Any) ->
  if x is Int then incr x else
  if x is Bool then lnot x else
    x

let map =
  fun (loop : ((Int -> Int)&(Bool -> Bool)) -> IntBoolList -> IntBoolList) f ->
  fun (l : IntBoolList) ->
  if l is Nil then l
  else let h = fst l in
    let t = snd l in
    if h is Int then (basic_inf h, loop f l)
    else loop f l







let is_int =
  fun (x : Any) -> if x is Int then true else false

let is_bool =
  fun (x : Any) -> if x is Bool then true else false

let is_char =
  fun (x : Any) -> if x is Char then true else false

let is_string =
  fun (x : Any) -> if x is String then true else false

let or_ =
  fun (x : Any) ->
  if x is True then (fun (y : Any) -> true)
  else
    fun (y : Any) -> if y is True then true else false

let not_ =
  fun (x : Any) ->
  if x is True then false else true

let and_ =
  fun (x : Any) ->
  fun (y : Any) ->
  if not_ (or_ (not_ x) (not_ y)) is True then true
  else false
(*
let and_ =
  fun (x : Any) ->
  fun (y : Any) ->
  if (x, y) is (True, True) then true
    else false
*)

(* not refined  ?*)
let xor_bad =
  fun (x : Any) ->
  fun (y : Any) ->
  if and_ (or_ x y) (not_ (and_ x y)) is True then true
    else false

let xor_ =
  fun (x : Any) ->
  fun (y : Any) ->
  if x is True then if y is True then false else true
  else if y is True then true else false

let complex =
  fun (x : Any) ->
  fun (y : Any) ->
  if and_ (is_int x) (is_bool y) is True then 1
  else if or_ (is_char x) (is_int y) is True then 2
    else 3

let test_1 = complex 3 true
let test_2 = complex (1,1) 1
let test_3 = complex nil nil











(* Why is it not as precise ? *)

let complex_p =
  fun (p : (Any,Any)) ->
  let x = fst p in
  let y = snd p in
  if and_ (is_int x) (is_bool y) is True then 1
  else if or_ (is_char x) (is_int y) is True then 2
  else 3

let test_1_p = complex_p (3, true)
let test_2_p = complex_p ((1,1), 1)
let test_3_p = complex_p (nil, nil)


let strlen = fun (String -> Int) x -> magic

let test_4 =
  fun (x : Any) ->
  fun (y : Any) ->
  if (if is_int x is True then
     is_string y
      else
        false) is True then
    plus x (strlen y)
  else 0
