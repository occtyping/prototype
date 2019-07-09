type Document = { nodeType=9 ..}
and NodeList = Nil | (Node, NodeList)
and Element = { nodeType=1,
                   childNodes = NodeList
                  .. }
and Text = { nodeType=3,
             isElementContentWhiteSpace=Bool
            .. }
and Node = Document | Element | Text

let is_empty_node = fun (x : Node) ->
  if x.nodeType is 9 then false
  else if x.nodeType is 3 then
    x.isElementContentWhiteSpace
  else
  if x.childNodes is Nil then true else false
