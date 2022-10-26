discard """
  errormsg: "illegal recursion in type 'TNode'"
  line: 9

"""

type
    PNode = ref TNode
    TNode = tuple # comment
      self: PNode # comment
      a, b: int # comment

var node: PNode
new(node)
node.self = node

