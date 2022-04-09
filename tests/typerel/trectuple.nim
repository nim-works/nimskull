discard """
  targets: native
  errormsg: "illegal recursion in type 'TNode'"
  line: 10

"""

type
    PNode = ref TNode
    TNode = tuple # comment
      self: PNode # comment
      a, b: int # comment

var node: PNode
new(node)
node.self = node

