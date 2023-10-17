discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:traverse --hint:Performance:off $file'''
  nimout: '''--expandArc: traverse

var it_cursor = root
block :label_0:
  while true:
    if not(not(==(it_cursor, nil))):
      break :label_0
    echo([it_cursor[].s])
    it_cursor = it_cursor[].ri
var jt_cursor = root
block :label_0:
  while true:
    if not(not(==(jt_cursor, nil))):
      break :label_0
    var ri_1_cursor = jt_cursor[].ri
    echo([jt_cursor[].s])
    jt_cursor = ri_1_cursor
-- end of expandArc ------------------------'''
"""

type
  Node = ref object
    le, ri: Node
    s: string

proc traverse(root: Node) =
  var it = root
  while it != nil:
    echo it.s
    it = it.ri

  var jt = root
  while jt != nil:
    let ri = jt.ri
    echo jt.s
    jt = ri

traverse(nil)

# XXX: This optimization is not sound
