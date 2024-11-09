discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:traverse --hint:Performance:off $file'''
  nimout: '''--expandArc: traverse

scope:
  def_cursor it: Node = root
  scope:
    while true:
      scope:
        def_cursor _4: Node = it
        def _5: bool = eqRef(arg _4, arg nil)
        def :tmp: bool = not(arg _5)
        scope:
          def_cursor _6: bool = :tmp
          def _7: bool = not(arg _6)
          if _7:
            scope:
              goto [L2]
        def_cursor _8: Node = it
        def_cursor _9: string = _8[].s
        echo(arg type(array[0..0, string]), arg _9) -> [Resume]
        def_cursor _10: Node = it
        it = _10[].ri
  L2:
  def jt: Node
  =copy(name jt, arg root)
  scope:
    while true:
      scope:
        def_cursor _13: Node = jt
        def _14: bool = eqRef(arg _13, arg nil)
        def :tmp: bool = not(arg _14)
        scope:
          def_cursor _15: bool = :tmp
          def _16: bool = not(arg _15)
          if _16:
            scope:
              goto [L5]
        def_cursor _18: Node = jt
        def ri: Node
        =copy(name ri, arg _18[].ri)
        def_cursor _19: Node = jt
        def_cursor _20: string = _19[].s
        echo(arg type(array[0..0, string]), arg _20) -> [L6]
        =sink(name jt, arg ri)
        goto [L7]
        finally (L6):
          =destroy(name ri)
          continue [L8]
        L7:
  L5:
  =destroy(name jt)
  goto [L9]
  finally (L8):
    =destroy(name jt)
    continue [Resume]
  L9:

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
