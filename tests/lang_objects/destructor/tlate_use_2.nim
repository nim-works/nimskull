discard """
  targets: "c js vm"
  description: '''
    The lvalue passed to the first parameter of a is considered to be used
    when control-flow enters the call, not when it reaches the address of the
    lvalue is computed at the callsite
  '''
"""

type Obj = object

proc `=copy`(x: var Obj, y: Obj) {.error.}

proc f_sink(x: Obj, y: sink Obj): lent Obj =
  result = x

proc main() =
  var s = Obj()
  discard f_sink(s, s) #[tt.Error
                    ^ '=copy' is not available for type <Obj>]#

main()