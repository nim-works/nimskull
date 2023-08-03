discard """
  description: '''
    The lvalue passed to a ``var`` parameter is considered to be read/used when
    control-flow reaches the call, not when it reaches the argument expression
  '''
  action: reject
  matrix: "--filenames=canonical --msgFormat=sexp"
  targets: "c js vm"
  nimoutFormat: sexp
"""

type Obj = object

proc `=copy`(x: var Obj, y: Obj) {.error.}

proc f_sink(x: var Obj, y: sink Obj) =
  discard

proc main() =
  var s = Obj()
  f_sink(s, s) #[tt.Error
           ^ (SemUnavailableTypeBound) (:str "=copy")]#

main()