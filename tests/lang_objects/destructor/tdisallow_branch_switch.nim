discard """
  description: '''
    Attempting to switch the branch of a variant object with an overridden
    destructor produces an error
  '''
  cmd: "nim check --msgFormat=sexp --filenames=canonical $options $file"
  nimoutFormat: sexp
  action: reject
"""

type
  Obj = object
    case tag: bool
    of false, true:
      discard

proc `=destroy`(x: var Obj) =
  discard

var o = Obj(tag: false)
o.tag = false #[tt.Error
     ^ (SemCannotAssignToDiscriminantWithCustomDestructor)]#

# the error must also be diagnosed for code not part of the alive graph (i.e.
# for unused procedures)
proc unused() {.used.} = # suppress the warning
  var o = Obj(tag: false)
  o.tag = false #[tt.Error
       ^ (SemCannotAssignToDiscriminantWithCustomDestructor)]#