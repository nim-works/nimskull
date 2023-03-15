discard """
  description: '''
    The '.compileTime' pragma is only allowed for globals or with location
    declarations in compile-time-only contexts
  '''
  nimoutformat: "sexp"
  cmd: "nim check --filenames=canonical --msgFormat=sexp $options $file"
"""

block non_compiletime_only_procedure:
  proc p() =
    var local {.compileTime.} = 1 #[tt.Error
             ^ (SemIllegalCompileTime)]#
