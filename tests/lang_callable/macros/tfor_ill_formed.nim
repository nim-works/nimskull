discard """
  cmd: "nim check --msgFormat=sexp --filenames=canonical --hints:off $options $file"
  action: reject
  nimoutFormat: sexp
"""
for i in (if true: discard else: discard): #[tt.Error
        ^ (SemExpressionHasNoType) ]#
  discard
