discard """
  description: '''
    Tests for detection of illegal captures across the compile-/run-time border
  '''
  cmd: "nim check --msgFormat:sexp --filenames=canonical $options $file"
  nimoutformat: sexp
  action: reject
"""

block:
  # attempting to capture a run-time location inside a compile-time context is
  # an error
  proc outer() =
    var x = 0
    proc inner() {.compileTime.} =
      discard x #[tt.Error
             ^ (SemIllegalCompTimeCapture)]#

    proc inner3() {.compileTime.} =
      proc innerInner() =
        discard x #[tt.Error
               ^ (SemIllegalCompTimeCapture)]#

block:
  proc outer() {.compileTime.} =
    proc innerInner() = # a compile-time procedure that's not explicitly
                        # marked as such
      var y = 0
      proc innerInnerInner() {.compileTime.} =
        discard y # legal; `innerInner` is also a compile-time procedure