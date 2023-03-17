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

block close_over_runtime_location:
  proc outer() {.compileTime.} =
    proc inner() =
      var y = 0
      proc innerInner() {.compileTime.} =
        discard y #[tt.Error
               ^ (SemIllegalCompTimeCapture)]#

block capture_across_non_compile_time_proc:
  proc outer() {.compileTime.} =
    var x = 0
    proc inner() =
      proc innerInner() {.compileTime.} =
        # closing over `x` would be legal if `inner` is also a compile-time-
        # only procedure
        discard x #[tt.Error
               ^ (SemIllegalCompTimeCapture)]#

block inner_macro:
  proc outer() =
    var x = 0
    macro m() =
      discard x #[tt.Error
             ^ (SemIllegalCompTimeCapture)]#

      proc inner() =
        discard x #[tt.Error
               ^ (SemIllegalCompTimeCapture)]#

      proc inner2() {.compileTime.} =
        discard x #[tt.Error
               ^ (SemIllegalCompTimeCapture)]#