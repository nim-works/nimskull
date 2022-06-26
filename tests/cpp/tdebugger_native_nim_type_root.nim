discard """
  targets: "cpp"
  cmd: "nim $target --debugger:native $options $file"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/9013
    debugger:native is currently unusable, coupled with gc and types diagnostics
  . https://github.com/nim-lang/Nim/pull/9015
    Fix linking issue in cpp codegen: Declare the root symbol only once and
    have the other modules depending on it emit an `extern` declaration.
  '''
"""

# The --debugger switch is needed in order to enable the defined(nimTypeNames)
# code path in hti.nim
import typeinfo
var tt: Any