discard """
  action: run
  exitcode: 1
  targets: "c cpp"
  disabled: "openbsd"
  disabled: "netbsd"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/9657
    infinite exception loop in sysFatal leads to out-of-memory
  . sysFatal calls write that raises an exception that causes sysfatal to be
    called etc in an infinite loop that eventually eats all memory, when
    stderr is closed (for example when killing a parent process)
  . And here's the culprit:
    https://github.com/nim-lang/Nim/blob/e844e536bf29d3b18333d7ed4c53bb54b75a2d75/lib/system/excpt.nim#L21
  . I think the error message writer should be annotated with {.raises: [].}...

  . From https://github.com/nim-lang/Nim/issues/10343
    nim cpp -r tests/exception/t9657 hangs
 '''
"""

close stdmsg
const m = "exception!"
# see #10343 for details on this test
discard writeBuffer(stdmsg, cstring(m), m.len)