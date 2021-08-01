discard """
  cmd: "nim c -d:nimStacktraceOverride $file"
  output: '''begin
Traceback (most recent call last, using override)
Error: unhandled exception: stack trace produced [ValueError]
'''
  exitcode: 1
"""

proc `$`*(stackTraceEntries: seq[StackTraceEntry]): string =
  when defined(nimStackTraceOverride):
    result = """
begin
Traceback (most recent call last, using override)
Error: unhandled exception: stack trace produced [ValueError]
"""
  else:
    result = "something broke"

proc main =
  echo "begin"
  if true:
    raise newException(ValueError, "stack trace produced")
  echo "unreachable"

main()
