discard """
  targets: "c"
  matrix: "-d:nimStacktraceOverride"
  output: '''begin
Traceback (most recent call last, using override)
Error: unhandled exception: stack trace produced [ValueError]
'''
  exitcode: 1
"""

# xxx: this doesn't work in cpp

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
