discard """
  matrix: "-d:release"
  outputsub: '''tunhandledexc.nim(16)    genErrors
Error: unhandled exception: bla [ESomeOtherErr]'''
  exitcode: "1"
  knownIssue.vm: "the stacktrace points to the wrong line"
"""
type
  ESomething = object of Exception
  ESomeOtherErr = object of Exception

proc genErrors(s: string) =
  if s == "error!":
    raise newException(ESomething, "Test")
  else:
    raise newException(EsomeotherErr, "bla")

when true:
  try: discard except: discard

  try:
    genErrors("errssor!")
  except ESomething:
    echo("Error happened")
