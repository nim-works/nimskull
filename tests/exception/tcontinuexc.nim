discard """
  outputsub: '''E
C
caught'''
  exitcode: "1"
"""
type
  ESomething = object of Exception
  ESomeOtherErr = object of Exception

proc genErrors(s: string) =
  if s == "error!":
    raise newException(ESomething, "Test")
  else:
    raise newException(EsomeotherErr, "bla")

try:
  for i in 0..3:
    try:
      genErrors("error!")
    except ESomething:
      echo "E"
    echo "C"
    raise newException(EsomeotherErr, "bla")
finally:
  echo "caught"

#OUT ECcaught
