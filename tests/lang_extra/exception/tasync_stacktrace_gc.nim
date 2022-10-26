discard """
  matrix: "--gc:arc; --gc:refc"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/18620
    Stacktrace when using async with arc/orc is different than
    when using refc
  . The stacktrace returned by an async program compiled with arc/orc is
    different than one compiled with refc. Most importantly,
    the stacktrace with arc/orc gives little help in finding out
    where the problem is.
  '''
"""

proc hello() =
  raise newException(ValueError, "You are wrong")

var flag = false

try:
  hello()
except ValueError as e:
  flag = true
  doAssert len(getStackTraceEntries(e)) > 0
  doAssert len(getStackTraceEntries(e)) > 0

doAssert flag