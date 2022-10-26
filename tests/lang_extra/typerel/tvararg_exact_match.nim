discard """
  errormsg: "cannot convert array[0..0, string] to varargs[string]"
  line: 20
  description: '''
  . From https://github.com/nim-lang/Nim/issues/8172
    varargs with array ignores leading arguments
  . The problem here is that varargs[string] and array[2, string] are
    linked by a isConvertible relationship and matchesAux is confused by this:
    it first creates a nkBracket with "a" in it and then overwrites it with
    the second argument.
  . https://github.com/nim-lang/Nim/pull/8186
    Error out if vararg match isn't an exact one
  '''
"""
proc f(v: varargs[string]) =
  echo(v)

f("b", "c")   # Works
f(["b", "c"]) # Works
f("b", ["c"]) # Fails

