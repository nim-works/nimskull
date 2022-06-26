discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/14153
    JS: no bound checks at top level
  . This only happens on top level, it works inside proc main or similar.
    --boundchecks:on doesn't help.
  . jsgen.genArrayAccess checks for optBoundsCheck in p.options which
    evaluates to false at top level and true anywhere else.
'''

"""

var x = @[1, 2, 3]

try:
  echo x[5]
  doAssert false
except IndexError:
  doAssert getCurrentExceptionMsg() == "index 5 not in 0 .. 2"
except:
  doAssert false

try:
  x[5] = 8
  doAssert false
except IndexError:
  doAssert getCurrentExceptionMsg() == "index 5 not in 0 .. 2"
except:
  doAssert false

