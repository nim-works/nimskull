discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/12223
    except Exception as e: doesn't work with nim js
  . var z: seq[string] should behave identical to var z: seq[string] = @[]
    (to make it consistent with libraries written for nim c) but does not
'''
"""

proc fun() =
  var z: seq[string]
  discard z[4]

proc main()=
  try:
    fun()
    doAssert false
  except Exception as e:
    # except without alias sidesteps the bug.
    const expectedMessage = "index out of bounds, the container is empty"
    doAssert getCurrentExceptionMsg() == expectedMessage

main()
