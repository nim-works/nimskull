discard """
  description: '''
    Ensure that the correct except branch is jumped to after exiting a try
    block through unstructured, but non-exceptional, control-flow.
  '''
"""

proc test(doExit: bool): bool =
  try:
    block exit:
      try:
        if doExit:
          break exit
        else:
          discard "fall through"
      except ValueError:
        doAssert false, "unreachable"

    # the above except branch previously caught the exception
    raise ValueError.newException("a")
  except ValueError as e:
    doAssert e.msg == "a"
    result = true

doAssert test(true)
doAssert test(false)