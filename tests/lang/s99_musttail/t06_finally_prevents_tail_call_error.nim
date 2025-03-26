discard """
  action: reject
"""

proc p() {.musttail.} = discard

proc test() {.musttail.} =
  try:
    discard
  except:
    p() # would work when there's no finally
  finally:
    discard
