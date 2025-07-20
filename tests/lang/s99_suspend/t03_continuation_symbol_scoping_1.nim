discard """
  description: '''
    The local introduced by the `suspend` is part of the suspend block's
    new scope.
  '''
"""

proc test() =
  var cont = 0
  suspend void, cont:
    discard
