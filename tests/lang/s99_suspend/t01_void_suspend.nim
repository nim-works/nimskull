discard """
  description: '''
    The type provided to a 'suspend' may be void, in which case the 'suspend'
    is a void expression and the continuation has no extra parameter.
  '''
  output: "done\n"
"""

proc test() =
  suspend void, cont:
    cont[1](cont[0])
  echo "done"

test()
