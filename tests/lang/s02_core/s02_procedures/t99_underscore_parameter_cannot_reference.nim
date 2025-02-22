discard """
  description: '''
    An underscore parameter cannot be reference in named parameter passing.
  '''
  action: reject
"""

proc test(_: int) = discard
test(_ = 1)
