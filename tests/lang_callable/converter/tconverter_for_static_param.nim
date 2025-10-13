discard """
  description: '''
    Ensure that converters are considered for arguments to static parameters
  '''
"""

converter toInt(x: float): int = int(x)

proc test(x: static int) =
  doAssert x == 1

test(1.5) # wouldn't work without a converter
