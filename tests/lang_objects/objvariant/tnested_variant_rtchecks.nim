discard """
  description: "Ensure that the outer variant access is checked first"
  exitcode: 1
  outputsub: '''
    field 'a' is not accessible for type 'Nested' using 'outer = false'
  '''
"""

type
  Nested = object
    case outer: bool
    of true:
      case inner: bool
      of true:
        a: int
      of false:
        discard
    of false:
      discard

var x = Nested(outer: false)
# field 'a' is part of a nested, non-active variant object
doAssert x.a == 0