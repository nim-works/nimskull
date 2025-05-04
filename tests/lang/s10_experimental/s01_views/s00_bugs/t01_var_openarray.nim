discard """
description: '''
`var openarray` is allowed as the type of a `var` binding, but it's unclear
whether this is going to stay
'''
knownIssue.vm: "`toOpenArray` is not yet supported"
"""

{.experimental: "views".}

## feature-documentation

block view_regular_array:
  var simpleArray = @[0, 1, 2, 3, 4, 5]

  var view: var openarray[int] = toOpenArray(simpleArray, 1, 3)

  doAssert view[0] == 1
