discard """
description: '''
`var openarray` must either generate a proper error message or be allowed,
currently fails with codegen error
'''
knownIssue: "var openarray fails with C codegen error"

"""

{.experimental: "views".}

## feature-documentation

block view_regular_array:
  var simpleArray = @[0, 1, 2, 3, 4, 5]

  var view: var openarray[int] = toOpenArray(simpleArray, 1, 3)

  doAssert view[0] == 1
