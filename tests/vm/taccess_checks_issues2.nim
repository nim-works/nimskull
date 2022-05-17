discard """
  description: "Valid usages that are reported as an access violation"
  action: compile

  knownIssue: '''array overlays don't work due to address validation being too
                 eager with them'''
"""

import maccess_checks

# XXX: once these tests don't fail anymore, move them to
#      `tsafety_checks_accept`


objectTest(array[4, int32]):
  doAssert p[][0] == 3

static:
  var s = @[1, 2, 3, 4]
  let p = s[0].asPtr(array[5, int])
  doAssert p[][0] == 1
  doAssert p[][1] == 2
  doAssert p[][2] == 3
  doAssert p[][3] == 4