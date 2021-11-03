discard """
description: '''
Unpacking tuple into wrong number of variables will cause `Error: wrong number of variables`.

This can be fixed by using placeholder `_` to increase number of elements in target assignment.
'''

errormsg: "wrong number of variables"
"""

## No error here - assigning three elements to three targets
let (a, b, _) = (12, 3, 4)

let (a, b) = (12, 3, 4)