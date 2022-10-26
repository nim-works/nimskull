discard """
description: "can't mix pragmas and tuple unpacking"
errormsg: "pragmas are disallowed during tuple unpacking assignment, this is a know design issue; as a work around split pragmas and unpacking into two steps."
line: 12
"""

import macros

macro foo(l, t, e: untyped): untyped =
  discard

var (x {.foo.}, b) = (1, 2)