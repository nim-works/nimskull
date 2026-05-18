discard """
  errormsg: "undeclared identifier: '%'"
  line: 9
"""

import std/strutils except `%`

# doesn't work
echo "$1" % "abc"
