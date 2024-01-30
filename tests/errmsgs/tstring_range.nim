discard """
  errormsg: "cannot create a range of strings"
  line: 8
"""

var x = "a"
case x
of "a".."def":
  discard
