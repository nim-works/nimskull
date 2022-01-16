discard """
  errormsg: "return type 'typed' is only valid for macros and templates"
  line: 6
"""

proc fun(x:typed)=discard
fun(10)
