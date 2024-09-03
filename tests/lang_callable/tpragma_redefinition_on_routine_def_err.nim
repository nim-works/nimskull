discard """
  description: "Error when routine def has pragma not in prototype"
  errormsg: "pragmas are only allowed in the header of a proc; redefinition of 'a' from"
  line: 8
"""

proc a()
proc a() {.gcsafe.} = discard