discard """
  errormsg: "'=sink' must satisfy the signature 'proc(x: var T, y: T)' where T is 'distinct' or 'object'"
  line: 8
"""

type typ = ref object

proc `=sink`(x: var typ, y: typ) = discard