discard """
  errormsg: "'=copy' must satisfy the signature 'proc(x: var T, y: T)' where T is 'distinct' or 'object'"
  line: 8
"""

type typ = ref object

proc `=copy`(dest: var typ, src: typ) = discard