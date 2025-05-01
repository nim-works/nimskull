discard """
  errormsg: "'=destroy' must satisfy the signature 'proc(x: var T)' where T is 'distinct' or 'object'"
  line: 8
"""

type typ = ref object

proc `=destroy`(x: var typ) = discard