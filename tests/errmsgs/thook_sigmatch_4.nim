discard """
  errormsg: "'=trace' must satisfy the signature 'proc(x: var T, env: pointer)' where T is 'distinct' or 'object'"
  line: 8
"""

type typ = ref object

proc `=trace`(x: var typ, env: pointer) = discard