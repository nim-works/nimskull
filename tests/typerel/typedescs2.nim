discard """
  targets: native
  errormsg: "invalid type: 'typedesc[Table]' for const"
  file: "typedescs2.nim"
  line: 17
"""

# bug #9961

import typetraits
import tables

proc test(v: typedesc) =
  echo v.type.name

# This crashes the compiler
const b: typedesc = Table
test b
