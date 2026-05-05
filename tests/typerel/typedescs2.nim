discard """
  errormsg: "invalid type: 'typedesc[Table]' for const"
  file: "typedescs2.nim"
  line: 16
"""

# bug #9961

import std/typetraits
import std/tables

proc test(v: typedesc) =
  echo v.type.name

# This crashes the compiler
const b: typedesc = Table
test b
