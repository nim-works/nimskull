discard """
  targets: "c cpp"
"""

import std/oids


block: # genOid
  let x = genOid()
  doAssert ($x).len == 24
