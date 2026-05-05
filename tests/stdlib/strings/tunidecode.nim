discard """
  cmd: "nim $target --hints:on -d:embedUnidecodeTable $options $file"
"""

import std/unidecode

loadUnidecodeTable("lib/pure/unidecode/unidecode.dat")

doAssert unidecode("北京") == "Bei Jing "
doAssert unidecode("Äußerst") == "Ausserst"
