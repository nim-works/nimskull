discard """
matrix: "--nimcache:build/myNimCache --nimblePath:myNimblePath --passc:'-fmax-errors=4' --passl:'-Xlinker --defsym -Xlinker dummysymboldoesnotexist=0x0'"
joinable: false
"""

import std/[strutils, compilesettings]
from std/os import fileExists, `/`

template main =
  doAssert querySetting(nimcacheDir) == nimcacheDir.querySetting
  doAssert "myNimCache" in nimcacheDir.querySetting
  doAssert "myNimblePath" in nimblePaths.querySettingSeq[0]
  doAssert querySetting(backend) == "c"
  doAssert fileExists(libPath.querySetting / "system.nim")
  doAssert "-fmax-errors=4" in querySetting(compileOptions)
  doAssert "-Xlinker --defsym -Xlinker dummysymboldoesnotexist=0x0" in
              querySetting(linkOptions)

static: main()
main()
