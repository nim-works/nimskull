discard """
  description: "Test for `std/compilesettings` querying of linker options"
  matrix: "--passl:'-Xlinker --defsym -Xlinker dummysymboldoesnotexist=0x0'"
  joinable: false
  disabled: windows
  disabled: osx
"""

# finding a "no-op" like cross-platform linker option turned out to be a pain,
# so doing some basic testing on linux at least.

import std/[strutils, compilesettings]
from std/os import fileExists, `/`

template main =
  doAssert "-Xlinker --defsym -Xlinker dummysymboldoesnotexist=0x0" in
              querySetting(linkOptions)

static: main()
main()
