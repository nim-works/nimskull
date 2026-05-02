discard """
  targets: "c js"
"""

include std/prelude

template main() =
  doAssert toSeq(1..3) == @[1,2,3]
static: main()
main()
