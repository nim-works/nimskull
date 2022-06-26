discard """
  matrix: "--gc:refc; --gc:arc"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/16607
    Constructing a named tuple of a requiresInit type causes destructions
    before initialization
  '''
"""

type
  O {.requiresInit.} = object
    initialized: bool

proc `=destroy`(o: var O) =
  doAssert o.initialized, "O was destroyed before initialization!"

proc initO(): O =
  O(initialized: true)

proc pair(): tuple[a, b: O] =
  result.a = initO()
  result.b = initO()

proc main() =
  discard pair()

main()