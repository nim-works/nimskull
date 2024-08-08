discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:tfor --expandArc:texit --hint:Performance:off $file'''
  nimout: '''--expandArc: main

scope:
  def a: seq[seq[int]]
  def b: seq[seq[int]]
  def x: seq[int] = f() -> [L0, Resume]
  scope:
    if cond:
      scope:
        def _5: seq[int] = move x
        add(name a, consume _5)
        goto [L2]
  scope:
    def _6: seq[int] = move x
    add(name b, consume _6)
  goto [L0, L3]
  finally (L0):
    =destroy(name b)
    =destroy(name a)
    continue {L3}
  L3:
-- end of expandArc ------------------------
--expandArc: tfor

scope:
  def a: seq[seq[int]]
  def b: seq[seq[int]]
  def x: seq[int] = f() -> [L0, Resume]
  scope:
    def a: int = 0
    def b: int = 4
    def i: int = copy a
    scope:
      while true:
        scope:
          def_cursor _9: int = i
          def :tmp: bool = ltI(arg _9, arg b)
          scope:
            def_cursor _10: bool = :tmp
            def _11: bool = not(arg _10)
            if _11:
              scope:
                goto [L3]
          scope:
            def_cursor i: int = i
            scope:
              def _13: bool = eqI(arg i, arg 2)
              if _13:
                scope:
                  goto [L5, L0, L6]
            def _14: seq[int]
            =copy(name _14, arg x)
            add(name a, consume _14)
          i = addI(arg i, arg 1) -> [L5, L0, Resume]
    L3:
  scope:
    if cond:
      scope:
        def _15: seq[int] = move x
        wasMoved(name x)
        add(name a, consume _15)
        goto [L8]
  scope:
    def _16: seq[int] = move x
    wasMoved(name x)
    add(name b, consume _16)
  L8:
  goto [L5, L0, L9]
  finally (L5):
    =destroy(name x)
    continue {L0}
  finally (L0):
    =destroy(name b)
    =destroy(name a)
    continue {L6, L9}
  L9:
L6:
-- end of expandArc ------------------------
--expandArc: texit
scope:
  def str: string
  def x: string = boolToStr(arg cond)
  scope:
    if cond:
      scope:
        goto [L1, L2]
  def _4: string = boolToStr(arg cond)
  str := move _4
  scope:
    def _5: bool = not(arg cond)
    if _5:
      scope:
        result := move str
        wasMoved(name str)
        goto [L1, L2]
  goto [L1, L4]
  finally (L1):
    =destroy(name x)
    =destroy(name str)
    continue {L2, L4}
  L4:
L2:
-- end of expandArc ------------------------'''
"""

proc f(): seq[int] =
  @[1, 2, 3]

proc main(cond: bool) =
  var a, b: seq[seq[int]]
  var x = f()
  if cond:
    a.add x
  else:
    b.add x

# all paths move 'x' so no wasMoved(x); destroy(x) pair should be left in the
# AST.

main(false)


proc tfor(cond: bool) =
  var a, b: seq[seq[int]]

  var x = f()

  for i in 0 ..< 4:
    if i == 2: return
    a.add x

  if cond:
    a.add x
  else:
    b.add x

tfor(false)

proc texit(cond: bool): string =
  var str: string
  let x = $cond # starts initialized and requires destruction

  if cond:
    return # make sure `x` escapes

  str = $cond # start `str`'s lifetime

  if not cond:
    result = str # `str` can be moved (str's lifetime ends)
    return # unstructured exit
  # there are no unstructured exits of `str`'s scope where `str` is alive

discard texit(false)