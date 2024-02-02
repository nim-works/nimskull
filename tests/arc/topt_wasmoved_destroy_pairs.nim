discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:tfor --expandArc:texit --hint:Performance:off $file'''
  nimout: '''--expandArc: main

scope:
  def a: seq[seq[int]]
  def b: seq[seq[int]]
  def x: seq[int] = f() (raises)
  block L0:
    if cond:
      scope:
        def _0: seq[int] = x
        add(name a, consume _0)
        break L0
    scope:
      def _1: seq[int] = x
      add(name b, consume _1)
  =destroy(name b)
  =destroy(name a)
-- end of expandArc ------------------------
--expandArc: tfor

scope:
  try:
    def a: seq[seq[int]]
    def b: seq[seq[int]]
    def x: seq[int] = f() (raises)
    scope:
      def a: int = 0
      def b: int = 4
      def i: int = a
      block L0:
        scope:
          while true:
            scope:
              def_cursor _0: int = i
              def_cursor _1: bool = ltI(arg _0, arg b)
              def_cursor _2: bool = not(arg _1)
              if _2:
                scope:
                  break L0
              scope:
                scope:
                  def_cursor i: int = i
                  def_cursor _3: bool = eqI(arg i, arg 2)
                  if _3:
                    scope:
                      return
                  def _4: seq[int]
                  =copy(name _4, arg x)
                  add(name a, consume _4)
                i = addI(arg i, arg 1)
    block L1:
      if cond:
        scope:
          def _5: seq[int] = x
          wasMoved(name x)
          add(name a, consume _5)
          break L1
      scope:
        def _6: seq[int] = x
        wasMoved(name x)
        add(name b, consume _6)
  finally:
    =destroy(name x)
    =destroy(name b)
    =destroy(name a)
-- end of expandArc ------------------------
--expandArc: texit
scope:
  try:
    def str: string
    def x: string = boolToStr(arg cond)
    if cond:
      scope:
        return
    str = boolToStr(arg cond)
    def_cursor _0: bool = not(arg cond)
    if _0:
      scope:
        result = str
        wasMoved(name str)
        return
  finally:
    =destroy(name x)
    =destroy(name str)
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