discard """
  output: ''''''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:tfor --hint:Performance:off $file'''
  nimout: '''--expandArc: main

scope:
  def a: seq[seq[int]]
  def b: seq[seq[int]]
  def x: seq[int] = f()
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
    def x: seq[int] = f()
    scope:
      def a: int = 0
      def b: int = 4
      def i: int = a
      block L0:
        scope:
          while true:
            scope:
              def_cursor _0: int = i
              def_cursor _1: int = b
              def _2: bool = <(arg _0, arg _1)
              def _3: bool = not(arg _2)
              if _3:
                scope:
                  break L0
              scope:
                scope:
                  def_cursor i: int = i
                  def_cursor _4: int = i
                  def _5: bool = ==(arg _4, arg 2)
                  if _5:
                    scope:
                      return
                  def _6: seq[int]
                  =copy(name _6, arg x)
                  add(name a, consume _6)
                inc(name i, arg 1)
    block L1:
      if cond:
        scope:
          def _7: seq[int] = x
          wasMoved(name x)
          add(name a, consume _7)
          break L1
      scope:
        def _8: seq[int] = x
        wasMoved(name x)
        add(name b, consume _8)
  finally:
    =destroy(name x)
    =destroy(name b)
    =destroy(name a)
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
