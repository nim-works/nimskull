discard """
  output: '''("string here", 80)'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:sio --hint:Performance:off $file'''
  nimout: '''--expandArc: main

scope:
  def_cursor x: (string, int) = <D0>
  scope:
    if cond:
      scope:
        x = <D1>
        goto [L1]
  scope:
    x = <D2>
  L1:
  def_cursor _3: (string, int) = x
  def _4: string = $(arg _3) -> [Resume]
  echo(arg type(array[0..0, string]), arg _4) -> [L2, Resume]
  goto [L2, L3]
  finally (L2):
    =destroy(name _4)
    continue {L3}
  L3:
-- end of expandArc ------------------------
--expandArc: sio

scope:
  scope:
    def_cursor filename: string = "debug.txt"
    def_cursor _3: string = filename
    def f: File = open(arg _3, arg fmRead, arg 8000) -> [Resume]
    scope:
      def res: string = newStringOfCap(arg 80)
      scope:
        while true:
          scope:
            def_cursor _6: File = f
            def :tmp: bool = readLine(arg _6, name res) -> [L1, L2, Resume]
            scope:
              def_cursor _7: bool = :tmp
              def _8: bool = not(arg _7)
              if _8:
                scope:
                  goto [L4]
            scope:
              def_cursor x: string = res
              def_cursor _10: string = x
              echo(arg type(array[0..0, string]), arg _10) -> [L1, L2, Resume]
      L4:
      goto [L1, L2, L5]
      finally (L1):
        =destroy(name res)
        continue {L2}
    finally (L2):
      scope:
        def_cursor _11: File = f
        close(arg _11) -> [Leave(L2), Resume]
      continue {L5}
    L5:

-- end of expandArc ------------------------'''
"""

proc main(cond: bool) =
  var x = ("hi", 5) # goal: computed as cursor

  x = if cond:
        ("different", 54)
      else:
        ("string here", 80)

  echo x

main(false)

proc sio =
  for x in lines("debug.txt"):
    echo x

if false:
  sio()
