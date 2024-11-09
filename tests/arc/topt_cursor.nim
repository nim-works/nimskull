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
  echo(arg type(array[0..0, string]), arg _4) -> [L2]
  =destroy(name _4)
  goto [L3]
  finally (L2):
    =destroy(name _4)
    continue [Resume]
  L3:
-- end of expandArc ------------------------
--expandArc: sio

scope:
  scope:
    def_cursor filename: string = "debug.txt"
    def_cursor _3: string = filename
    def f: File = open(arg _3, arg fmRead, arg 8000) -> [Resume]
    def _4: uint32
    scope:
      def res: string = newStringOfCap(arg 80)
      scope:
        while true:
          scope:
            def_cursor _7: File = f
            def :tmp: bool = readLine(arg _7, name res) -> [L1]
            scope:
              def_cursor _8: bool = :tmp
              def _9: bool = not(arg _8)
              if _9:
                scope:
                  goto [L3]
            scope:
              def_cursor x: string = res
              def_cursor _11: string = x
              echo(arg type(array[0..0, string]), arg _11) -> [L1]
      L3:
      =destroy(name res)
      _4 = 0'u32
      goto [L4]
      finally (L1):
        =destroy(name res)
        continue [L5]
    except (L5):
      _4 := 1'u32
    L4:
    scope:
      def_cursor _12: File = f
      close(arg _12) -> [L6]
    goto [L7]
    finally (L6):
      def _13: bool = eqI(arg _4, arg 1'u32)
      if _13:
        nimAbortException(arg true)
      continue [Resume]
    L7:
    case _4
    of 0'u32: goto L9
    of 1'u32: goto L10
    L9:
    goto [L11]
    L10:
    raise -> [Resume]
    L11:

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
