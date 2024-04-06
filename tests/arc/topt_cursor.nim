discard """
  output: '''("string here", 80)'''
  cmd: '''nim c --gc:arc --expandArc:main --expandArc:sio --hint:Performance:off $file'''
  nimout: '''--expandArc: main

scope:
  try:
    def_cursor x: (string, int) = <D0>
    block L0:
      scope:
        if cond:
          scope:
            x = <D1>
            break L0
      scope:
        x = <D2>
    def_cursor _3: (string, int) = x
    def _4: string = $(arg _3) (raises)
    echo(arg type(array[0..0, string]), arg _4) (raises)
  finally:
    =destroy(name _4)
-- end of expandArc ------------------------
--expandArc: sio

scope:
  scope:
    def_cursor filename: string = "debug.txt"
    def_cursor _3: string = filename
    def f: File = open(arg _3, arg 0'u, arg 8000) (raises)
    try:
      scope:
        try:
          def res: string = newStringOfCap(arg 80)
          block L0:
            scope:
              while true:
                scope:
                  def_cursor _6: File = f
                  def :tmp: bool = readLine(arg _6, name res) (raises)
                  scope:
                    def_cursor _7: bool = :tmp
                    def _8: bool = not(arg _7)
                    if _8:
                      scope:
                        break L0
                  scope:
                    scope:
                      def_cursor x: string = res
                      def_cursor _10: string = x
                      echo(arg type(array[0..0, string]), arg _10) (raises)
        finally:
          =destroy(name res)
    finally:
      scope:
        def_cursor _11: File = f
        close(arg _11) (raises)
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
