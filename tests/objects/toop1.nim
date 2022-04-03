discard """
  targets: "c cpp"
  output: "34[]o 5"
"""
# Test the stuff in the tutorial
import macros

type
  TFigure = object of RootObj    # abstract base class:
    draw: proc (my: var TFigure) {.nimcall.} # concrete classes implement this

proc init(f: var TFigure) =
  f.draw = nil

type
  TCircle = object of TFigure
    radius: int

proc drawCircle(my: var TCircle) = stdout.writeLine("o " & $my.radius)

proc init(my: var TCircle) =
  init(TFigure(my)) # call base constructor
  my.radius = 5
  my.draw = cast[proc (my: var TFigure) {.nimcall.}](drawCircle)

type
  TRectangle = object of TFigure
    width, height: int

proc drawRectangle(my: var TRectangle) = stdout.write("[]")

proc init(my: var TRectangle) =
  init(TFigure(my)) # call base constructor
  my.width = 5
  my.height = 10
  my.draw = cast[proc (my: var TFigure) {.nimcall.}](drawRectangle)

macro `!` (args: varargs[untyped]): typed =
  args.expectLen 2
  let arg1 = args[0]
  let arg2 = args[1]
  for i, arg in args:
    echo "arg", i, " ", arg.lispRepr

  result = newNimNode(nnkCall)
  var dot = newNimNode(nnkDotExpr)
  dot.add(arg1)    # obj
  if arg2.kind == nnkCall:
    # transforms ``obj!method(arg1, arg2, ...)`` to
    # ``(obj.method)(obj, arg1, arg2, ...)``
    dot.add(arg2[0]) # method
    result.add(dot)
    result.add(arg1) # obj
    for i in 1..arg2.len-1:
      result.add(arg2[i])
  else:
    # transforms ``obj!method`` to
    # ``(obj.method)(obj)``
    dot.add(arg2) # method
    result.add(dot)
    result.add(arg1) # obj

type
  TSocket* = object of RootObj
    FHost: int # cannot be accessed from the outside of the module
               # the `F` prefix is a convention to avoid clashes since
               # the accessors are named `host`

proc `host=`*(s: var TSocket, value: int) {.inline.} =
  ## setter of hostAddr
  s.FHost = value

proc host*(s: TSocket): int {.inline.} =
  ## getter of hostAddr
  return s.FHost

var
  s: TSocket
s.host = 34  # same as `host=`(s, 34)
stdout.write(s.host)

# now use these classes:
var
  r: TRectangle
  c: TCircle
init(r)
init(c)
r!draw
c!draw()

#OUT 34[]o 5
