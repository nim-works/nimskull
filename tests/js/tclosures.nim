discard """
  action: run
"""

import random, strutils
const consolePrefix = "jsCallbacks"

var callback {.exportc.}: JsRoot

proc regCallback_ex(fn: JsRoot) {.exportc: "regCallback", asmNoStackFrame.} =
  ## Simulates a function that is defined at the JavaScript side.
  asm """
    if (callback == null) {
      callback = []
    }
    callback.push (`fn`);
  """

proc runCallbacks(): cstring {.asmNoStackFrame.} =
  asm """
        var result = "\n"
        var n = 0
        for (var fn in callback) {
            n += 1
            result += "("+String (n)+")"
            result += callback [fn] ()
            result += "\n"
        }
        return result
  """

proc print* (a: varargs[string, `$`]) = log "$1: $2" % [consolePrefix, join(a, " ")]

type CallbackProc {.importc.} = proc () : cstring

# import the JavaScript procedure that has no knowledge of NimSkull
# closures:
proc regCallback (fn:CallbackProc) {.importc.}

proc `*` (s:string, n:Natural) : string = s.repeat(n)

proc outer (i:Natural) : (string, int) =
    let c = $char(rand(93) + 33)
    let n = rand(40)
    let s = c * n
    proc inner(): cstring = ("[$1]" % $n) & s & " <--"
    regCallback(inner)
    return (s, n)

var expected = "\n"
for i in 1 .. 10:
    let (s, n) = outer(i)
    expected &= ("($1)[$2]" % [$i, $n]) & s & " <--"
    expected &= "\n"

let results = runCallbacks()

doAssert(expected == $results)

block issue7048:
  block:
    proc foo(x: seq[int]): auto =
      proc bar: int = x[1]
      bar

    var stuff = @[1, 2]
    let f = foo(stuff)
    stuff[1] = 321
    doAssert f() == 2

  block:
    proc foo(x: tuple[things: string]; y: array[3, int]): auto =
      proc very: auto = 
        proc deeply: auto =
          proc nested: (char, int) = (x.things[0], y[1])
          nested
        deeply
      very()

    var
      stuff = (things: "NIM")
      stuff2 = [32, 64, 96]
    let f = foo(stuff, stuff2)
    stuff.things = "VIM"
    stuff2[1] *= 10
    doAssert f()() == ('N', 64)
    doAssert (stuff.things[0], stuff2[1]) == ('V', 640)

  block:
    proc foo(x: ptr string): auto =
      proc bar(): int = len(x[])
      bar
    
    var 
      s1 = "xyz"
      s2 = "stuff"
      p = addr s1
    
    let f = foo(p)
    p = addr s2
    doAssert len(p[]) == 5
    doAssert f() == 3
