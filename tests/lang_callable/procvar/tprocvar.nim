discard """
  output: ''''''
"""

block genericprocvar:
  proc foo[T](thing: T) =
    discard thing
  var a: proc (thing: int) {.nimcall.} = foo[int]


block tprocvar2:
  var output = ""
  proc pa() {.cdecl.} = output.add "pa"
  proc pb() {.cdecl.} = output.add "pb"
  proc pc() {.cdecl.} = output.add "pc"
  proc pd() {.cdecl.} = output.add "pd"
  proc pe() {.cdecl.} = output.add "pe"

  const algos = [pa, pb, pc, pd, pe]
  var x: proc (a, b: int): int {.cdecl.}

  proc ha(c, d: int): int {.cdecl.} =
    result = c + d

  for a in items(algos):
    a()

  x = ha
  output.add $x(3, 4)

  doAssert output == "papbpcpdpe7"


block tprocvars:
  proc doSomething(v: int, x: proc(v:int):int): int = return x(v)
  proc doSomething(v: int, x: proc(v:int)) = x(v)

  doAssert doSomething(10, proc(v: int): int = return v div 2) == 5

