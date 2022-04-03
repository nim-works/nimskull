discard """
  errormsg: "'mainUnsafe' is not GC-safe"
  line: 27
  targets: "c cpp"
  matrix: "--hints:on --threads:on"
"""

# bug #6955
var global_proc: proc(a: string): int {.nimcall.} = nil

proc myproc(i: int) {.gcsafe.} =
  if global_proc != nil:
    echo "a"
  if isNil(global_proc):
    return

proc mymap(x: proc ()) =
  x()

var
  myglob: string

proc mainSafe() {.gcsafe.} =
  mymap(proc () = echo "foo")

proc mainUnsafe() {.gcsafe.} =
  mymap(proc () = myglob = "bar"; echo "foo", myglob)
