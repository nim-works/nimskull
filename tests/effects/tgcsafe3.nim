discard """
  errormsg: "'myproc' is not GC-safe as it calls 'global_proc'"
  line: 13
  matrix: "--hints:on --threads:on"
  targets: "c cpp"
"""

var useGcMem = "string here"

var global_proc: proc(a: string) {.nimcall.} = proc (a: string) =
  echo useGcMem

proc myproc(i: int) {.gcsafe.} =
  when false:
    if global_proc != nil:
      echo "a"
    if isNil(global_proc):
      return

  global_proc("ho")

myproc(0)
