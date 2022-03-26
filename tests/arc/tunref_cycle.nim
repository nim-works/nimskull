discard """
  outputsub: '''inside closure
hello world'''
  matrix: "--gc:orc -d:useMalloc"
  valgrind: true
"""

# bug #18579

var fp: proc (env: pointer) {.cdecl.}
var env: pointer

proc store(f: proc (){.closure.}) =
  proc closeOver() =
    echo "inside closure"
    f()
  (fp,env) = (cast[proc(env: pointer){.cdecl.}](rawProc closeOver), rawEnv closeOver)
  GC_ref(cast[RootRef](env))

proc run() =
  fp(env)
  GC_unref(cast[RootRef](env))

store(proc() = echo "hello world")
run()
GC_fullCollect()
