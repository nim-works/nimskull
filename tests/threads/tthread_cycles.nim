discard """
  matrix: "--threads:on --gc:orc"
  description: '''
    Reference cycles created by a thread must be freed prior to it
    terminating
  '''
  output: "d\nd\nd\nexit"
"""

type
  Ref = ref Obj
  Obj = object
    x: Ref

proc `=destroy`(x: var Obj) =
  echo "d"

proc t() {.thread.} =
  # create a reference cycle that involves multiple cells
  var
    v1 = Ref()
    v2 = Ref(x: v1)
    v3 = Ref(x: v2)
  v1.x = v3

proc main() =
  # wrap the test in a procedure to prevent the thread from being a
  # global
  var thr: Thread[void]
  createThread(thr, t)
  joinThread(thr)
  # the reference cycles created by `thr` must have all been freed
  # by the time ``joinThread`` returns
  echo "exit"

main()