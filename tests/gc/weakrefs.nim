discard """
  output: "true"
"""

import intsets

type
  TMyObject = object
    id: int
  StrongObject = ref TMyObject
  WeakObject = object
    id: int
    data: ptr TMyObject

var
  gid: int # for id generation
  valid = initIntSet()

proc `=destroy`(x: var TMyObject) =
  valid.excl(x.id)

proc create: StrongObject =
  new(result)
  result.id = gid
  valid.incl(gid)
  inc gid

proc register(s: StrongObject): WeakObject =
  result.data = cast[ptr TMyObject](s)
  result.id = s.id

proc access(w: WeakObject): StrongObject =
  ## returns nil if the object doesn't exist anymore
  if valid.contains(w.id):
    result = cast[StrongObject](w.data)

proc main =
  var s: seq[WeakObject]
  newSeq(s, 10_000)
  for i in 0 .. s.high:
    s[i] = register(create())
  # test that we have at least 80% unreachable weak objects by now:
  when defined(gcMarkAndSweep):
    GC_fullcollect()
  var unreachable = 0
  for i in 0 .. s.high:
    if access(s[i]) == nil: inc unreachable
  echo unreachable > 8_000

main()
