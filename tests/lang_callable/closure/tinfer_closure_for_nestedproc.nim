discard """
  action: compile
  target: "!vm"
"""

# disabled on VM because it crashes (knownIssue)

# bug #9441
import std/strtabs

type
  Request = object
  Context = object
    position: int
    accept: bool
    headers: StringTableRef
  Future[C] = object of RootObj
    failed*: bool
    guts*: C
  Handler = proc (r: ref Request, c: Context): Future[Context]

proc respond(req: Request): Future[void] = discard

proc handle*(h: Handler): auto = # (proc (req: Request): Future) =
  proc server(req: Request): Future[void] =
    let emptyCtx = Context(
      position: 0,
      accept: true,
      headers: newStringTable()
    )
    var reqHeap = new(Request)
    reqHeap[] = req
    var
      f: Future[Context]
      ctx: Context
    try:
      f = h(reqHeap, emptyCtx)
      ctx = f.guts
    except:
      discard
    if f.failed:
      discard req.respond()
    else:
      if not ctx.accept:
        discard req.respond()
  return server

discard handle(nil)(Request())
