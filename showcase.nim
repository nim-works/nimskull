## Provides some examples of possible use cases for delimited continuations.

import std/[strutils, macros]

{.experimental: "callOperator".}

# some helper routines to allow for "erasing" the context parameter, so that
# different continuation with the same shape can be stored in the same location

type
  CellBase = ref object of RootObj
    copy: proc(x: CellBase): CellBase {.nimcall, raises: [].}
  Cell[T] {.final.} = ref object of CellBase
    val: T
  CellPtr = object
    ## A unique managed pointer.
    cell: CellBase

  Cont[P, R] = tuple
    prc: proc(p: sink P, cell: sink CellPtr): R {.tailcall.}
    env: CellPtr

proc `=copy`(x: var CellPtr, y: CellPtr) =
  if y.cell.isNil:
    x.cell = nil
  elif x.cell != y.cell:
    x.cell = y.cell.copy(y.cell)

proc copyCell[T](x: CellBase): CellBase =
  Cell[T](val: Cell[T](x).val, copy: x.copy)

proc newCell[T](x: sink T): CellPtr =
  CellPtr(cell: Cell[T](val: x, copy: copyCell[T]))

proc take[T](x: sink CellPtr): T =
  move Cell[T](x.cell).val

proc newCont[R, T](env: sink T, prc: proc(x: sink T): R {.tailcall.}): Cont[void, R] =
  (proc(env: sink CellPtr): R {.tailcall.} =
    let (prc, env) = take[(typeof(prc), T)](env)
    prc(env)
   , newCell((prc, env)))

proc newCont[P, R, T](env: sink T, prc: proc(x: sink P, y: sink T): R {.tailcall.}): Cont[P, R] =
  (proc(param: sink P, env: sink CellPtr): R {.tailcall.} =
    let (prc, env) = take[(typeof(prc), T)](env)
    prc(param, env)
   , newCell((prc, env)))

proc newCont[T](x: sink T): auto {.inline.} =
  let (env, prc) = x
  newCont(env, prc)

proc drop[T](x: sink T) = discard

template newContP[P, R](body: proc(p: sink P): R {.tailcall.}): Cont[P, R] =
  (proc (p: sink P, env: sink CellPtr): R {.tailcall.} = (drop(env); body(p)),
   CellPtr())

proc `()`[P, R](c: sink Cont[P, R], a: sink P): R {.tailcall.} =
  # TODO: make this a tailcall procedure
  let (prc, env) = c
  prc(a, env)

# ----- interpreter example ------

type
  NodeKind = enum
    nkAdd, nkEq, nkIf, nkConst

  Node = object
    case kind: NodeKind
    of nkAdd, nkEq, nkIf:
      sub: seq[Node]
    of nkConst:
      val: int

template `[]`(n: Node, i: untyped): Node =
  n.sub[i]

template tree(k: NodeKind, args: varargs[Node]): Node =
  Node(kind: k, sub: @args)

template cnst(v: int): Node =
  Node(kind: nkConst, val: v)

proc interpret(n: Node, then: sink Cont[Node, Node]): Node {.tailcall.} =
  template eval(n: Node): Node =
    suspend(Node, cont, interpret(n, newCont(cont)))

  proc take(x: sink Node): int = x.val

  case n.kind
  of nkConst:
    then n
  of nkAdd:
    then cnst(take(eval(n[0])) + take(eval(n[1])))
  of nkEq:
    then cnst(ord(take(eval(n[0])) == take(eval(n[1]))))
  of nkIf:
    if take(eval n[0]) == 0:
      eval(n[2]) # else branch
    else:
      eval(n[1]) # then branch

proc interpret(n: Node): Node {.tailcall.} =
  interpret(n, newContP(proc(x: sink Node): Node {.tailcall.} = x))

# XXX: cannot work given the rules and current implementation
#[
doAssert interpret(
            tree(nkIf,
              tree(nkEq, cnst(1), cnst(2)),
              cnst(0),
              cnst(1))).val == 1
]#

# ----- async/await -----

type
  Future[T] = ref object
    content: ref T
    callback: proc(x: Future[T])

proc awaitImpl[T, U](f: Future[U], with: sink Cont[U, Future[T]],
                     on: sink Future[T]): Future[T] =
  f.callback = proc(x: Future[U]) =
    discard with(x.content[])
  return on

macro async(p: untyped) =
  let prev = p.body
  p.body = quote do:
    result = typeof(result)()
    template await[T](x: Future[T]): T {.used.} =
      suspend(T, cont, awaitImpl(x, newCont(cont), result))
    `prev`
  result = p

proc resolve[T](x: Future[T], val: sink T) =
  x.content = new T
  x.content[] = val
  if x.callback != nil:
    x.callback(x)

var inner: Future[string]
  ## an unresolved future

proc jield[T, U](on: sink Future[T], with: sink Cont[T, Future[U]]): Future[T] =
  inner = Future[T]()
  inner.callback = proc(x: Future[T]) =
    discard with(x.content[])
  return on

proc readFileAsync(): Future[string] {.async.} =
  # simulate a wait:
  let val = suspend(string, cont, jield(result, newCont(cont)))
  result.resolve val

proc parseFileContent(): Future[int] {.async.} =
  let content = await readFileAsync()
  result.resolve parseInt(content)

var res = parseFileContent()
assert res.content == nil
# simulate the read file operation finishing:
inner.resolve("123")
# now the future is done:
assert res.content != nil
assert res.content[] == 123

# ------ fun things ------

# some non-sensical things that are fun to write

proc repeat(): int =
  var i = suspend(int, cont, (proc(cont: Cont[int, int]): int =
      var val = 0
      for _ in 0..<10:
        val = cont(val)
      val
    )(newCont(cont)))
  return i + 1

assert repeat() == 10

# continuations can be duplicated and run multiple times

proc asgn[T](a: var T, b: T) =
  a = b

proc multishot(o: var Cont[int, void]) =
  var x = "abc"
  let i = suspend(int, cont, asgn(o, newCont(cont)))
  x.addInt i
  echo x

var r: Cont[int, void]
multishot(r)
for i in 0..<100:
  r(i)

macro cps(param: untyped, p: untyped) =
  ## Primitive implementation of turning a direct style procedure into a
  ## continuation-passing style procedure.
  let wrapParams = nnkFormalParams.newTree(newEmptyNode())
  let call = newCall(copyNimTree(p.name))
  let cont = genSym(nskLet, "cont")
  for i in 1..<p.params.len:
    if p.params[i][0].eqIdent(param):
      call.add copyNimTree(cont)
    else:
      call.add copyNimTree(p.params[i][0])

  let wrapper = newProc(procType=nnkTemplateDef, name=p.name)
  wrapper.params = wrapParams
  wrapper.body = quote do:
    suspend(void, `cont`, `call`)

  result = newStmtList(p, wrapper)

proc interrupt[C, P](cont: sink (C, P)) {.cps: cont.} =
  echo "interrupted"
  cont[1](cont[0])
  echo "done"

proc test() =
  echo "before"
  interrupt()
  echo "after"

test()
# echoes "before", "interrupted", "after", "done"
