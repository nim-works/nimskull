import compiler/vm/vmir

import std/strformat
import std/options

# TODO: not related to debugging, move proc somewhere else
func calcStmt*(irs: IrStore3): seq[bool] =
  # XXX: very poor way of checking for statements
  result.newSeq(irs.len)
  var i = 0
  for n in irs.nodes:
    case n.kind
    of ntkSym, ntkLocal, ntkJoin, ntkLit, ntkGoto, ntkProc, ntkParam:
      discard
    of ntkCall:
      for it in irs.args(i):
        result[it] = true

      if n.callKind == ckNormal:
        result[n.callee] = true
    of ntkAddr, ntkDeref:
      result[n.addrLoc] = true
    of ntkAsgn:
      result[n.wrLoc] = true
      result[n.srcLoc] = true
    of ntkPathObj:
      result[n.srcLoc] = true
    of ntkPathArr:
      result[n.srcLoc] = true
      result[n.arrIdx] = true
    of ntkUse, ntkConv, ntkCast:
      result[n.srcLoc] = true
    of ntkBranch:
      result[n.cond] = true
    else:
      debugEcho "Skipping: ", n.kind
    inc i

proc printTypes*(ir: IrStore3, e: IrEnv) =
  var i = 0
  for (id, sid) in ir.locals:
    var visited: seq[TypeId]
    var indent = ""
    echo "local ", i, ":"
    var t = id
    while t != NoneType and t notin visited:
      visited.add t
      echo indent, e.types[t]
      indent &= "  "
      t = e.types[t].base

    inc i

func typeName(e: TypeEnv, t: TypeId): string =
  if (let iface = e.iface(t); iface != nil):
    iface.name.s
  else:
    $e.kind(t)

func typeToStr*(env: TypeEnv, id: TypeId): string =
  var id = id
  var d = -1
  while id != NoneType:
    inc d
    let t = env[id]
    if (let iface = env.iface(id); iface != nil):
      result.add iface.name.s
      break
    elif (let a = env.getAttachmentIndex(id); a.isSome):
      result.add env.getAttachment(a.unsafeGet)[0].s
      break
    else:
      result.add $env.kind(id)
      result.add "["
      id = t.base

  for _ in 0..<d:
    result.add "]"

iterator toStrIter*(irs: IrStore3, e: IrEnv, exprs: seq[bool]): string =
  var i = 0
  for n in irs.nodes:
    var
      line = ""
      indentStmt = true

    case n.kind
    of ntkSym:
      line = fmt"sym {e.syms[irs.sym(n)].decl.name.s}"
    of ntkProc:
      line = fmt"proc '{e.procs[n.procId].decl.name.s}'"
    of ntkParam:
      line = fmt"param {n.paramIndex}: '{e.procs.param(irs.owner, n.paramIndex).name.s}'"
    of ntkAsgn:
      case n.asgnKind
      of askCopy, askDiscr:
        line = fmt"{n.wrLoc} = {n.srcLoc}"
      of askMove:
        line = fmt"{n.wrLoc} = move {n.srcLoc}"
      of askShallow:
        line = fmt"{n.wrLoc} = shallow {n.srcLoc}"
      of askInit:
        line = fmt"{n.wrLoc} := {n.srcLoc}"
    of ntkDeref:
      line = fmt"deref {n.addrLoc}"
    of ntkLit:
      let (val, typ) = irs.getLit(n)
      if val.isNil:
        # a type literal
        line = fmt"lit type:{typeToStr(e.types, typ)}"
      else:
        line = fmt"lit {val.kind}"
    of ntkUse:
      line = fmt"use {n.srcLoc}"
    of ntkGoto:
      line = fmt"goto label:{n.target}"
    of ntkLocal:
      let L = irs.getLocal(i)
      line = fmt"local kind:{L.kind} idx:{irs.getLocalIdx(i)}"
    of ntkPathObj:
      line = fmt"path obj:{n.srcLoc} field:{n.fieldIdx}"
    of ntkPathArr:
      line = fmt"path arr:{n.srcLoc} idx:@{n.arrIdx}"
    of ntkConv:
      line = fmt"conv val:{n.srcLoc} typ:{typeName(e.types, n.typ)}"
    of ntkCast:
      line = fmt"cast val:{n.srcLoc} typ:{typeName(e.types, n.typ)}"
    of ntkCall:
      case n.callKind
      of ckBuiltin:
        line = fmt"call bi: {n.builtin}"
      of ckMagic:
        line = fmt"call magic: {n.magic} args: {n.argCount}"
      of ckNormal:
        line = fmt"call {n.callee} args: ["
        for arg in irs.args(i):
          line.add fmt"{arg} "
        line.add "]"
    of ntkBranch:
      line = fmt"branch label:{n.target} cond:{n.cond}"
    of ntkJoin:
      indentStmt = false
      if irs.isLoop(n.joinPoint):
        line = fmt"loop {n.joinPoint} :"
      else:
        line = fmt"label {n.joinPoint} :"

    else:
      line = fmt"<missing: {n.kind}>"

    if exprs.len == 0:
      line = fmt"{i}(?): {line}"
    elif exprs[i]:
      line = fmt"{i}: {line}"
    elif indentStmt:
      line = "  " & line

    yield line
    inc i

proc printIr*(irs: IrStore3, e: IrEnv, exprs: seq[bool]) =
  for x in toStrIter(irs, e, exprs):
    echo x


proc echoTrace*(ir: IrStore3, n: IRIndex) =
  let trace = ir.traceFor(n)
  for e in trace.items:
    echo fmt"{e.filename}({e.line}, 1) {e.procname}"