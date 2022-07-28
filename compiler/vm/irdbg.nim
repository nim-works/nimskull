import compiler/vm/vmir

import std/strformat

# TODO: not related to debugging, move proc somewhere else
func calcStmt*(irs: IrStore3): seq[bool] =
  # XXX: very poor way of checking for statements
  result.newSeq(irs.len)
  var i = 0
  for n in irs.nodes:
    case n.kind
    of ntkSym, ntkLocal, ntkJoin, ntkLit, ntkGoto:
      discard
    of ntkCall:
      for it in n.args:
        result[it] = true

      if not n.isBuiltIn:
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
    of ntkUse:
      result[n.srcLoc] = true
    of ntkBranch:
      result[n.cond] = true
    else:
      debugEcho "Skipping: ", n.kind
    inc i


proc printIr*(irs: IrStore3, exprs: seq[bool]) =
  var i = 0
  for n in irs.nodes:
    var line = ""
    case n.kind
    of ntkSym:
      line = fmt"sym {irs.sym(n).name.s}"
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
      line = fmt"lit {irs.getLit(n).kind}"
    of ntkUse:
      line = fmt"use {n.srcLoc}"
    of ntkGoto:
      line = fmt"goto label:{n.target}"
    of ntkLocal:
      let (k, t, _) = irs.getLocal(i)
      line = fmt"local kind:{k} idx:{irs.getLocalIdx(i)} typ:{t.kind}"
    of ntkPathObj:
      line = fmt"path obj:{n.srcLoc} field:{n.fieldIdx}"
    of ntkPathArr:
      line = fmt"path arr:{n.srcLoc} idx:@{n.arrIdx}"
    of ntkCall:
      if n.isBuiltIn:
        line = fmt"call bi: {n.builtin}"
      else:
        line = fmt"call {n.callee} args: ["
        for arg in n.args:
          line.add fmt"{arg} "
        line.add "]"
    of ntkBranch:
      line = fmt"branch label:{n.target} cond:{n.cond}"
    of ntkJoin:
      if irs.isLoop(n.joinPoint):
        echo "loop ", n.joinPoint, ":"
      else:
        echo "label ", n.joinPoint, ":"
      inc i
      continue
    else:
      line = fmt"<missing: {n.kind}>"

    if exprs[i]:
      echo i, ": ", line
    else:
      echo "  ", line
    inc i
