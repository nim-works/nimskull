#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Plugin to transform an inline iterator into a data structure.

import
  compiler/ast/[
    ast
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    msgs
  ],
  compiler/sem/[
    lookups,
    semdata,
    lambdalifting
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportAst
from compiler/ast/report_enums import ReportKind

proc iterToProcImpl*(c: PContext, n: PNode): PNode =
  result = newNodeI(nkStmtList, n.info)
  let iter = n[1]
  if iter.kind != nkSym or iter.sym.kind != skIterator:
    localReport(c.config, iter.info, reportAst(
      rsemIllformedAst, iter,
      str = "first argument needs to be an iterator"))

    return
  if n[2].typ.isNil:
    localReport(c.config, n[2].info, reportAst(
      rsemIllformedAst, n,
      str = "second argument needs to be a type"))

    return
  if n[3].kind != nkIdent:
    localReport(c.config, n[3].info, reportAst(
      rsemIllformedAst, n,
      str = "third argument needs to be an identifier"))

    return

  let t = n[2].typ.skipTypes({tyTypeDesc, tyGenericInst})
  if t.kind notin {tyRef, tyPtr} or t.lastSon.kind != tyObject:
    localReport(c.config, n[2].info, reportAst(
      rsemIllformedAst, n[2],
      str = "type must be a non-generic ref|ptr to object with state field"))

    return
  let body = liftIterToProc(c.graph, iter.sym, getBody(c.graph, iter.sym), t, c.idgen)

  let prc = newSym(skProc, n[3].ident, nextSymId c.idgen, iter.sym.owner, iter.sym.info)
  prc.typ = copyType(iter.sym.typ, nextTypeId c.idgen, prc)
  excl prc.typ.flags, tfCapturesEnv
  prc.typ.n.add newSymNode(getEnvParam(iter.sym))
  prc.typ.rawAddSon t
  let orig = iter.sym.ast
  prc.ast = newProcNode(nkProcDef, n.info,
              body = body, params = orig[paramsPos], name = newSymNode(prc),
              pattern = c.graph.emptyNode, genericParams = c.graph.emptyNode,
              pragmas = orig[pragmasPos], exceptions = c.graph.emptyNode)

  prc.ast.add iter.sym.ast[resultPos]
  addInterfaceDecl(c, prc)
