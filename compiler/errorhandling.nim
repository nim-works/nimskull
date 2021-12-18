#
#
#           The Nim Compiler
#        (c) Copyright 2021 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains support code for error handling via an `nkError` node
## kind.
##
## An nkError node is used where an error occurs within the AST. Wrap the ast
## node with `newError` and typically take over the position of the wrapped
## node in whatever AST it was in.
##
## Internally an nkError node stores these children:
## * 0 - wraps an AST node that has the error
## * 1 - nkIntLit with a value corresponding to `ord(ErrorKind)`
## * 2 - compiler instantiation location info
## * 3 - first argument position, assuming one was provided
## * _ - zero or more nodes with data for the error message
##
## The rest of the compiler should watch for nkErrors and mostly no-op or wrap
## further errors as needed.
##
## # Future Considerations/Improvements:
## * accomodate for compiler related information like site of node creation to
##   make it easier to debug the compiler itself, so we know where a node was
##   created
## * rework internals to store actual error information in a lookup data
##   structure on the side instead of directly in the node

import ast
from options import ConfigRef, store
from lineinfos import unknownLineInfo
import reports

template instLoc(depth = -2): InstantiationInfo =
  ## grabs where in the compiler an error was instanced to ease debugging.
  ##
  ## whether to use full paths depends on --excessiveStackTrace compiler option.
  instantiationInfo(depth, fullPaths = compileOption"excessiveStackTrace")

proc errorSubNode*(n: PNode): PNode =
  ## find the first error node, or nil, under `n` using a depth first traversal
  case n.kind
  of nkEmpty..nkNilLit:
    result = nil
  of nkError:
    result = n
  else:
    result = nil
    for s in n.items:
      if s.isNil: continue
      result = errorSubNode(s)
      if result != nil: break

const
  wrongNodePos*    = 0 ## the ast node we swapped
  errorKindPos*    = 1 ## the enum as an intlit
  compilerInfoPos* = 2 ## compiler source file as strlit, line & col on info
  firstArgPos*     = 3 ## first 0..n additional nodes depends on error kind

func errorKind*(e: PNode): SemReportErrorKind {.inline.} =
  ## property to retrieve the error kind
  assert e != nil, "can't have a nil error node"
  assert e.kind == nkError, "must be an error node to have an ErrorKind"

  result = SemReportErrorKind(e[errorKindPos].intVal)

func compilerInstInfo*(e: PNode): InstantiationInfo {.inline.} =
  ## return where the error was instantiated in the compiler
  let i = e[compilerInfoPos]
  assert i != nil, "we should always have compiler diagnositics"
  (filename: i.strVal, line: i.info.line.int, column: i.info.col.int)

proc newError*(
    wrongNode: PNode;
    errorKind: SemReportErrorKind,
    report: ReportId,
    inst: InstantiationInfo,
    args: varargs[PNode]
  ): PNode =
  ## Create `nkError` node with with given error report and additional
  ## subnodes.
  assert wrongNode != nil, "can't have a nil node for `wrongNode`"

  result = newNodeIT(
    nkError,
    wrongNode.info,
    newType(tyError, ItemId(module: -2, item: -1), nil))

  result.reportId = report
  result.add wrongNode
  result.add newIntNode(nkIntLit, ord(errorKind)) # errorKindPos
  result.add newStrNode(inst.filename, wrongNode.info) # compilerInfoPos

  for a in args:
    result.add a

template newError*(
    conf: ConfigRef,
    wrongNode: PNode,
    report: SemReport,
    args: seq[PNode] = @[],
    info: TLineInfo = wrongNode.info,
  ): untyped =

  var rep = report
  if isNil(rep.expression):
    rep.expression = wrongNode

  let tmp = wrap(rep, instLoc(), conf.toReportLinePoint(info))
  let id = conf.addReport(tmp)
  newError(wrongNode, tmp.semReport.kind, id, instLoc(), args)

template newError*(
    conf: ConfigRef,
    node: PNode,
    reportKind: SemReportKind,
    sym: PSym = nil,
    errMsg: string = "",
    args: seq[PNode] = @[]
  ): untyped =
  newError(
    node,
    reportKind,
    conf.addReport(wrap(
      SemReport(expression: node, psym: sym, kind: reportKind, msg: errMsg),
      instLoc(),
      conf.toReportLinePoint(node.info))),
    instLoc(),
    args)

proc wrapErrorInSubTree*(wrongNodeContainer: PNode): PNode =
  ## `wrongNodeContainer` doesn't directly have an error but one exists further
  ## down the tree, this is used to wrap the `wrongNodeContainer` in an nkError
  ## node but no message will be reported for it.
  var e = errorSubNode(wrongNodeContainer)
  assert e != nil, "there must be an error node within"
  result = newError(
    wrongNodeContainer, rsemWrappedError, emptyReportId, instLoc())

proc wrapIfErrorInSubTree*(conf: ConfigRef, wrongNodeContainer: PNode): PNode
  {.deprecated: "transition proc, remove usage as soon as possible".} =
  ## `wrongNodeContainer` doesn't directly have an error but one may exist
  ## further down the tree. If an error does exist it will wrap
  ## `wrongNodeContainer` in an nkError node but no message will be reported
  ## for this wrapping. If there is no error, the `wrongNodeContainer` will be
  ## returned as is.
  var e = errorSubNode(wrongNodeContainer)
  result =
    if e.isNil:
      wrongNodeContainer
    else:
      newError(
        wrongNodeContainer,
        rsemWrappedError,
        conf.store SemReport(kind: rsemWrappedError),
        instLoc())

proc buildErrorList(n: PNode, errs: var seq[PNode]) =
  ## creates a list (`errs` seq) from least specific to most specific
  case n.kind
  of nkEmpty..nkNilLit:
    discard
  of nkError:
    errs.add n
    buildErrorList(n[wrongNodePos], errs)
  else:
    for i in countdown(n.len - 1, 0):
      buildErrorList(n[i], errs)

iterator walkErrors*(config: ConfigRef; n: PNode): PNode =
  ## traverses the ast and yields errors from innermost to outermost. this is a
  ## linear traversal and two, or more, sibling errors will result in only the
  ## first error (per `PNode.sons`) being yielded.
  assert n != nil
  var errNodes: seq[PNode] = @[]
  buildErrorList(n, errNodes)

  # report from last to first (deepest in tree to highest)
  for i in 1..errNodes.len:
    # reverse index so we go from the innermost to outermost
    let e = errNodes[^i]
    if e.errorKind == rsemWrappedError:
      continue

    yield e

iterator ifErrorWalkErrors*(config: ConfigRef; n: PNode): PNode =
  ## traverse the ast like `walkErrors`, but will only do so if `n` is not nil
  ## or an error -- useful when guarding isn't beneficial.
  if n != nil and n.kind == nkError:
    for e in walkErrors(config, n):
      yield e

iterator anyErrorsWalk*(config: ConfigRef; n: PNode
    ): PNode {.deprecated: "only use for debugging purposes".} =
  ## for debugging, walk n yielding any errors found
  if n != nil:
    for e in walkErrors(config, n):
      yield e
