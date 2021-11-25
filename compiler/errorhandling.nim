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
from options import ConfigRef

type
  ErrorKind* {.pure.} = enum ## expand as you need.
    CustomError
    CustomPrintMsgAndNodeError
      ## just like custom error, prints a message and renders wrongNode
    RawTypeMismatchError

    CustomUserError
      ## just like customer error, but reported as a errUser in msgs

    # Global Errors
    CustomGlobalError
      ## just like custom error, but treat it like a "raise" and fast track the
      ## "graceful" abort of this compilation run, used by `errorreporting` to
      ## bridge into the existing `msgs.liMessage` and `msgs.handleError`.

    # Fatal Errors
    FatalError
      ## treat as a fatal error, meaning we do a less (?) "graceful" abort,
      ## used by `errorreporting` to bridge into the existing `msgs.liMessage`
      ## and `msgs.handleError`.
      ## xxx: with the curren way the errorreporting module works, these must
      ##      be created via msgs.fatal 

    # Call
    CallTypeMismatch
    ExpressionCannotBeCalled
    WrongNumberOfArguments
    AmbiguousCall
    CallingConventionMismatch

    # ParameterTypeMismatch

    # Identifier Lookup
    UndeclaredIdentifier
    ExpectedIdentifier
    ExpectedIdentifierInExpr

    # Object and Object Construction
    FieldNotAccessible 
      ## object field is not accessible
    FieldAssignmentInvalid
      ## object field assignment invalid syntax
    FieldOkButAssignedValueInvalid
      ## object field assignment, where the field name is ok, but value is not
    ObjectConstructorIncorrect
      ## one or more issues encountered with object constructor
    
    # General Type Checks
    ExpressionHasNoType
      ## an expression has not type or is ambiguous
    
    # Literals
    IntLiteralExpected
      ## int literal node was expected, but got something else
    StringLiteralExpected
      ## string literal node was expected, but got something else

    # Pragma
    InvalidPragma
      ## suplied pragma is invalid
    IllegalCustomPragma
      ## supplied pragma is not a legal custom pragma, and cannot be attached
    NoReturnHasReturn
      ## a routine marked as no return, has a return type
    ImplicitPragmaError
      ## a symbol encountered an error when processing implicit pragmas, this
      ## should be applied to symbols and treated as a wrapper for the purposes
      ## of reporting. the original symbol is stored as the first argument
    PragmaDynlibRequiresExportc
      ## much the same as `ImplicitPragmaError`, except it's a special case
      ## where dynlib pragma requires an importc pragma to exist on the same
      ## symbol
      ## xxx: pragmas shouldn't require each other, that's just bad design

    WrappedError
      ## there is no meaningful error to construct, but there is an error
      ## further down the AST that invalidates the whole

type InstantiationInfo* = typeof(instantiationInfo())
  ## type alias for instantiation information
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

func errorKind*(e: PNode): ErrorKind {.inline.} =
  ## property to retrieve the error kind
  assert e != nil, "can't have a nil error node"
  assert e.kind == nkError, "must be an error node to have an ErrorKind"

  result = ErrorKind(e[errorKindPos].intVal)

func compilerInstInfo*(e: PNode): InstantiationInfo {.inline.} =
  ## return where the error was instantiated in the compiler
  let i = e[compilerInfoPos]
  assert i != nil, "we should always have compiler diagnositics"
  (filename: i.strVal, line: i.info.line.int, column: i.info.col.int)

proc newErrorAux(
    wrongNode: PNode;
    k: ErrorKind;
    inst: InstantiationInfo;
    args: varargs[PNode]
  ): PNode =
  ## create an `nkError` node with error `k`, with additional error `args` and
  ## given `inst` as to where it was instanced int he compiler.
  assert wrongNode != nil, "can't have a nil node for `wrongNode`"

  result = newNodeIT(nkError, wrongNode.info,
                     newType(tyError, ItemId(module: -2, item: -1), nil))

  result.add wrongNode
  result.add newIntNode(nkIntLit, ord(k)) # errorKindPos
  result.add newStrNode(inst.filename, wrongNode.info) # compilerInfoPos

  # save the compiler's line and column information here for reporting
  result[compilerInfoPos].info.line = uint16 inst.line 
  result[compilerInfoPos].info.col = int16 inst.column

  for a in args: result.add a

proc newErrorActual(
    wrongNode: PNode;
    k: ErrorKind;
    inst: InstantiationInfo,
    args: varargs[PNode]
  ): PNode =
  ## create an `nkError` node with error `k`, with additional error `args` and
  ## given `inst` as to where it was instanced in the compiler.
  assert wrongNode != nil, "can't have a nil node for `wrongNode`"

  result = newErrorAux(wrongNode, k, inst, args)

proc newErrorActual(
    wrongNode: PNode;
    msg: string,
    inst: InstantiationInfo
  ): PNode =
  ## create an `nkError` node with a `CustomError` message `msg`
  newErrorAux(
    wrongNode, CustomError, inst, newStrNode(msg, wrongNode.info))

template newError*(wrongNode: PNode; k: ErrorKind; args: varargs[PNode]): PNode =
  ## create an `nkError` node with error `k`, with additional error `args` and
  ## given `inst` as to where it was instanced int he compiler.
  assert k != FatalError,
    "use semdata.fatal(config:ConfigRef, err: PNode) instead"
  newErrorActual(wrongNode, k, instLoc(-1), args)

template newFatal*(wrongNode: PNode; args: varargs[PNode]): PNode
  {.deprecated: "rework to remove the need for this awkward fatal handling".} =
  ## just like `newError`, only meant to be used by `semDdta` an and other
  ## modules that know to appropriately use `msgs.fatal(ConfigRef, PNode)` as
  ## the next call.
  newErrorActual(wrongNode, FatalError,
                 instLoc(-1), args)

template newError*(wrongNode: PNode; msg: string): PNode =
  ## create an `nkError` node with a `CustomError` message `msg`
  newErrorActual(wrongNode, msg, instLoc(-1))

template newCustomErrorMsgAndNode*(wrongNode: PNode; msg: string): PNode =
  ## create an `nkError` node with a `CustomMsgError` message `msg`
  newErrorActual(
    wrongNode,
    CustomPrintMsgAndNodeError,
    instLoc(-1),
    newStrNode(msg, wrongNode.info)
  )

proc wrapErrorInSubTree*(wrongNodeContainer: PNode): PNode =
  ## `wrongNodeContainer` doesn't directly have an error but one exists further
  ## down the tree, this is used to wrap the `wrongNodeContainer` in an nkError
  ## node but no message will be reported for it.
  var e = errorSubNode(wrongNodeContainer)
  assert e != nil, "there must be an error node within"
  result = newErrorAux(wrongNodeContainer, WrappedError, instLoc())

proc wrapIfErrorInSubTree*(wrongNodeContainer: PNode): PNode
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
      newErrorAux(wrongNodeContainer, WrappedError, instLoc())

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
    if e.errorKind == WrappedError: continue
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
