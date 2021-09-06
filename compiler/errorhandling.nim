#
#
#           The Nim Compiler
#        (c) Copyright 2021 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains support code for new-styled error
## handling via an `nkError` node kind.
## 
## An nkError node is used where an error occurs within the AST. Wrap the ast
## node with `newError` and typically take over the position of the wrapped
## node in whatever AST it was in.
## 
## Internally an nkError node stores these children:
## * 0 - wraps an AST node that has the error
## * 1 - any prior errors (picks the first child from a depth first search)
## * 2 - nkIntLit with a value corresponding to `ord(ErrorKind)`
## * _ - zero or more nodes with data for the error message
## 
## The rest of the compiler should watch for nkErrors and mostly no-op or wrap
## further errors as needed.

import ast, renderer, options, strutils, types

type
  ErrorKind* = enum ## expand as you need.
    RawTypeMismatchError
    ExpressionCannotBeCalled
    CustomError
    WrongNumberOfArguments
    AmbiguousCall
    ExpectedIdentifier
    ExpectedIdentifierInExpr

proc errorSubNode*(n: PNode): PNode =
  case n.kind
  of nkEmpty..nkNilLit:
    result = nil
  of nkError:
    result = n
  else:
    result = nil
    for i in 0..<n.len:
      result = errorSubNode(n[i])
      if result != nil: break

let noPrevError = newNode(nkEmpty)
  ## sentinil value to mark no previous errors

const
  wrongNodePos = 0
  prevErrorPos = 1
  errorKindPos = 2
  firstArgPos  = 3

proc newError*(wrongNode: PNode; k: ErrorKind; args: varargs[PNode]): PNode =
  ## create an `nkError` node with error `k`, with additional error `args`
  assert wrongNode != nil, "can't have a nil node for `wrongNode`"

  result = newNodeIT(nkError, wrongNode.info, newType(tyError, ItemId(module: -1, item: -1), nil))
  if wrongNode.kind == nkError:
    # wrapping an error in another error
    result.add wrongNode[wrongNodePos] # fetch the real one from the previous
    result.add wrongNode # the wrongNode param is the prevError

    result.info = wrongNode[wrongNodePos].info # fix line info
  else:
    # not an error, we are wrapping a regular node
    result.add wrongNode
    let prevError = errorSubNode(wrongNode) # find buried errors
    result.add:
      if prevError.isNil:
        noPrevError
      else:
        prevError
  result.add newIntNode(nkIntLit, ord(k))
  for a in args: result.add a

proc newError*(wrongNode: PNode; msg: string): PNode =
  ## create an `nkError` node with a `CustomError` message `msg`
  result = newError(wrongNode, CustomError, newStrNode(msg, wrongNode.info))

proc errorToString*(
    config: ConfigRef; n: PNode, rf = {renderWithoutErrorPrefix}
  ): string =
  assert n.kind == nkError
  assert n.len > 1
  let wrongNode = n[wrongNodePos]

  case ErrorKind(n[errorKindPos].intVal)
  of RawTypeMismatchError:
    result = "type mismatch"
  of ExpressionCannotBeCalled:
    result = "expression '$1' cannot be called" % wrongNode[0].renderTree(rf)
  of CustomError:
    result = n[firstArgPos].strVal
  of WrongNumberOfArguments:
    result = "wrong number of arguments"
  of ExpectedIdentifier:
    result = "identifier expected, but found '$1'" % wrongNode.renderTree(rf)
  of ExpectedIdentifierInExpr:
    result = "in expression '$1': identifier expected, but found '$2'" % [
      n[firstArgPos].renderTree(rf),
      wrongNode.renderTree(rf)
    ]
  of AmbiguousCall:
    let a = n[firstArgPos].sym
    let b = n[firstArgPos + 1].sym
    var args = "("
    for i in 1..<wrongNode.len:
      if i > 1: args.add(", ")
      args.add(typeToString(wrongNode[i].typ))
    args.add(")")
    result = "ambiguous call; both $1 and $2 match for: $3" % [
      getProcHeader(config, a),
      getProcHeader(config, b),
      args]

iterator walkErrors*(config: ConfigRef; n: PNode): PNode =
  ## traverses previous errors and yields errors from outermost to innermost.
  ## this is a linear traversal and two, or more, sibling errors will result in
  ## only the first error (per `PNode.sons`) will be yielded.
  var errNode = n # last one is the real wrongNode
  while errNode != noPrevError:
    yield errNode
    errNode = errNode[prevErrorPos]