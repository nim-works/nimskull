#
#
#           The Nim Compiler
#        (c) Copyright 2021 Saem Ghani
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## module handles reporting errors, it's used in conjunction with errorhandling
##
## Todo:
## * write an error reporting proc that handles string conversion and also
##   determines which error handling strategy to use doNothing, raise, etc.

import ast, errorhandling, renderer, strutils, astmsgs, types, options
from msgs import TErrorHandling

export compilerInstInfo, walkErrors, errorKind
# export because keeping the declaration in `errorhandling` acts as a reminder
# of what the contract is with the subtleties around line and column info
# overloading

proc errorHandling*(err: PNode): TErrorHandling =
  ## which error handling strategy should be used given the error, use with
  ## `msg.liMessage` when reporting errors.
  assert err.isError, "err can't be nil and must be an nkError"
  case err.errorKind:
    of CustomGlobalError: doRaise
    of FatalError: doAbort
    else: doNothing

proc `$`(info: InstantiationInfo): string =
  ## prints the compiler line info in `filepath(line, column)` format
  "$1($2, $3)" % [ info.filename, $info.line.int, $info.column.int ]

proc errorToString*(
    config: ConfigRef; n: PNode, rf = {renderWithoutErrorPrefix}
  ): string =
  ## converts an error node into a string representation for reporting
  
  # xxx: note the schema/structure of each error kind
  assert n.kind == nkError, "not an error '$1'" % n.renderTree(rf)
  assert n.len > 1
  let wrongNode = n[wrongNodePos]

  case ErrorKind(n[errorKindPos].intVal)
  of CustomError, CustomGlobalError, CustomUserError:
    result = n[firstArgPos].strVal
  of CustomPrintMsgAndNodeError:
    result = "$1$2" % [ n[firstArgPos].strVal, n[wrongNodePos].renderTree(rf) ]
  of RawTypeMismatchError:
    result = "type mismatch"
  of FatalError:
    result = "Fatal: $1" % n[firstArgPos].strVal
  of CallTypeMismatch:
    result = "type mismatch: got <"
    var hasErrorType = false
    for i in 1..<wrongNode.len:
      if i > 1: result.add(", ")
      let nt = wrongNode[i].typ
      result.add(typeToString(nt))
      if nt.kind == tyError:
        hasErrorType = true
        break
    if not hasErrorType:
      let typ = wrongNode[0].typ
      result.add(">\nbut expected one of:\n$1" % typeToString(typ))
      if typ.sym != nil and sfAnon notin typ.sym.flags and typ.kind == tyProc:
        # when can `typ.sym != nil` ever happen?
        result.add(" = $1" % typeToString(typ, preferDesc))
      result.addDeclaredLocMaybe(config, typ)
  of ExpressionCannotBeCalled:
    result = "expression '$1' cannot be called" % wrongNode[0].renderTree(rf)
  of WrongNumberOfArguments:
    result = "wrong number of arguments"
  of AmbiguousCall:
    let
      a = n[firstArgPos].sym
      b = n[firstArgPos + 1].sym
    var args = "("
    for i in 1..<wrongNode.len:
      if i > 1: args.add(", ")
      args.add(typeToString(wrongNode[i].typ))
    args.add(")")
    result = "ambiguous call; both $1 and $2 match for: $3" % [
      getProcHeader(config, a),
      getProcHeader(config, b),
      args]
  of CallingConventionMismatch:
    result = n[firstArgPos].strVal
  of UndeclaredIdentifier:
    let
      identName = n[firstArgPos].strVal
      optionalExtraErrMsg = if n.len > firstArgPos + 1: n[firstArgPos + 1].strVal else: ""
    result = "undeclared identifier: '$1'$2" % [identName, optionalExtraErrMsg]
  of ExpectedIdentifier:
    result = "identifier expected, but found '$1'" % wrongNode.renderTree(rf)
  of ExpectedIdentifierInExpr:
    result = "in expression '$1': identifier expected, but found '$2'" % [
      n[firstArgPos].renderTree(rf),
      wrongNode.renderTree(rf)
    ]
  of FieldNotAccessible:
    result = "the field '$1' is not accessible." % n[firstArgPos].sym.name.s
  of FieldAssignmentInvalid, FieldOkButAssignedValueInvalid:
    let
      hasHint = n.len > firstArgPos
      hint = if hasHint: "; " & n[firstArgPos].renderTree(rf) else: ""
    result = "Invalid field assignment '$1'$2" % [
      wrongNode.renderTree(rf),
      hint,
    ]
  of ObjectConstructorIncorrect:
    result = "Invalid object constructor: '$1'" % wrongNode.renderTree(rf)
  of ExpressionHasNoType:
    result = "expression '$1' has no type (or is ambiguous)" % [
        n[firstArgPos].renderTree(rf)
      ]
  of StringLiteralExpected:
    result = "string literal expected"
  of IntLiteralExpected:
    result = "int literal expected"
  of InvalidPragma:
    result = "invalid pragma: $1" % wrongNode.renderTree(rf)
  of IllegalCustomPragma:
    result = "cannot attach a custom pragma to '$1'" % n[firstArgPos].sym.name.s
  of NoReturnHasReturn:
    result = ".noreturn with return type not allowed"
  of ImplicitPragmaError:
    result = "" # treat as a wrapper
  of PragmaDynlibRequiresExportc:
    result = ".dynlib requires .exportc"
  of WrappedError:
    result = ""
  
  # useful for debugging where error nodes are generated
  # result = result & " compiler error origin: " & $n.compilerInstInfo()

template messageError*(config: ConfigRef; err: PNode) =
  ## report errors, call this on a per error basis, as you would receive from
  ## `errorhandling.walkErrors`
  msgs.liMessage(
    conf  = config,
    info  = err.info,
    msg   =
      case err.errorKind:
        of FatalError: errFatal
        of CustomUserError: errUser
        else: errGenerated,
    arg   = errorreporting.errorToString(config, err),
    eh    = err.errorHandling,
    info2 = err.compilerInstInfo
  )