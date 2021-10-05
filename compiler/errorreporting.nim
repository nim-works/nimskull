# module handles reporting errors

import errorhandling, strutils, astmsgs, types, options

proc errorToString*(
    config: ConfigRef; n: PNode, rf = {renderWithoutErrorPrefix}
  ): string =
  assert n.kind == nkError, "not an error '$1'" % n.renderTree(rf)
  assert n.len > 1
  let wrongNode = n[wrongNodePos]

  case ErrorKind(n[errorKindPos].intVal)
  of CustomError:
    result = n[firstArgPos].strVal
  of RawTypeMismatchError:
    result = "type mismatch"
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

iterator walkErrors*(config: ConfigRef; n: PNode): PNode =
  ## traverses previous errors and yields errors from  innermost to outermost.
  ## this is a linear traversal and two, or more, sibling errors will result in
  ## only the first error (per `PNode.sons`) being yielded.
  
  # first collect all the nodes by depth
  var errNodes = @[n]
  while errNodes[^1][prevErrorPos] != nil and errNodes[^1][prevErrorPos].kind != nkEmpty:
    # check nkEmpty and not noPrevNode because tree copies break references
    errNodes.add errNodes[^1][prevErrorPos]
  
  # report from last to first (deepest in tree to highest)
  for i in 1..errNodes.len:
    # reverse index so we go from the innermost to outermost
    yield errNodes[^i]