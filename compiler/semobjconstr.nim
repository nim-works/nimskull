#
#
#           The Nim Compiler
#        (c) Copyright 2015 Nim Contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements Nim's object construction rules.
##
## # Future Considerations/Improvements:
## * The return nil pattern leads to a lot of boilerplate, most of this is for
##   `PNode`s which can be converted to use nkEmpty and/or optional.

# included from sem.nim

type
  ObjConstrContext = object
    typ: PType               # The constructed type
    initExpr: PNode          # The init expression (nkObjConstr)
    needsFullInit: bool      # A `requiresInit` derived type will
                             # set this to true while visiting
                             # parent types.
    missingFields: seq[PSym] # Fields that the user failed to specify

  InitStatus = enum # This indicates the result of object construction
    initUnknown
    initFull     # All  of the fields have been initialized
    initPartial  # Some of the fields have been initialized
    initNone     # None of the fields have been initialized
    initConflict # Fields from different branches have been initialized
    initError    # Some or all fields had errors during initialization

proc mergeInitStatus(existing: var InitStatus, newStatus: InitStatus) =
  case newStatus
  of initConflict:
    existing = newStatus
  of initPartial:
    if existing in {initUnknown, initFull, initNone}:
      existing = initPartial
  of initNone:
    if existing == initUnknown:
      existing = initNone
    elif existing == initFull:
      existing = initPartial
  of initFull:
    if existing == initUnknown:
      existing = initFull
    elif existing == initNone:
      existing = initPartial
  of initError:
    existing = initError
  of initUnknown:
    discard

proc invalidObjConstr(c: PContext, n: PNode): PNode =
  if n.kind == nkInfix and n[0].kind == nkIdent and n[0].ident.s[0] == ':':
    newError(c.config, n, rsemFieldAssignmentInvalid)

  else:
    newError(c.config, n, rsemFieldAssignmentInvalid)

proc locateFieldInInitExpr(c: PContext, field: PSym, initExpr: PNode): PNode =
  ## Returns the assignment nkExprColonExpr node, nkError if malformed, or nil
  let fieldId = field.name.id
  for i in 1..<initExpr.len:
    let
      e = initExpr[i]
      valid = e.kind == nkExprColonExpr
      partiallyValid = e.kind == nkError and
                       e.errorKind == rsemFieldOkButAssignedValueInvalid
      atLeastPartiallyValid = valid or partiallyValid
      assignment = if partiallyValid: e[wrongNodePos] else: e
      match =
        if atLeastPartiallyValid:
          let
            (ident, errNode) = considerQuotedIdent2(c, assignment[0])
            validIdent = errNode.isNil
          validIdent and fieldId == ident.id
        else:
          false

    if match: # found it!
      return e # return the error so we can handle it later
    elif not atLeastPartiallyValid:
      result = invalidObjConstr(c, assignment)
      initExpr[i] = result # remember the error and bail early
      return
    else:
      discard # keep looking

proc semConstrField(c: PContext, flags: TExprFlags,
                    field: PSym, initExpr: PNode): PNode =
  let assignment = locateFieldInInitExpr(c, field, initExpr)
  if assignment != nil:
    result = assignment
    if nfSem in result.flags:
      return
    if not fieldVisible(c, field):
      result = newError(c.config, initExpr, rsemFieldNotAccessible, args = @[
        newSymNode(field)])

      result.typ = errorType(c)
      return
    if result.kind == nkError and result.errorKind != rsemFieldOkButAssignedValueInvalid:
      return # result is the assignment error

    var initValue = semExprFlagDispatched(c, result[1], flags)
    if initValue != nil:
      initValue = fitNodeConsiderViewType(c, field.typ, initValue, result.info)
    result[0] = newSymNode(field)
    result[1] = initValue
    result.flags.incl nfSem
    if initValue != nil and initValue.kind == nkError:
      result = newError(c.config, result, rsemFieldOkButAssignedValueInvalid)

proc caseBranchMatchesExpr(branch, matched: PNode): bool =
  for i in 0..<branch.len-1:
    if branch[i].kind == nkRange:
      if overlap(branch[i], matched): return true
    elif exprStructuralEquivalent(branch[i], matched):
      return true

  return false

proc branchVals(c: PContext, caseNode: PNode, caseIdx: int,
                isStmtBranch: bool): IntSet =
  if caseNode[caseIdx].kind == nkOfBranch:
    result = initIntSet()
    for val in processBranchVals(caseNode[caseIdx]):
      result.incl(val)
  else:
    result = c.getIntSetOfType(caseNode[0].typ)
    for i in 1..<caseNode.len-1:
      for val in processBranchVals(caseNode[i]):
        result.excl(val)

proc findUsefulCaseContext(c: PContext, discrimator: PNode): (PNode, int) =
  for i in countdown(c.p.caseContext.high, 0):
    let
      (caseNode, index) = c.p.caseContext[i]
      skipped = caseNode[0].skipHidden
    if skipped.kind == nkSym and skipped.sym == discrimator.sym:
      return (caseNode, index)

proc pickCaseBranch(caseExpr, matched: PNode): PNode =
  # XXX: Perhaps this proc already exists somewhere
  let endsWithElse = caseExpr[^1].kind == nkElse
  for i in 1..<caseExpr.len - int(endsWithElse):
    if caseExpr[i].caseBranchMatchesExpr(matched):
      return caseExpr[i]

  if endsWithElse:
    return caseExpr[^1]

iterator directFieldsInRecList(recList: PNode): PNode =
  # XXX: We can remove this case by making all nkOfBranch nodes
  # regular. Currently, they try to avoid using nkRecList if they
  # include only a single field
  if recList.kind == nkSym:
    yield recList
  else:
    doAssert recList.kind == nkRecList
    for field in recList:
      if field.kind != nkSym: continue
      yield field

template quoteStr(s: string): string = "'" & s & "'"

proc fieldsPresentInInitExpr(c: PContext, fieldsRecList, initExpr: PNode): seq[PSym] =
  for field in directFieldsInRecList(fieldsRecList):
    let
      assignment = locateFieldInInitExpr(c, field.sym, initExpr)
      found = assignment != nil
    if found:
      case assignment.kind:
      of nkError:
        discard # XXX: figure out whether errors should be represented and how
      else:
        # more than just nkExprColonExpr is handled by this
        result.add field.sym

proc collectMissingFields(c: PContext, fieldsRecList: PNode,
                          constrCtx: var ObjConstrContext) =
  for r in directFieldsInRecList(fieldsRecList):
    if constrCtx.needsFullInit or
       sfRequiresInit in r.sym.flags or
       r.sym.typ.requiresInit:
      let assignment = locateFieldInInitExpr(c, r.sym, constrCtx.initExpr)
      if assignment == nil or
         assignment.kind == nkError and
         assignment.errorKind != rsemFieldOkButAssignedValueInvalid:
        constrCtx.missingFields.add r.sym

proc semConstructFields(c: PContext, n: PNode,
                        constrCtx: var ObjConstrContext,
                        flags: TExprFlags): InitStatus =
  result = initUnknown

  case n.kind
  of nkRecList:
    for field in n:
      let status = semConstructFields(c, field, constrCtx, flags)
      mergeInitStatus(result, status)

  of nkRecCase:
    template fieldsPresentInBranch(branchIdx: int): seq[PSym] =
      let branch = n[branchIdx]
      let fields = branch[^1]
      fieldsPresentInInitExpr(c, fields, constrCtx.initExpr)

    template collectMissingFields(branchNode: PNode) =
      if branchNode != nil:
        let fields = branchNode[^1]
        collectMissingFields(c, fields, constrCtx)

    let discriminator = n[0]
    internalAssert(
      c.config, discriminator.kind == nkSym, $discriminator.kind)

    var selectedBranch = -1

    for i in 1..<n.len:
      let innerRecords = n[i][^1]
      let status = semConstructFields(c, innerRecords, constrCtx, flags)
      if status notin {initNone, initUnknown}:
        mergeInitStatus(result, status)
        if selectedBranch != -1:
          localReport(c.config, constrCtx.initExpr.info, SemReport(
            kind: rsemDisjointFields,
            fieldMismatches: (
              first: fieldsPresentInBranch(selectedBranch),
              second: fieldsPresentInBranch(i))))

          result = initConflict
        else:
          selectedBranch = i

    if selectedBranch != -1:
      template badDiscriminatorError =
        if c.inUncheckedAssignSection == 0:
          let fields = fieldsPresentInBranch(selectedBranch)
          localReport(c.config, constrCtx.initExpr.info, SemReport(
            kind: rsemUnsafeRuntimeDiscriminantInit,
            fieldMismatches: (
              first: fields,
              second: @[discriminator.sym])))

        mergeInitStatus(result, initNone)

      template wrongBranchError(i) =
        if c.inUncheckedAssignSection == 0:
          let fields = fieldsPresentInBranch(i)
          localReport(c.config, constrCtx.initExpr.info,  SemReport(
            kind: rsemConflictingDiscriminantInit,
            expression: discriminatorVal,
            fieldMismatches: (
              first: fields,
              second: @[discriminator.sym])))

      template valuesInConflictError(valsDiff) =
        localReport(c.config, discriminatorVal.info, SemReport(
          kind: rsemConflictingDiscriminantValues,
          rtype: n[0].typ,
          expression: n[selectedBranch]))

      let branchNode = n[selectedBranch]
      let flags = {efPreferStatic, efPreferNilResult}
      let discrimAssign = semConstrField(c, flags,
                                         discriminator.sym,
                                         constrCtx.initExpr)
      if discrimAssign != nil and discrimAssign.kind == nkError:
        mergeInitStatus(result, initError)
        return
      var discriminatorVal = if discrimAssign.isNil: nil else: discrimAssign[1]

      if discriminatorVal != nil:
        discriminatorVal = discriminatorVal.skipHidden
        if discriminatorVal.kind notin nkLiterals and (
            not isOrdinalType(discriminatorVal.typ, true) or
            lengthOrd(c.config, discriminatorVal.typ) > MaxSetElements or
            lengthOrd(c.config, n[0].typ) > MaxSetElements):
          localReport(c.config, discriminatorVal.info, SemReport(
            kind: rsemRuntimeDiscriminantInitCap))

      if discriminatorVal == nil:
        badDiscriminatorError()
      elif discriminatorVal.kind == nkSym:
        let (ctorCase, ctorIdx) = findUsefulCaseContext(c, discriminatorVal)
        if ctorCase == nil:
          if discriminatorVal.typ.kind == tyRange:
            let rangeVals = c.getIntSetOfType(discriminatorVal.typ)
            let recBranchVals = branchVals(c, n, selectedBranch, false)
            let diff = rangeVals - recBranchVals
            if diff.len != 0:
              valuesInConflictError(diff)
          else:
            badDiscriminatorError()
        elif discriminatorVal.sym.kind notin {skLet, skParam} or
            discriminatorVal.sym.typ.kind in {tyVar}:
          if c.inUncheckedAssignSection == 0:
            localReport(c.config, discriminatorVal.info, SemReport(
              kind: rsemRuntimeDiscriminantMustBeImmutable))

        elif ctorCase[ctorIdx].kind == nkElifBranch:
          localReport(c.config, discriminatorVal.info, SemReport(
            kind: rsemRuntimeDiscriminantRequiresElif))

        else:
          var
            ctorBranchVals = branchVals(c, ctorCase, ctorIdx, true)
            recBranchVals = branchVals(c, n, selectedBranch, false)
            branchValsDiff = ctorBranchVals - recBranchVals
          if branchValsDiff.len != 0:
            valuesInConflictError(branchValsDiff)
      else:
        var failedBranch = -1
        if branchNode.kind != nkElse:
          if not branchNode.caseBranchMatchesExpr(discriminatorVal):
            failedBranch = selectedBranch
        else:
          # With an else clause, check that all other branches don't match:
          for i in 1..<n.len - 1:
            if n[i].caseBranchMatchesExpr(discriminatorVal):
              failedBranch = i
              break
        if failedBranch != -1:
          if discriminatorVal.typ.kind == tyRange:
            let rangeVals = c.getIntSetOfType(discriminatorVal.typ)
            let recBranchVals = branchVals(c, n, selectedBranch, false)
            let diff = rangeVals - recBranchVals
            if diff.len != 0:
              valuesInConflictError(diff)
          else:
            wrongBranchError(failedBranch)

      # When a branch is selected with a partial match, some of the fields
      # that were not initialized may be mandatory. We must check for this:
      if result == initPartial:
        collectMissingFields branchNode

    else:
      result = initNone
      let discrimAssign = semConstrField(c, flags + {efPreferStatic},
                                         discriminator.sym,
                                         constrCtx.initExpr)
      if discrimAssign != nil and discrimAssign.kind == nkError:
        mergeInitStatus(result, initError)
        return

      let discriminatorVal = if discrimAssign.isNil: nil else: discrimAssign[1]
      if discriminatorVal == nil:
        # None of the branches were explicitly selected by the user and no
        # value was given to the discrimator. We can assume that it will be
        # initialized to zero and this will select a particular branch as
        # a result:
        let defaultValue = newIntLit(c.graph, constrCtx.initExpr.info, 0)
        let matchedBranch = n.pickCaseBranch defaultValue
        collectMissingFields matchedBranch
      else:
        result = initPartial
        if discriminatorVal.kind == nkIntLit:
          # When the discriminator is a compile-time value, we also know
          # which branch will be selected:
          let matchedBranch = n.pickCaseBranch discriminatorVal
          if matchedBranch != nil: collectMissingFields matchedBranch
        else:
          # All bets are off. If any of the branches has a mandatory
          # fields we must produce an error:
          for i in 1..<n.len: collectMissingFields n[i]

  of nkSym:
    let field = n.sym
    let assignment = semConstrField(c, flags, field, constrCtx.initExpr)
    result =
      if assignment != nil:
        if assignment.kind == nkError:
          initError
        else:
          let initVal = assignment[1]
          if initVal != nil:
            # assignment should cover all error cases already
            initFull
          else:
            initNone
      else:
        initNone

  else:
    internalAssert(c.config, false, "")

proc semConstructTypeAux(c: PContext,
                         constrCtx: var ObjConstrContext,
                         flags: TExprFlags): InitStatus =
  result = initUnknown
  var t = constrCtx.typ
  while true:
    let status = semConstructFields(c, t.n, constrCtx, flags)
    mergeInitStatus(result, status)
    if status in {initPartial, initNone, initUnknown}:
      collectMissingFields c, t.n, constrCtx
    let base = t[0]
    if base == nil: break
    t = skipTypes(base, skipPtrs)
    if t.kind != tyObject:
      # XXX: This is not supposed to happen, but apparently
      # there are some issues in semtypinst. Luckily, it
      # seems to affect only `computeRequiresInit`.
      return
    constrCtx.needsFullInit = constrCtx.needsFullInit or
                              tfNeedsFullInit in t.flags
  if result == initError:
    constrCtx.initExpr = newError(
      c.config, constrCtx.initExpr, rsemObjectConstructorIncorrect)

proc initConstrContext(t: PType, initExpr: PNode): ObjConstrContext =
  ObjConstrContext(
    typ: t,
    initExpr: initExpr,
    needsFullInit: tfNeedsFullInit in t.flags
  )

proc computeRequiresInit(c: PContext, t: PType): bool =
  assert t.kind == tyObject
  var constrCtx = initConstrContext(t, newNode(nkObjConstr))
  let initResult = semConstructTypeAux(c, constrCtx, {})
  constrCtx.missingFields.len > 0

proc defaultConstructionError(c: PContext, t: PType, info: TLineInfo) =
  var objType = t
  while objType.kind notin {tyObject, tyDistinct}:
    objType = objType.lastSon
    assert objType != nil
  if objType.kind == tyObject:
    var constrCtx = initConstrContext(objType, newNodeI(nkObjConstr, info))
    let initResult = semConstructTypeAux(c, constrCtx, {})
    assert constrCtx.missingFields.len > 0
    localReport(c.config, info, SemReport(
      kind: rsemObjectRequiresFieldInit,
      rtype: t,
      candidates: constrCtx.missingFields))

  elif objType.kind == tyDistinct:
    localReport(c.config, info, SemReport(
      kind: rsemDistinctDoesNotHaveDefaultValue, rtype: t))

  else:
    assert false, "Must not enter here."

proc semObjConstr(c: PContext, n: PNode, flags: TExprFlags): PNode =
  var t = semTypeNode(c, n[0], nil)
  result = newNodeIT(nkObjConstr, n.info, t)
  for child in n: result.add child

  if t == nil:
    return newError(c.config, result, rsemExpectedObjectType)

  t = skipTypes(t, {tyGenericInst, tyAlias, tySink, tyOwned})
  if t.kind == tyRef:
    t = skipTypes(t[0], {tyGenericInst, tyAlias, tySink, tyOwned})
    if optOwnedRefs in c.config.globalOptions:
      result.typ = makeVarType(c, result.typ, tyOwned)
      # we have to watch out, there are also 'owned proc' types that can be used
      # multiple times as long as they don't have closures.
      result.typ.flags.incl tfHasOwned
  if t.kind != tyObject:
    return newError(c.config, result, SemReport(
      kind: rsemExpectedObjectType, rtype: t))

  # Check if the object is fully initialized by recursively testing each
  # field (if this is a case object, initialized fields in two different
  # branches will be reported as an error):
  var constrCtx = initConstrContext(t, result)
  let
    initResult = semConstructTypeAux(c, constrCtx, flags)
    missedFields = constrCtx.missingFields.len > 0
    constructionError = initResult == initError
  var hasError = constructionError or missedFields
    ## needed to split error detect/report for better msgs

  # It's possible that the object was not fully initialized while
  # specifying a .requiresInit. pragma:
  if missedFields:
    localReport(c.config, result.info, SemReport(
      kind: rsemObjectRequiresFieldInit,
      rtype: t,
      candidates: constrCtx.missingFields))

  if constructionError:
    result = constrCtx.initExpr
    return

  # Since we were traversing the object fields, it's possible that
  # not all of the fields specified in the constructor was visited.
  # We'll check for such fields here:
  for i in 1..<result.len:
    let field = result[i]
    if nfSem notin field.flags:
      # handle various error cases first
      case field.kind
      of nkError:
        # skip nkError so we report it later
        continue
      of nkExprColonExpr:
        # process later in the loop
        discard
      else:
        let e =
          if field.kind == nkError:
            field
          else:
            invalidObjConstr(c, field)
        # XXX: shouldn't report errors here, since creating and reporting split
        #      need to cascade an nkError instead
        localReport(c.config, e)
        hasError = true
        continue

      let id = considerQuotedIdent(c, field[0])
      # This node was not processed. There are two possible reasons:
      # 1) It was shadowed by a field with the same name on the left
      for j in 1..<i:
        let prevId = considerQuotedIdent(c, result[j][0])
        if prevId.id == id.id:
          localReport(c.config, field.info, SemReport(
            kind: rsemFieldInitTwice, expression: result[j][0]))

          hasError = true
          break
      # 2) No such field exists in the constructed type

      localReport(c.config, field[0].info, SemReport(
        kind: rsemUndeclaredField,
        expression: field[0], rtype: t))

      hasError = true
      break

  if initResult == initFull:
    incl result.flags, nfAllFieldsSet

  # wrap in an error see #17437
  if hasError:
    result = newError(c.config, result, rsemObjectConstructorIncorrect)
    result.typ = errorType(c)
