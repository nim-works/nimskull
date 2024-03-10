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

## included from sem.nim

type
  ObjConstrContext = object
    typ: PType               ## The constructed type
    initExpr: PNode          ## The init expression (nkObjConstr)
    needsFullInit: bool      ## A `requiresInit` derived type will
                             ## set this to true while visiting
                             ## parent types.
    missingFields: seq[PSym] ## Fields that the user failed to specify

  InitStatus = enum ## This indicates the result of object construction
    initUnknown
    initFull     ## All  of the fields have been initialized
    initPartial  ## Some of the fields have been initialized
    initNone     ## None of the fields have been initialized
    initConflict ## Fields from different branches have been initialized
    initError    ## Some or all fields had errors during initialization

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

proc locateFieldInInitExpr(c: PContext, field: PSym, initExpr: PNode): PNode =
  ## Returns the assignment ``nkExprColonExpr`` node, or nil, if there's none
  ## for `field`.
  for i in 1..<initExpr.len:
    let e = initExpr[i]
    case e.kind
    of nkExprColonExpr:
      if e[0].kind == nkSym and e[0].sym.id == field.id:
        # found it!
        return e
      # continue searching
    of nkError:
      discard "skip errors"
    else:
      unreachable()

  # assignment for field is missing
  result = nil

proc caseBranchMatchesExpr(branch, matched: PNode): bool =
  for i in 0..<branch.len-1:
    if branch[i].kind == nkRange:
      if overlap(branch[i], matched): return true
    elif exprStructuralEquivalent(branch[i], matched):
      # comment equality doesn't matter here (AST is comment free from semConstExpr or a nkIntLit)
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
  for i in countdown(c.execCon.caseContext.high, 0):
    let
      (caseNode, index) = c.execCon.caseContext[i]
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

proc fieldsPresentInInitExpr(c: PContext, fieldsRecList, initExpr: PNode): seq[PSym] =
  for field in directFieldsInRecList(fieldsRecList):
    if locateFieldInInitExpr(c, field.sym, initExpr) != nil:
      result.add field.sym

proc collectMissingFields(c: PContext, fieldsRecList: PNode,
                          constrCtx: var ObjConstrContext) =
  for r in directFieldsInRecList(fieldsRecList):
    if constrCtx.needsFullInit or
       sfRequiresInit in r.sym.flags or
       r.sym.typ.requiresInit:
      if locateFieldInInitExpr(c, r.sym, constrCtx.initExpr).isNil:
        constrCtx.missingFields.add r.sym

proc checkConstructFields(c: PContext, n: PNode,
                          constrCtx: var ObjConstrContext): InitStatus =
  result = initUnknown

  case n.kind
  of nkRecList:
    for field in n:
      let status = checkConstructFields(c, field, constrCtx)
      mergeInitStatus(result, status)

  of nkRecCase:
    template fieldsPresentInBranch(branchIdx: int): seq[PSym] =
      # XXX: this is related to diagnostics rendering, it should not be
      #      located here, but rather in the rendering layer
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
      let status = checkConstructFields(c, innerRecords, constrCtx)
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
            ast: discriminatorVal,
            fieldMismatches: (
              first: fields,
              second: @[discriminator.sym])))

      template valuesInConflictError(valsDiff: IntSet): untyped =
        ## Write out 'conflicting discriminant values' report
        var rep = reportTyp(
          rsemConflictingDiscriminantValues,
          n[0].typ, str = $selectedBranch)

        rep.nodes = toLiterals(valsDiff, rep.typ)

        localReport(c.config, discriminatorVal.info, rep)

      let branchNode = n[selectedBranch]
      let discrimAssign = locateFieldInInitExpr(c, discriminator.sym,
                                                constrCtx.initExpr)
      var discriminatorVal = if discrimAssign.isNil: nil else: discrimAssign[1]

      if discriminatorVal != nil and discriminatorVal.kind != nkError:
        discriminatorVal = discriminatorVal.skipHidden
        if discriminatorVal.kind notin nkLiterals - nkNilLit and (
            not isOrdinalType(discriminatorVal.typ, true) or
            lengthOrd(c.config, discriminatorVal.typ) > MaxSetElements or
            lengthOrd(c.config, n[0].typ) > MaxSetElements):
          localReport(c.config, discriminatorVal.info, SemReport(
            kind: rsemRuntimeDiscriminantInitCap))

      if discriminatorVal == nil:
        badDiscriminatorError()
      elif discriminatorVal.kind == nkError:
        mergeInitStatus(result, initError)
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
      let discrimAssign = locateFieldInInitExpr(c, discriminator.sym,
                                                constrCtx.initExpr)

      let discriminatorVal = if discrimAssign.isNil: nil else: discrimAssign[1]
      if discriminatorVal == nil:
        # None of the branches were explicitly selected by the user and no
        # value was given to the discrimator. We can assume that it will be
        # initialized to zero and this will select a particular branch as
        # a result:
        let defaultValue = newIntLit(c.graph, constrCtx.initExpr.info, 0)
        let matchedBranch = n.pickCaseBranch defaultValue
        collectMissingFields matchedBranch
      elif discriminatorVal.kind == nkError:
        mergeInitStatus(result, initError)
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
    let assignment = locateFieldInInitExpr(c, field, constrCtx.initExpr)
    # error or not, if the field has an assginment, we treat it as
    # fully initialized
    result =
      if assignment != nil: initFull
      else:                 initNone

  else:
    unreachable(n.kind)

proc checkConstructTypeAux(c: PContext,
                           constrCtx: var ObjConstrContext): InitStatus =
  result = initUnknown
  var t = constrCtx.typ
  while true:
    let status = checkConstructFields(c, t.n, constrCtx)
    mergeInitStatus(result, status)
    if status in {initPartial, initNone, initUnknown}:
      collectMissingFields c, t.n, constrCtx
    let base = t[0]
    if base == nil: break
    t = skipTypes(base, skipPtrs)
    constrCtx.needsFullInit = constrCtx.needsFullInit or
                              tfNeedsFullInit in t.flags

proc initConstrContext(t: PType, initExpr: PNode): ObjConstrContext =
  ObjConstrContext(
    typ: t,
    initExpr: initExpr,
    needsFullInit: tfNeedsFullInit in t.flags
  )

proc computeRequiresInit(c: PContext, t: PType): bool =
  assert t.kind == tyObject
  var constrCtx = initConstrContext(t, newNode(nkObjConstr))
  discard checkConstructTypeAux(c, constrCtx)
  constrCtx.missingFields.len > 0

proc defaultConstructionError(c: PContext, t: PType, n: PNode): PNode =
  var objType = t
  
  while objType.kind notin {tyObject, tyDistinct}:
    objType = objType.lastSon
    assert objType != nil
  
  case objType.kind
  of tyObject:
    var constrCtx = initConstrContext(objType, newNodeI(nkObjConstr, n.info))
    discard checkConstructTypeAux(c, constrCtx)
    if constrCtx.missingFields.len > 0:
      c.config.newError(
                  n,
                  PAstDiag(
                    kind: adSemObjectRequiresFieldInitNoDefault,
                    missing: constrCtx.missingFields,
                    objTyp: t))
    else:
      c.config.newError(n,
                PAstDiag(kind: adSemObjectDoesNotHaveDefaultValue,
                         typWithoutDefault: t))

  of tyDistinct:
    c.config.newError(n,
                PAstDiag(kind: adSemDistinctDoesNotHaveDefaultValue,
                         typWithoutDefault: t))

  else:
    unreachable "Must not enter here."

proc semObjConstr(c: PContext, n: PNode, flags: TExprFlags): PNode =
  var t = semTypeNode(c, n[0], nil)
  result = newNodeIT(nkObjConstr, n.info, t)
  for child in n: result.add child

  if t == nil:
    return newError(c.config, result, PAstDiag(kind: adSemExpectedObjectType))

  t = skipTypes(t, {tyGenericInst, tyAlias, tySink})
  if t.kind == tyRef:
    t = skipTypes(t[0], {tyGenericInst, tyAlias, tySink})

  if t.kind != tyObject:
    return newError(c.config, result,
                    PAstDiag(kind: adSemExpectedObjectOfType,
                             expectedObjTyp: t))

  proc lookupInType(typ: PType, field: PIdent): PSym =
    ## Searches for a field with the given identifier (`field`) in the object
    ## type hierarchy of `typ`.
    var typ = typ
    while typ != nil:
      typ = typ.skipTypes(skipPtrs)
      result = lookupInRecord(typ.n, field)
      if result != nil:
        return
      typ = typ.base

  # phase 1: verify that the AST is valid and has unambiguous meaning
  for i in 1..<n.len:
    let it = n[i]

    if it.kind != nkExprColonExpr or it.len != 2 or
       it[0].kind notin nkIdentKinds:
      # the AST is invalid; skip
      result[i] = newError(c.config, it):
        PAstDiag(kind: adSemFieldAssignmentInvalid)
      continue

    # don't perform anything like fitting the type or folding the expression
    # here; we just want the typed AST
    let
      (ident, err) = considerQuotedIdent(c, it[0])
      field =
        if err.isNil:
          let s = lookupInType(t, ident)
          if s != nil:
            newSymNode(s, it[0].info)
          else:
            newError(c.config, it[0]):
              PAstDiag(kind: adSemUndeclaredField, givenSym: t.sym, symTyp: t)
        else:
          err

    let elem = shallowCopy(it)
    elem[0] = field
    elem[1] = semExprWithType(c, it[1])
    result[i] = elem

  # phase 2: ensure that the valid parts of the typed construction expression
  # adhere to the language rules. For the first pass, we only consider the
  # rules that don't require a complex type traversal
  var seen = initIntSet()
  for i in 1..<result.len:
    let it = result[i]
    if it.kind == nkError or it[0].kind == nkError:
      continue # invalid AST or no valid field symbol; ignore

    # post-process the initializer expression:
    var val = it[1]
    if sfDiscriminant in it[0].sym.flags:
      # we prefer a compile-time-known value
      val = getConstExpr(c.module, val, c.idgen, c.graph)
      if val == nil:
        val = evalAtCompileTime(c, it[1])

    result[i][1] = fitNodeConsiderViewType(c, it[0].typ, val, val.info)

    # now check whether it's legal for the field to appear in the construction.
    # Visibility errors take precedence over multiple initializations
    if not fieldVisible(c, it[0].sym):
      result[i][0] = newError(c.config, it[0]):
        PAstDiag(kind: adSemFieldNotAccessible, inaccessible: it[0].sym)
    elif containsOrIncl(seen, it[0].sym.id):
      # duplicate field initialization
      result[i][0] = newError(c.config, it[0]):
        PAstDiag(kind: adSemFieldInitTwice, dupFld: it[0].sym.name)

  # now verify that the language rules requiring a complex type traversal are
  # not violated. These are:
  # 1. all fields of the object that are required to be initialized are
  #    initialized
  # 2. an initialized discriminated field's branch must be proven to be active
  var constrCtx = initConstrContext(t, result)
  let
    initResult = checkConstructTypeAux(c, constrCtx)
    missedFields = constrCtx.missingFields.len > 0
    constructionError = initResult == initError
  var hasError = constructionError or missedFields
    ## needed to split error detect/report for better msgs

  if not hasError:
    # we're not tracking the error nodes and thus have to look for
    # them here
    for i in 1..<n.len:
      let it = result[i]
      if it.kind == nkError or nkError in {it[0].kind, it[1].kind}:
        hasError = true
        break

  # It's possible that the object was not fully initialized while
  # specifying a .requiresInit. pragma:
  if missedFields:
    result = newError(c.config, result):
      PAstDiag(kind: adSemObjectRequiresFieldInit,
               missing: constrCtx.missingFields,
               objTyp: t)
  elif hasError:
    # wrap in an error see #17437
    result = wrapError(c.config, result)
