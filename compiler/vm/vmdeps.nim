#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/[
    os
  ],
  compiler/ast/[
    types,
    ast,
    idents,
    lineinfos,
    parser,
  ],
  compiler/front/[
    cli_reporter,
    msgs,
    options
  ],
  compiler/utils/[
    pathutils,
    idioms
  ],
  experimental/[
    results
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import reportStr
from compiler/ast/report_enums import ReportKind,
  ReportCategory

# xxx: `Report` is faaaar too wide a type for what the VM needs, even with all
#      the ground that can cover.
from compiler/ast/reports import Report,
  ReportSeverity,
  kind

proc opSlurp*(file: string, info: TLineInfo, module: PSym; conf: ConfigRef): string =
  try:
    var filename = parentDir(toFullPath(conf, info)) / file
    if not fileExists(filename):
      filename = findFile(conf, file).string
    result = readFile(filename)
    # we produce a fake include statement for every slurped filename, so that
    # the module dependencies are accurate:
    discard conf.fileInfoIdx(AbsoluteFile filename)
    appendToModule(module, newTreeI(nkIncludeStmt, info, newStrNode(nkStrLit, filename)))
  except IOError:
    localReport(conf, info, reportStr(rsemCannotOpenFile, file))
    result = ""

proc atomicTypeX(cache: IdentCache; name: string; m: TMagic; t: PType; info: TLineInfo;
                 idgen: IdGenerator): PNode =
  let sym = newSym(skType, getIdent(cache, name), nextSymId(idgen), t.owner, info)
  sym.magic = m
  sym.typ = t
  result = newSymNode(sym)
  result.typ = t

proc atomicTypeX(s: PSym; info: TLineInfo): PNode =
  result = newSymNode(s)
  result.info = info

proc mapTypeToAstX(cache: IdentCache; t: PType; info: TLineInfo; idgen: IdGenerator;
                   inst=false; allowRecursionX=false): PNode

proc mapTypeToBracketX(cache: IdentCache; name: string; m: TMagic; t: PType; info: TLineInfo;
                       idgen: IdGenerator;
                       inst=false): PNode =
  result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
  result.add atomicTypeX(cache, name, m, t, info, idgen)
  for i in 0..<t.len:
    if t[i] == nil:
      let void = atomicTypeX(cache, "void", mVoid, t, info, idgen)
      void.typ = newType(tyVoid, nextTypeId(idgen), t.owner)
      result.add void
    else:
      result.add mapTypeToAstX(cache, t[i], info, idgen, inst)

proc objectNode(cache: IdentCache; n: PNode; idgen: IdGenerator): PNode =
  if n.kind == nkSym:
    result = newNodeI(nkIdentDefs, n.info)
    result.add n  # name
    result.add mapTypeToAstX(cache, n.sym.typ, n.info, idgen, true, false)  # type
    result.add newNodeI(nkEmpty, n.info)  # no assigned value
  else:
    result = copyNode(n)
    for i in 0..<n.safeLen:
      result.add objectNode(cache, n[i], idgen)

proc mapTypeToAstX(cache: IdentCache; t: PType; info: TLineInfo;
                   idgen: IdGenerator;
                   inst=false; allowRecursionX=false): PNode =
  var allowRecursion = allowRecursionX
  template atomicType(name, m): untyped = atomicTypeX(cache, name, m, t, info, idgen)
  template atomicType(s): untyped = atomicTypeX(s, info)
  template mapTypeToAst(t,info): untyped = mapTypeToAstX(cache, t, info, idgen, inst)
  template mapTypeToAstR(t,info): untyped = mapTypeToAstX(cache, t, info, idgen, inst, true)
  template mapTypeToAst(t,i,info): untyped =
    if i<t.len and t[i]!=nil: mapTypeToAstX(cache, t[i], info, idgen, inst)
    else: newNodeI(nkEmpty, info)
  template mapTypeToBracket(name, m, t, info): untyped =
    mapTypeToBracketX(cache, name, m, t, info, idgen, inst)
  template newNodeX(kind): untyped =
    newNodeIT(kind, if t.n.isNil: info else: t.n.info, t)
  template newIdentDefs(n,t): untyped =
    var id = newNodeX(nkIdentDefs)
    id.add n  # name
    id.add mapTypeToAst(t, info)  # type
    id.add newNodeI(nkEmpty, info)  # no assigned value
    id
  template newIdentDefs(s): untyped = newIdentDefs(s, s.typ)

  if inst and not allowRecursion and t.sym != nil:
    # getTypeInst behavior: return symbol
    return atomicType(t.sym)

  case t.kind
  of tyNone: result = atomicType("none", mNone)
  of tyBool: result = atomicType("bool", mBool)
  of tyChar: result = atomicType("char", mChar)
  of tyNil: result = atomicType("nil", mNil)
  of tyUntyped: result = atomicType("untyped", mExpr)
  of tyTyped: result = atomicType("typed", mStmt)
  of tyVoid: result = atomicType("void", mVoid)
  of tyEmpty: result = atomicType("empty", mNone)
  of tyUncheckedArray:
    result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
    result.add atomicType("UncheckedArray", mUncheckedArray)
    result.add mapTypeToAst(t[0], info)
  of tyArray:
    result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
    result.add atomicType("array", mArray)
    if inst and t[0].kind == tyRange:
      var rng = newNodeX(nkInfix)
      rng.add newIdentNode(getIdent(cache, ".."), info)
      rng.add t[0].n[0].copyTree
      rng.add t[0].n[1].copyTree
      result.add rng
    else:
      result.add mapTypeToAst(t[0], info)
    result.add mapTypeToAst(t[1], info)
  of tyTypeDesc:
    if t.base != nil:
      result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
      result.add atomicType("typeDesc", mTypeDesc)
      result.add mapTypeToAst(t.base, info)
    else:
      result = atomicType("typeDesc", mTypeDesc)
  of tyGenericInvocation:
    result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
    for i in 0..<t.len:
      result.add mapTypeToAst(t[i], info)
  of tyGenericInst:
    if inst and allowRecursion:
      result = mapTypeToAstR(t.lastSon, info)
    else:
      result = newNodeX(nkBracketExpr)
      result.add mapTypeToAst(t[0], info)
      for i in 1..<t.len-1:
        result.add mapTypeToAst(t[i], info)
  of tyGenericBody:
    if inst:
      result = mapTypeToAstR(t.lastSon, info)
    else:
      result = mapTypeToAst(t.lastSon, info)
  of tyAlias:
    result = mapTypeToAstX(cache, t.lastSon, info, idgen, inst, allowRecursion)
  of tyOrdinal:
    result = mapTypeToAst(t.lastSon, info)
  of tyDistinct:
    if inst:
      result = newNodeX(nkDistinctTy)
      result.add mapTypeToAst(t[0], info)
    else:
      if allowRecursion or t.sym == nil:
        result = mapTypeToBracket("distinct", mDistinct, t, info)
      else:
        result = atomicType(t.sym)
  of tyGenericParam, tyForward:
    result = atomicType(t.sym)
  of tyObject:
    if inst:
      result = newNodeX(nkObjectTy)
      var objectDef = t.sym.ast[2]
      if objectDef.kind == nkRefTy:
        objectDef = objectDef[0]
      result.add objectDef[0].copyTree  # copy object pragmas
      if t[0] == nil:
        result.add newNodeI(nkEmpty, info)
      else:  # handle parent object
        var nn = newNodeX(nkOfInherit)
        nn.add mapTypeToAst(t[0], info)
        result.add nn
      if t.n.len > 0:
        result.add objectNode(cache, t.n, idgen)
      else:
        result.add newNodeI(nkEmpty, info)
    else:
      if allowRecursion or t.sym == nil:
        result = newNodeIT(nkObjectTy, if t.n.isNil: info else: t.n.info, t)
        result.add newNodeI(nkEmpty, info)
        if t[0] == nil:
          result.add newNodeI(nkEmpty, info)
        else:
          result.add mapTypeToAst(t[0], info)
        result.add copyTree(t.n)
      else:
        result = atomicType(t.sym)
  of tyEnum:
    result = newNodeIT(nkEnumTy, if t.n.isNil: info else: t.n.info, t)
    result.add newNodeI(nkEmpty, info)  # pragma node, currently always empty for enum
    for c in t.n.sons:
      result.add copyTree(c)
  of tyTuple:
    if inst:
      # only named tuples have a node, unnamed tuples don't
      if t.n.isNil:
        result = newNodeX(nkTupleConstr)
        for subType in t.sons:
          result.add mapTypeToAst(subType, info)
      else:
        result = newNodeX(nkTupleTy)
        for s in t.n.sons:
          result.add newIdentDefs(s)
    else:
      result = mapTypeToBracket("tuple", mTuple, t, info)
  of tySet: result = mapTypeToBracket("set", mSet, t, info)
  of tyPtr:
    if inst:
      result = newNodeX(nkPtrTy)
      result.add mapTypeToAst(t[0], info)
    else:
      result = mapTypeToBracket("ptr", mPtr, t, info)
  of tyRef:
    if inst:
      result = newNodeX(nkRefTy)
      result.add mapTypeToAst(t[0], info)
    else:
      result = mapTypeToBracket("ref", mRef, t, info)
  of tyVar:
    if inst:
      result = newNodeX(nkVarTy)
      result.add mapTypeToAst(t[0], info)
    else:
      result = mapTypeToBracket("var", mVar, t, info)
  of tyLent: result = mapTypeToBracket("lent", mBuiltinType, t, info)
  of tySink: result = mapTypeToBracket("sink", mBuiltinType, t, info)
  of tySequence: result = mapTypeToBracket("seq", mSeq, t, info)
  of tyProc:
    if inst:
      result = newNodeX(nkProcTy)
      var fp = newNodeX(nkFormalParams)
      if t[0] == nil:
        fp.add newNodeI(nkEmpty, info)
      else:
        fp.add mapTypeToAst(t[0], t.n[0].info)
      for i in 1..<t.len:
        fp.add newIdentDefs(t.n[i], t[i])
      result.add fp
      result.add if t.n[0].len > 0: t.n[0][pragmasEffects].copyTree
                 else: newNodeI(nkEmpty, info)
    else:
      result = mapTypeToBracket("proc", mNone, t, info)
  of tyOpenArray: result = mapTypeToBracket("openArray", mOpenArray, t, info)
  of tyRange:
    result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
    result.add atomicType("range", mRange)
    if inst and t.n.len == 2:
      let rng = newNodeX(nkInfix)
      rng.add newIdentNode(getIdent(cache, ".."), info)
      rng.add t.n[0].copyTree
      rng.add t.n[1].copyTree
      result.add rng
    else:
      result.add t.n[0].copyTree
      if t.n.len > 1:
        result.add t.n[1].copyTree
  of tyPointer: result = atomicType("pointer", mPointer)
  of tyString: result = atomicType("string", mString)
  of tyCstring: result = atomicType("cstring", mCstring)
  of tyInt: result = atomicType("int", mInt)
  of tyInt8: result = atomicType("int8", mInt8)
  of tyInt16: result = atomicType("int16", mInt16)
  of tyInt32: result = atomicType("int32", mInt32)
  of tyInt64: result = atomicType("int64", mInt64)
  of tyFloat: result = atomicType("float", mFloat)
  of tyFloat32: result = atomicType("float32", mFloat32)
  of tyFloat64: result = atomicType("float64", mFloat64)
  of tyUInt: result = atomicType("uint", mUInt)
  of tyUInt8: result = atomicType("uint8", mUInt8)
  of tyUInt16: result = atomicType("uint16", mUInt16)
  of tyUInt32: result = atomicType("uint32", mUInt32)
  of tyUInt64: result = atomicType("uint64", mUInt64)
  of tyVarargs: result = mapTypeToBracket("varargs", mVarargs, t, info)
  of tyProxy: result = atomicType("error", mNone)
  of tyBuiltInTypeClass:
    # the type stored in the type class is not necessarily
    # valid. We need to manually map the type.
    result = newNodeIT(nkBracketExpr, info, t)
    result.add atomicTypeX(cache, "builtinTypeClass", mNone, t, info, idgen)
    template elem(kind): PNode =
      newNodeIT(kind, info, t.base)
    result.add:
      case t.base.kind
      of tyDistinct: elem(nkDistinctTy)
      of tyEnum:     elem(nkEnumTy)
      of tyObject:   elem(nkObjectTy)
      of tyTuple:    elem(nkTupleClassTy)
      of tyVar:      elem(nkVarTy)
      of tyProc:     elem(nkProcTy)
      of tyPtr:      elem(nkPtrTy)
      of tyRef:      elem(nkRefTy)
      of tyOrdinal, tyArray, tySet, tyRange, tySequence, tyOpenArray, tyLent,
         tyVarargs, tyUncheckedArray:
        # use the symbol of the type stored in the type class
        newSymNode(t.base.sym, info)
      of {low(TTypeKind)..high(TTypeKind)} - tyBuiltInTypeClasses:
        unreachable(t.base.kind)
  of tyUserTypeClass, tyUserTypeClassInst:
    if t.isResolvedUserTypeClass:
      result = mapTypeToAst(t.lastSon, info)
    else:
      result = mapTypeToBracket("concept", mNone, t, info)
      result.add t.n.copyTree
  of tyCompositeTypeClass:
    result = mapTypeToBracket("compositeTypeClass", mNone, t, info)
  of tyAnd: result = mapTypeToBracket("and", mAnd, t, info)
  of tyOr: result = mapTypeToBracket("or", mOr, t, info)
  of tyNot: result = mapTypeToBracket("not", mNot, t, info)
  of tyAnything: result = atomicType("anything", mNone)
  of tyInferred: result = mapTypeToAstX(cache, t.lastSon, info, idgen, inst, allowRecursion)
  of tyStatic, tyFromExpr:
    if inst:
      if t.n != nil: result = t.n.copyTree
      else: result = atomicType("void", mVoid)
    else:
      result = newNodeIT(nkBracketExpr, if t.n.isNil: info else: t.n.info, t)
      result.add atomicType("static", mNone)
      if t.n != nil:
        result.add t.n.copyTree

proc opMapTypeToAst*(cache: IdentCache; t: PType; info: TLineInfo; idgen: IdGenerator): PNode =
  result = mapTypeToAstX(cache, t, info, idgen, inst=false, allowRecursionX=true)

# the "Inst" version includes generic parameters in the resulting type tree
# and also tries to look like the corresponding Nim type declaration
proc opMapTypeInstToAst*(cache: IdentCache; t: PType; info: TLineInfo; idgen: IdGenerator): PNode =
  result = mapTypeToAstX(cache, t, info, idgen, inst=true, allowRecursionX=false)

# the "Impl" version includes generic parameters in the resulting type tree
# and also tries to look like the corresponding Nim type implementation
proc opMapTypeImplToAst*(cache: IdentCache; t: PType; info: TLineInfo; idgen: IdGenerator): PNode =
  result = mapTypeToAstX(cache, t, info, idgen, inst=true, allowRecursionX=true)

proc parseCode*(code: string, cache: IdentCache, config: ConfigRef,
                filename: string, line: int): Result[PNode, Report] =
  ## Invokes the parser for the given `code` and returns either the parsed AST
  ## or the first error that occurred during parsing

  # @haxscramper: REFACTOR because parsing can expectedly fail (for example
  # `"{a+}"` in strformat module) we need to handle failure and report it as
  # a *VM* exception, so user can properly handle this.
  #
  # Previous implementation also relied on the report hook override -
  # in the future this need to be completely removed, since I
  # personally find exceptions-as-control-flow extremely ugly and unfit
  # for any serious work. Technically it does the job, one might argue
  # this is a good compromized, but I'd much rather have a paser
  # template that (1) writes out error message with correction location
  # information and (2) if parser is in 'speculative' mode, aborts the
  # parsing completely (via early exit checks in the parser module).
  # Yes, more work, but it avoids rebouncing exceptions through two (or
  # even three) compiler subsystems (`Lexer?->Parser->Vm`)

  type TemporaryExceptionHack = ref object of CatchableError
    report: Report

  let oldHook = config.structuredReportHook

  config.structuredReportHook = proc(
      conf: ConfigRef, report: Report
  ): TErrorHandling =
    # @haxscramper: QUESTION This check might be affected by current severity
    # configurations, maybe it makes sense to do a hack-in that would
    # ignore all user-provided CLI otions?
    if report.category in {repParser, repLexer} and
        conf.severity(report) == rsevError:
      raise TemporaryExceptionHack(report: report)
    else:
      return oldHook(conf, report)

  try:
    let ast = parseString(code, cache, config, filename, line).toPNode()
    result.initSuccess(ast)
  except TemporaryExceptionHack as e:
    result.initFailure(move e.report)

  # restore the previous report hook:
  config.structuredReportHook = oldHook

proc parseCode*(code: string, cache: IdentCache, config: ConfigRef,
                info: TLineInfo): Result[PNode, Report] {.inline.} =
  ## Version of ``parseCode`` that accepts a ``TLineInfo``
  parseCode(code, cache, config, config.toFullPath(info), info.line.int)

proc errorReportToString*(c: ConfigRef, error: Report): string =
  ## Turns the given `error` report into text that is meant to be passed on to
  ## the user
  assert error.kind != repNone
  # @haxscramper: Not sure if there is any better solution - I /do/ want to
  # make sure that error reported to the user in the VM is the same as one
  # I would report on the CLI, but at the same time this still looks like an
  # overly hacky approach
  result = "Error: " & c.reportBody(error)
              # ^ `reportBody()` only returns main part of
              # the report, so need to add `"Error: "`
              # manally to stay consistent with the old
              # output.

proc toExceptionAst*(name, msg: sink string): PNode =
  ## Creates the AST as for an exception object as expected by the report.
  # TODO: the report should take the two strings directly instead
  let empty = newNode(nkEmpty)
  newTree(nkObjConstr,
          empty, # constructor type; unused
          empty, # unused
          newStrNode(nkStrLit, name),
          newStrNode(nkStrLit, msg))
