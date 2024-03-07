#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## abstract syntax tree + symbol table

import
  compiler/ast/[
    lineinfos,        # Positional information
    idents,           # Ast identifiers
    ast_types,        # Main ast type definitions
    ast_idgen,        # Per module Id generation
    ast_query,        # querying/reading the ast
    ast_parsed_types, # Data types for the parsed node
  ],
  compiler/front/[
    in_options
  ],
  compiler/utils/[
    idioms,
    int128 # Values for integer nodes
  ],
  std/[
    strutils,
    tables # For symbol table mapping
  ]

export ast_types, ast_idgen, ast_query, int128

var ggDebug* {.deprecated.}: bool ## convenience switch for trying out things

when defined(useNodeIds):
  const nodeIdToDebug* = NodeId -1 # 2322968

proc addAllowNil*(father, son: Indexable) {.inline.} =
  father.sons.add(son)

var gNodeId: int32
template setNodeId() =
  inc gNodeId
  result.id = NodeId gNodeId
  when defined(useNodeIds):
    if result.id == nodeIdToDebug:
      echo "KIND ", result.kind
      writeStackTrace()

proc newNodeAux(
    kind: TNodeKind, info: TLineInfo, typ: PType, children: int
  ): PNode {.inline.} =
  ## Base proc to create new nodes. This does the main work - the exported procs
  ## below act as the interfaces, also capturing intention for tracing/debugging
  result = PNode(kind: kind, info: info, typ: typ)
  
  {.cast(noSideEffect).}:
    setNodeId()
  
    if children > 0:
      newSeq(result.sons, children)
  
  when false:
    # this would add overhead, so we skip it; it results in a small amount of leaked entries
    # for old PNode that gets re-allocated at the same address as a PNode that
    # has `nfHasComment` set (and an entry in that table). Only `nfHasComment`
    # should be used to test whether a PNode has a comment; gconfig.comments
    # can contain extra entries for deleted PNode's with comments.
    gconfig.comments.del(result.id)

proc newNodeIT*(kind: TNodeKind, info: TLineInfo, typ: PType, children: int): PNode =
  ## new node with line info, type, and children
  newNodeAux(kind, info, typ, children)

proc newNodeIT*(kind: TNodeKind, info: TLineInfo, typ: PType): PNode =
  ## new node with line info, type, and no children
  result = newNodeAux(kind, info, typ, 0)

proc newNodeI*(kind: TNodeKind, info: TLineInfo, children: int): PNode =
  ## new node with line info, no type, and children
  result = newNodeAux(kind, info, nil, children)

func newNodeI*(kind: TNodeKind, info: TLineInfo): PNode {.inline.} =
  ## new node with line info, no type, and no children
  result = newNodeAux(kind, info, nil, 0)

proc newNode*(kind: TNodeKind): PNode {.inline.} =
  ## new node with unknown line info, no type, and no children
  result = newNodeAux(kind, unknownLineInfo, nil, 0)

proc newTreeAux(
    kind: TNodeKind; info: TLineInfo; typ: PType; children: varargs[PNode]
  ): PNode {.inline.} =
  ## Base proc to create a new tree. This does the main work - the exported
  ## procedures below act as the interfaces, also capturing intention for
  ## tracing/debugging
  result = newNodeAux(kind, info, typ, 0)
  result.sons = @children

proc newTree*(kind: TNodeKind; children: varargs[PNode]): PNode =
  let info =
    if children.len > 0:
      children[0].info
    else:
      unknownLineInfo
  
  result = newTreeAux(kind, info, nil, children)

proc newTreeI*(kind: TNodeKind; info: TLineInfo; children: varargs[PNode]): PNode =
  result = newTreeAux(kind, info, nil, children)

proc newTreeIT*(
    kind: TNodeKind; info: TLineInfo; typ: PType; children: varargs[PNode]
  ): PNode =
  result = newTreeAux(kind, info, typ, children)

when false:
  import tables, strutils
  var x: CountTable[string]

  addQuitProc proc () {.noconv.} =
    for k, v in pairs(x):
      echo k
      echo v

proc newSym*(symKind: TSymKind, name: PIdent, id: ItemId, owner: PSym,
             info: TLineInfo, typ: PType; options: TOptions = {}): PSym =
  # generates a symbol and initializes the hash field too
  result = PSym(name: name, kind: symKind, flags: {}, info: info, itemId: id,
                typ: typ, options: options, owner: owner, offset: defaultOffset)
  when false:
    if id.module == 48 and id.item == 39:
      writeStackTrace()
      echo "kind ", symKind, " ", name.s
      if owner != nil: echo owner.name.s

proc newSym*(symKind: TSymKind, name: PIdent, id: ItemId, owner: PSym,
             info: TLineInfo; options: TOptions = {}): PSym {.inline.} =
  # generates a symbol and initializes the hash field too
  result = newSym(symKind, name, id, owner, info, typ = nil, options)

proc linkTo*(t: PType, s: PSym): PType {.discardable.} =
  t.sym = s
  s.typ = t
  result = t

proc linkTo*(s: PSym, t: PType): PSym {.discardable.} =
  t.sym = s
  s.typ = t
  result = s

proc appendToModule*(m: PSym, n: PNode) =
  ## The compiler will use this internally to add nodes that will be
  ## appended to the module after the sem pass
  if m.ast == nil:
    m.ast = newNode(nkStmtList)
    m.ast.sons = @[n]
  else:
    assert m.ast.kind == nkStmtList
    m.ast.sons.add(n)

const                         # for all kind of hash tables:
  GrowthFactor* = 2           # must be power of 2, > 0
  StartSize* = 8              # must be power of 2, > 0

proc copyStrTable*(dest: var TStrTable, src: TStrTable) =
  dest.counter = src.counter
  setLen(dest.data, src.data.len)
  for i in 0..high(src.data): dest.data[i] = src.data[i]

proc copyIdTable*(dest: var TIdTable, src: TIdTable) =
  dest.counter = src.counter
  newSeq(dest.data, src.data.len)
  for i in 0..high(src.data): dest.data[i] = src.data[i]

proc copyObjectSet*(dest: var TObjectSet, src: TObjectSet) =
  dest.counter = src.counter
  setLen(dest.data, src.data.len)
  for i in 0..high(src.data): dest.data[i] = src.data[i]

proc discardSons*(father: PNode) =
  father.sons = @[]

proc withInfo*(n: PNode, info: TLineInfo): PNode =
  ## set the line information (`info`) on the node `n`
  n.info = info
  return n

proc newIdentNode*(ident: PIdent, info: TLineInfo): PNode =
  result = newNode(nkIdent)
  result.ident = ident
  result.info = info

proc newSymNode2*(sym: PSym): PNode =
  ## creates a new `nkSym` node, unless sym.kind is an skError where an nkError
  ## is extracted from the sym and returned instead.
  ## NB: not a `newSymNode` replacement, it's for when symbol sem fails
  if sym.isError:
    result = sym.ast
  else:
    result = newNode(nkSym)
    result.sym = sym
    result.typ = sym.typ
    result.info = sym.info

proc newSymNode2*(sym: PSym, info: TLineInfo): PNode =
  ## creates a new `nkSym` node, unless sym.kind is an skError where an nkError
  ## is extracted from the sym and returned instead. In either case sets the
  ## node info to the one provided
  ## NB: not a `newSymNode` replacement, it's for when symbol sem fails
  if sym.isError:
    result = sym.ast
    result.info = info
  else:
    result = newNode(nkSym)
    result.sym = sym
    result.typ = sym.typ
    result.info = info

proc newSymNodeIT*(sym: PSym, info: TLineInfo, typ: PType): PNode =
  ## create a new sym node with the supplied `info` and `typ`
  result = newNodeIT(nkSym, info, typ)
  result.sym = sym

proc newSymNode*(sym: PSym, info: TLineInfo): PNode {.inline.} =
  ## create a new sym node from `sym` with its type and supplied `info`
  result = newSymNodeIT(sym, info, sym.typ)

proc newSymNode*(sym: PSym): PNode {.inline.} =
  ## create a new sym node from `sym` with its info and type
  result = newSymNode(sym, sym.info)

proc newIntNode*(kind: TNodeKind, intVal: BiggestInt): PNode =
  result = newNode(kind)
  result.intVal = intVal

proc newIntNode*(kind: TNodeKind, intVal: Int128): PNode =
  result = newNode(kind)
  result.intVal = castToInt64(intVal)

proc newIntTypeNode*(intVal: BiggestInt, typ: PType): PNode =
  let kind = skipTypes(typ, abstractVarRange).kind
  case kind
  of tyInt:     result = newNode(nkIntLit)
  of tyInt8:    result = newNode(nkInt8Lit)
  of tyInt16:   result = newNode(nkInt16Lit)
  of tyInt32:   result = newNode(nkInt32Lit)
  of tyInt64:   result = newNode(nkInt64Lit)
  of tyChar:    result = newNode(nkCharLit)
  of tyUInt:    result = newNode(nkUIntLit)
  of tyUInt8:   result = newNode(nkUInt8Lit)
  of tyUInt16:  result = newNode(nkUInt16Lit)
  of tyUInt32:  result = newNode(nkUInt32Lit)
  of tyUInt64:  result = newNode(nkUInt64Lit)
  of tyEnum:
    # XXX: the kind for the underlying type should be used here, but too much
    #      code still relies on literal enum values being represented as
    #      exactly ``nkIntLit``
    result = newNode(nkIntLit)
  of tyBool:
    # XXX: does this really need to be the kind nkIntLit?
    result = newNode(nkIntLit)
  of tyStatic: # that's a pre-existing bug, will fix in another PR
    result = newNode(nkIntLit)
  else: doAssert false, $kind
  result.intVal = intVal
  result.typ = typ

proc newIntTypeNode*(intVal: Int128, typ: PType): PNode =
  # XXX: introduce range check
  newIntTypeNode(castToInt64(intVal), typ)

proc newFloatNode*(kind: TNodeKind, floatVal: BiggestFloat): PNode =
  result = newNode(kind)
  result.floatVal = floatVal

proc newStrNode*(kind: TNodeKind, strVal: string): PNode =
  result = newNode(kind)
  result.strVal = strVal

proc newStrNode*(strVal: string; info: TLineInfo): PNode =
  result = newNodeI(nkStrLit, info)
  result.strVal = strVal

proc newStrNode*(strVal: sink string, typ: PType; info=unknownLineInfo): PNode =
  result = newNodeIT(nkStrLit, info, typ)
  result.strVal = strVal

proc newProcNode*(kind: TNodeKind, info: TLineInfo, body: PNode,
                 params,
                 name, pattern, genericParams,
                 pragmas, exceptions: PNode): PNode =
  result = newNodeI(kind, info)
  result.sons = @[name, pattern, genericParams, params,
                  pragmas, exceptions, body]

proc newTypeError*(prev: PType,
                   id: ItemId, 
                   owner: PSym = if prev.isNil: nil else: prev.owner,
                   err: PNode): PType =
  ## create a new error type, with an optional `prev`ious type (can be nil) and
  ## `err`or node for the error msg
  result = PType(kind: tyError, owner: owner, size: defaultSize,
                 align: defaultAlignment, itemId: id,
                 lockLevel: UnspecifiedLockLevel, uniqueId: id, n: err)
  result.typeInst = prev

proc newType*(kind: TTypeKind, id: ItemId; owner: PSym): PType =
  result = PType(kind: kind, owner: owner, size: defaultSize,
                 align: defaultAlignment, itemId: id,
                 lockLevel: UnspecifiedLockLevel,
                 uniqueId: id)
  when false:
    if result.itemId.module == 55 and result.itemId.item == 2:
      echo "KNID ", kind
      writeStackTrace()

proc newSons*(father: Indexable, length: int) =
  setLen(father.sons, length)

proc assignType*(dest, src: PType) =
  dest.kind = src.kind
  dest.flags = src.flags
  dest.callConv = src.callConv
  dest.n = src.n
  dest.size = src.size
  dest.align = src.align
  dest.lockLevel = src.lockLevel
  # this fixes 'type TLock = TSysLock':
  if src.sym != nil:
    if dest.sym != nil:
      dest.sym.flags.incl src.sym.flags-{sfExported}
      if dest.sym.annex.isNil: dest.sym.annex = src.sym.annex
      dest.sym.extFlags.incl src.sym.extFlags
      if dest.sym.extname.len == 0:
        dest.sym.extname = src.sym.extname
    else:
      dest.sym = src.sym
  newSons(dest, src.len)
  for i in 0..<src.len: dest[i] = src[i]

proc copyType*(t: PType, id: ItemId, owner: PSym): PType =
  result = newType(t.kind, id, owner)
  assignType(result, t)
  result.sym = t.sym          # backend-info should not be copied

proc exactReplica*(t: PType): PType =
  result = copyType(t, t.itemId, t.owner)

proc copySym*(s: PSym; id: ItemId): PSym =
  result = newSym(s.kind, s.name, id, s.owner, s.info, s.options)
  #result.ast = nil            # BUGFIX; was: s.ast which made problems
  result.typ = s.typ
  result.flags = s.flags
  result.magic = s.magic
  result.options = s.options
  result.position = s.position
  result.extname = s.extname
  result.extFlags = s.extFlags
  result.annex = s.annex      # BUGFIX
  result.constraint = s.constraint
  if result.kind in {skVar, skLet, skField}:
    result.guard = s.guard
    result.bitsize = s.bitsize
    result.alignment = s.alignment

proc createModuleAlias*(s: PSym, id: ItemId, newIdent: PIdent, info: TLineInfo;
                        options: TOptions): PSym =
  result = newSym(s.kind, newIdent, id, s.owner, info, options)
  # keep ID!
  result.ast = s.ast
  #result.id = s.id # XXX figure out what to do with the ID.
  result.flags = s.flags
  result.options = s.options
  result.position = s.position
  result.extname = s.extname
  result.extFlags = s.extFlags
  result.annex = s.annex

proc initStrTable*(x: var TStrTable) =
  x.counter = 0
  newSeq(x.data, StartSize)

proc newStrTable*: TStrTable =
  initStrTable(result)

proc initIdTable*(x: var TIdTable) =
  x.counter = 0
  newSeq(x.data, StartSize)

proc newIdTable*: TIdTable =
  initIdTable(result)

proc resetIdTable*(x: var TIdTable) =
  x.counter = 0
  # clear and set to old initial size:
  setLen(x.data, 0)
  setLen(x.data, StartSize)

proc initObjectSet*(x: var TObjectSet) =
  x.counter = 0
  newSeq(x.data, StartSize)

proc initIdNodeTable*(x: var TIdNodeTable) =
  x.counter = 0
  newSeq(x.data, StartSize)

proc newIdNodeTable*: TIdNodeTable =
  initIdNodeTable(result)

proc initNodeTable*(x: var TNodeTable) =
  x.counter = 0
  newSeq(x.data, StartSize)

proc isGCedMem*(t: PType): bool {.inline.} =
  result = t.kind in {tyString, tyRef, tySequence} or
           t.kind == tyProc and t.callConv == ccClosure

proc propagateToOwner*(owner, elem: PType; propagateHasAsgn = true) =
  owner.flags.incl elem.flags * {tfHasMeta, tfTriggersCompileTime}
  if tfNotNil in elem.flags:
    if owner.kind in {tyGenericInst, tyGenericBody, tyGenericInvocation}:
      owner.flags.incl tfNotNil

  if elem.isMetaType:
    owner.flags.incl tfHasMeta

  let mask = elem.flags * {tfHasAsgn}
  if mask != {} and propagateHasAsgn:
    let o2 = owner.skipTypes({tyGenericInst, tyAlias, tySink})
    if o2.kind in {tyTuple, tyObject, tyArray,
                   tySequence, tySet, tyDistinct}:
      o2.flags.incl mask
      owner.flags.incl mask

  if owner.kind notin {tyProc, tyGenericInst, tyGenericBody,
                       tyGenericInvocation, tyPtr}:
    let elemB = elem.skipTypes({tyGenericInst, tyAlias, tySink})
    if elemB.isGCedMem or tfHasGCedMem in elemB.flags:
      # for simplicity, we propagate this flag even to generics. We then
      # ensure this doesn't bite us in sempass2.
      owner.flags.incl tfHasGCedMem

proc rawAddSon*(father, son: PType; propagateHasAsgn = true) =
  father.sons.add(son)
  if son != nil: propagateToOwner(father, son, propagateHasAsgn)

proc rawAddSonNoPropagationOfTypeFlags*(father, son: PType) =
  father.sons.add(son)

proc addSonNilAllowed*(father, son: PNode) =
  father.sons.add(son)

proc delSon*(father: PNode, idx: int) =
  if father.len == 0:
    return
  for i in idx..<father.len - 1:
    father[i] = father[i + 1]
  father.sons.setLen(father.len - 1)

template copyNodeImpl(dst, src, processSonsStmt) =
  if src == nil: return
  dst = newNode(src.kind)
  dst.info = src.info
  dst.typ = src.typ
  dst.flags = src.flags * PersistentNodeFlags
  dst.comment = src.comment
  when defined(useNodeIds):
    if dst.id == nodeIdToDebug:
      echo "COMES FROM ", src.id
    elif src.id == nodeIdToDebug:
      echo "GOES TO ", dst.id
      writeStackTrace()
  case src.kind
  of nkIntLiterals:
    dst.intVal = src.intVal
    dst.intLitBase = src.intLitBase
  of nkFloatLiterals:
    dst.floatVal = src.floatVal
    dst.floatLitBase = src.floatLitBase
  of nkSym: dst.sym = src.sym
  of nkIdent: dst.ident = src.ident
  of nkStrLiterals: dst.strVal = src.strVal
  of nkEmpty, nkNilLit, nkType, nkCommentStmt: discard "no children"
  of nkError: dst.diag = src.diag # do cheap copies
  of nkWithSons: processSonsStmt

proc copyNode*(src: PNode): PNode =
  # does not copy its sons!
  copyNodeImpl(result, src):
    discard

proc copyNodeWithKids*(src: PNode): PNode =
  ## Creates a shallow copy of `src`, meaning that a copy of `src` is
  ## created without deep-copying the tree.
  copyNodeImpl(result, src):
    result.sons = src.sons

template transitionNodeKindCommon(k: TNodeKind) =
  let obj {.inject.} = n[]
  when defined(useNodeIds):
    if obj.id == nodeIdToDebug:
      echo "TRANSITIONED TO ", k
      writeStackTrace()
  n[] = TNode(id: obj.id, kind: k, typ: obj.typ, info: obj.info,
              flags: obj.flags)
  # n.comment = obj.comment # shouldn't be needed, the address doesnt' change

proc transitionSonsKind*(n: PNode, kind: range[nkDotCall..nkTupleConstr]) =
  transitionNodeKindCommon(kind)
  n.sons = obj.sons

proc transitionIntKind*(n: PNode, kind: range[nkCharLit..nkUInt64Lit]) =
  transitionNodeKindCommon(kind)
  n.intVal = obj.intVal

proc transitionNoneToSym*(n: PNode) =
  transitionNodeKindCommon(nkSym)

template transitionSymKindCommon*(k: TSymKind) =
  let obj {.inject.} = s[]
  s[] = TSym(kind: k, itemId: obj.itemId, magic: obj.magic, typ: obj.typ, name: obj.name,
             info: obj.info, owner: obj.owner, flags: obj.flags, ast: obj.ast,
             options: obj.options, position: obj.position, offset: obj.offset,
             extname: obj.extname, extFlags: obj.extFlags, locId: obj.locId,
             annex: obj.annex, constraint: obj.constraint)
  when defined(nimsuggest):
    s.allUsages = obj.allUsages

proc transitionGenericParamToType*(s: PSym) =
  transitionSymKindCommon(skType)

proc transitionRoutineSymKind*(s: PSym, kind: range[skProc..skTemplate]) =
  transitionSymKindCommon(kind)
  if obj.kind in routineKinds - {skMacro} and s.kind != skMacro:
    s.gcUnsafetyReason = obj.gcUnsafetyReason

proc transitionToLet*(s: PSym) =
  transitionSymKindCommon(skLet)
  s.guard = obj.guard
  s.bitsize = obj.bitsize
  s.alignment = obj.alignment

proc transitionToError*(s: PSym, err: PNode) =
  ## transition `s`ymbol to an `skError` with `err`or node
  assert err.kind == nkError
  transitionSymKindCommon(skError)
  s.ast = err

proc shallowCopy*(src: PNode): PNode =
  # does not copy its sons, but provides space for them:
  copyNodeImpl(result, src):
    newSeq(result.sons, src.len)

proc copyTree*(src: PNode): PNode =
  # copy a whole syntax tree; performs deep copying
  copyNodeImpl(result, src):
    newSeq(result.sons, src.len)
    for i in 0..<src.len:
      result[i] = copyTree(src[i])

proc copyTreeWithoutNode*(src, skippedNode: PNode): PNode =
  copyNodeImpl(result, src):
    result.sons = newSeqOfCap[PNode](src.len)
    for n in src.sons:
      if n != skippedNode:
        result.sons.add copyTreeWithoutNode(n, skippedNode)

proc copyTreeWithoutNodes*(src: PNode; skippedNodes: varargs[PNode]): PNode =
  copyNodeImpl(result, src):
    result.sons = newSeqOfCap[PNode](src.len)
    for n in src.sons:
      if n notin skippedNodes:
        result.sons.add copyTreeWithoutNodes(n, skippedNodes)

proc makeStmtList*(n: PNode): PNode =
  if n.kind == nkStmtList:
    result = n
  else:
    result = newNodeI(nkStmtList, n.info)
    result.add n

proc toVar*(typ: PType; kind: TTypeKind; idgen: IdGenerator): PType =
  ## If ``typ`` is not a tyVar then it is converted into a `var <typ>` and
  ## returned. Otherwise ``typ`` is simply returned as-is.
  result = typ
  if typ.kind != kind:
    result = newType(kind, nextTypeId(idgen), typ.owner)
    rawAddSon(result, typ)

proc toObject*(typ: PType): PType =
  ## If ``typ`` is a tyRef then its immediate son is returned (which in many
  ## cases should be a ``tyObject``).
  ## Otherwise ``typ`` is simply returned as-is.
  let t = typ.skipTypes({tyAlias, tyGenericInst})
  if t.kind == tyRef: t.lastSon
  else: typ

proc toObjectFromRefPtrGeneric*(typ: PType): PType =
  #[
  See also `toObject`.
  Finds the underlying `object`, even in cases like these:
  type
    B[T] = object f0: int
    A1[T] = ref B[T]
    A2[T] = ref object f1: int
    A3 = ref object f2: int
    A4 = object f3: int
  ]#
  result = typ
  while true:
    case result.kind
    of tyGenericInvocation:
      result = result[0]
    of tyRef, tyPtr, tyGenericInst, tyAlias, tyGenericBody:
      result = result[^1]
    else: break
  assert result.sym != nil

proc newProcType*(info: TLineInfo; id: ItemId; owner: PSym): PType =
  result = newType(tyProc, id, owner)
  result.n = newNodeI(nkFormalParams, info)
  rawAddSon(result, nil) # return type
  # result.n[0] used to be `nkType`, but now it's `nkEffectList` because
  # the effects are now stored in there too ... this is a bit hacky, but as
  # usual we desperately try to save memory:
  result.n.add newNodeI(nkEffectList, info)

proc addParam*(procType: PType; param: PSym) =
  param.position = procType.len-1
  procType.n.add newSymNode(param)
  rawAddSon(procType, param.typ)

proc toHumanStrImpl[T](kind: T, num: static int): string =
  result = $kind
  result = result[num..^1]
  result[0] = result[0].toLowerAscii

proc toHumanStr*(kind: TSymKind): string =
  ## strips leading `sk`
  result = toHumanStrImpl(kind, 2)

proc toHumanStr*(kind: TTypeKind): string =
  ## strips leading `tk`
  result = toHumanStrImpl(kind, 2)

func toTNodeKind*(kind: ParsedNodeKind): TNodeKind {.inline.} =
  case kind
  of pnkError: nkError
  of pnkEmpty: nkEmpty
  of pnkIdent: nkIdent
  of pnkCharLit: nkCharLit
  of pnkIntLit: nkIntLit
  of pnkInt8Lit: nkInt8Lit
  of pnkInt16Lit: nkInt16Lit
  of pnkInt32Lit: nkInt32Lit
  of pnkInt64Lit: nkInt64Lit
  of pnkUIntLit: nkUIntLit
  of pnkUInt8Lit: nkUInt8Lit
  of pnkUInt16Lit: nkUInt16Lit
  of pnkUInt32Lit: nkUInt32Lit
  of pnkUInt64Lit: nkUInt64Lit
  of pnkFloatLit: nkFloatLit
  of pnkFloat32Lit: nkFloat32Lit
  of pnkFloat64Lit: nkFloat64Lit
  of pnkStrLit: nkStrLit
  of pnkRStrLit: nkRStrLit
  of pnkTripleStrLit: nkTripleStrLit
  of pnkNilLit: nkNilLit
  of pnkCustomLit: nkDotExpr
  of pnkCall: nkCall
  of pnkCommand: nkCommand
  of pnkCallStrLit: nkCallStrLit
  of pnkInfix: nkInfix
  of pnkPrefix: nkPrefix
  of pnkPostfix: nkPostfix
  of pnkExprEqExpr: nkExprEqExpr
  of pnkExprColonExpr: nkExprColonExpr
  of pnkIdentDefs: nkIdentDefs
  of pnkConstDef: nkConstDef
  of pnkVarTuple: nkVarTuple
  of pnkPar: nkPar
  of pnkSqrBracket: nkBracket
  of pnkCurly: nkCurly
  of pnkTupleConstr: nkTupleConstr
  of pnkObjConstr: nkObjConstr
  of pnkTableConstr: nkTableConstr
  of pnkSqrBracketExpr: nkBracketExpr
  of pnkCurlyExpr: nkCurlyExpr
  of pnkPragmaExpr: nkPragmaExpr
  of pnkPragma: nkPragma
  of pnkPragmaBlock: nkPragmaBlock
  of pnkDotExpr: nkDotExpr
  of pnkAccQuoted: nkAccQuoted
  of pnkIfExpr: nkIfExpr
  of pnkIfStmt: nkIfStmt
  of pnkElifBranch: nkElifBranch
  of pnkElifExpr: nkElifExpr
  of pnkElse: nkElse
  of pnkElseExpr: nkElseExpr
  of pnkCaseStmt: nkCaseStmt
  of pnkOfBranch: nkOfBranch
  of pnkWhenStmt, pnkWhenExpr: nkWhenStmt
  of pnkForStmt: nkForStmt
  of pnkWhileStmt: nkWhileStmt
  of pnkBlockExpr: nkBlockExpr
  of pnkBlockStmt: nkBlockStmt
  of pnkDiscardStmt: nkDiscardStmt
  of pnkContinueStmt: nkContinueStmt
  of pnkBreakStmt: nkBreakStmt
  of pnkReturnStmt: nkReturnStmt
  of pnkRaiseStmt: nkRaiseStmt
  of pnkYieldStmt: nkYieldStmt
  of pnkTryStmt: nkTryStmt
  of pnkExceptBranch: nkExceptBranch
  of pnkFinally: nkFinally
  of pnkDefer: nkDefer
  of pnkLambda: nkLambda
  of pnkDo: nkDo
  of pnkBind: nkBind
  of pnkBindStmt: nkBindStmt
  of pnkMixinStmt: nkMixinStmt
  of pnkCast: nkCast
  of pnkStaticStmt: nkStaticStmt
  of pnkAsgn: nkAsgn
  of pnkGenericParams: nkGenericParams
  of pnkFormalParams: nkFormalParams
  of pnkStmtList: nkStmtList
  of pnkStmtListExpr: nkStmtListExpr
  of pnkImportStmt: nkImportStmt
  of pnkImportExceptStmt: nkImportExceptStmt
  of pnkFromStmt: nkFromStmt
  of pnkIncludeStmt: nkIncludeStmt
  of pnkExportStmt: nkExportStmt
  of pnkExportExceptStmt: nkExportExceptStmt
  of pnkConstSection: nkConstSection
  of pnkLetSection: nkLetSection
  of pnkVarSection: nkVarSection
  of pnkProcDef: nkProcDef
  of pnkFuncDef: nkFuncDef
  of pnkMethodDef: nkMethodDef
  of pnkConverterDef: nkConverterDef
  of pnkIteratorDef: nkIteratorDef
  of pnkMacroDef: nkMacroDef
  of pnkTemplateDef: nkTemplateDef
  of pnkTypeSection: nkTypeSection
  of pnkTypeDef: nkTypeDef
  of pnkEnumTy: nkEnumTy
  of pnkEnumFieldDef: nkEnumFieldDef
  of pnkObjectTy: nkObjectTy
  of pnkTupleTy: nkTupleTy
  of pnkProcTy: nkProcTy
  of pnkIteratorTy: nkIteratorTy
  of pnkRecList: nkRecList
  of pnkRecCase: nkRecCase
  of pnkRecWhen: nkRecWhen
  of pnkTypeOfExpr: nkTypeOfExpr
  of pnkRefTy: nkRefTy
  of pnkVarTy: nkVarTy
  of pnkPtrTy: nkPtrTy
  of pnkStaticTy: nkStaticTy
  of pnkDistinctTy: nkDistinctTy
  of pnkMutableTy: nkMutableTy
  of pnkTupleClassTy: nkTupleClassTy
  of pnkTypeClassTy: nkTypeClassTy
  of pnkOfInherit: nkOfInherit
  of pnkArgList: nkArgList
  of pnkWith: nkWith
  of pnkWithout: nkWithout
  of pnkAsmStmt: nkAsmStmt
  of pnkCommentStmt: nkCommentStmt
  of pnkUsingStmt: nkUsingStmt

proc splitCustomLit*(n: ParsedNode): tuple[num, ident: ParsedNode] {.inline.} =
  assert n != nil
  assert n.kind == pnkCustomLit
  var strLitTok = n.lit
  strLitTok.literal = n.lit.literal.substr(0, n.lit.iNumber.int - 1)
  var suffixTok = n.lit
  suffixTok.literal = n.lit.ident.s
  result =
    (ParsedNode(kind: pnkRStrLit, fileIndex: n.fileIndex, lit: strLitTok),
     ParsedNode(kind: pnkIdent, fileIndex: n.fileIndex, startToken: suffixTok))

proc toPNode*(parsed: ParsedNode): PNode =
  if parsed.isNil: return
  result =
    case parsed.kind
    of pnkCustomLit:
      # xxx: between reworking the `lexer` and `PNode`, this awkward handling
      #      should go away
      let split = splitCustomLit(parsed)
      newTreeI(nkDotExpr, parsed.info): # TODO: confirm info is correct
              [toPNode(split.num), toPNode(split.ident)]
    else:
      newNodeI(parsed.kind.toTNodeKind, parsed.info)

  result.comment = parsed.comment

  case parsed.kind
  of pnkEmpty:
    discard

  of pnkFloatKinds:
    result.floatVal = parsed.lit.fNumber
    result.floatLitBase = parsed.lit.base

  of pnkIntKinds - { pnkCharLit }:
    result.intVal = parsed.lit.iNumber
    result.intLitBase = parsed.lit.base

  of pnkCharLit:
    result.intVal = ord(parsed.lit.literal[0])

  of pnkStrKinds:
    result.strVal = parsed.lit.literal

  of pnkNilLit:
    discard

  of pnkIdent:
    assert parsed.startToken.ident != nil,
          "tkIn, tkOut, etc need to be translated, probably in the parser for now"
    result.ident = parsed.startToken.ident

  of pnkAccQuoted:
    for (ident, line, col) in parsed.idents.items:
      let info = TLineInfo(fileIndex: parsed.fileIndex, line: line, col: col)
      result.add newIdentNode(ident, info)

  of pnkCustomLit:
    discard "handled earlier"

  of pnkError:
    unreachable("IMPLEMENT ME")

  else:
    for sub in parsed.sons.items:
      result.add toPNode(sub)
