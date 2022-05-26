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
    lineinfos, # Positional information
    idents, # Ast identifiers
    ast_types # Main ast type definitions
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    ropes,
    int128 # Values for integer nodes
  ],
  std/[
    hashes,
    strutils,
    tables # For symbol table mapping
  ]

export ast_types, int128

type Gconfig = object
  ## we put comments in a side channel to avoid increasing `sizeof(TNode)`,
  ## which reduces memory usage given that `PNode` is the most allocated
  ## type by far.
  comments: Table[NodeId, string] # nodeId => comment
  useIc*: bool

var gconfig {.threadvar.}: Gconfig

proc setUseIc*(useIc: bool) = gconfig.useIc = useIc

proc comment*(n: PNode): string =
  if nfHasComment in n.flags and not gconfig.useIc:
    # IC doesn't track comments, see `packed_ast`, so this could fail
    result = gconfig.comments[n.id]

proc `comment=`*(n: PNode, a: string) =
  if a.len > 0:
    # if needed, we could periodically cleanup gconfig.comments when its size increases,
    # to ensure only live nodes (and with nfHasComment) have an entry in gconfig.comments;
    # for compiling compiler, the waste is very small:
    # num calls to newNodeImpl: 14984160 (num of PNode allocations)
    # size of gconfig.comments: 33585
    # num of nodes with comments that were deleted and hence wasted: 3081
    n.flags.incl nfHasComment
    gconfig.comments[n.id] = a
  elif nfHasComment in n.flags:
    n.flags.excl nfHasComment
    gconfig.comments.del(n.id)

# BUGFIX: a module is overloadable so that a proc can have the
# same name as an imported module. This is necessary because of
# the poor naming choices in the standard library.

const
  OverloadableSyms* = {skProc, skFunc, skMethod, skIterator,
    skConverter, skModule, skTemplate, skMacro, skEnumField}

  skipForDiscardable* = {nkIfStmt, nkIfExpr, nkCaseStmt, nkOfBranch,
    nkElse, nkStmtListExpr, nkTryStmt, nkFinally, nkExceptBranch,
    nkElifBranch, nkElifExpr, nkElseExpr, nkBlockStmt, nkBlockExpr,
    nkHiddenStdConv, nkHiddenDeref}


  GenericTypes*: TTypeKinds = {tyGenericInvocation, tyGenericBody,
    tyGenericParam}

  StructuralEquivTypes*: TTypeKinds = {tyNil, tyTuple, tyArray,
    tySet, tyRange, tyPtr, tyRef, tyVar, tyLent, tySequence, tyProc, tyOpenArray,
    tyVarargs}

  ConcreteTypes*: TTypeKinds = { # types of the expr that may occur in::
                                 # var x = expr
    tyBool, tyChar, tyEnum, tyArray, tyObject,
    tySet, tyTuple, tyRange, tyPtr, tyRef, tyVar, tyLent, tySequence, tyProc,
    tyPointer,
    tyOpenArray, tyString, tyCstring, tyInt..tyInt64, tyFloat..tyFloat128,
    tyUInt..tyUInt64}
  IntegralTypes* = {tyBool, tyChar, tyEnum, tyInt..tyInt64,
    tyFloat..tyFloat128, tyUInt..tyUInt64} # weird name because it contains tyFloat
  ConstantDataTypes*: TTypeKinds = {tyArray, tySet,
                                    tyTuple, tySequence}
  NilableTypes*: TTypeKinds = {tyPointer, tyCstring, tyRef, tyPtr,
    tyProc, tyError} # TODO
  PtrLikeKinds*: TTypeKinds = {tyPointer, tyPtr} # for VM
  PersistentNodeFlags*: TNodeFlags = {nfBase2, nfBase8, nfBase16,
                                      nfDotSetter, nfDotField,
                                      nfIsRef, nfIsPtr, nfPreventCg, nfLL,
                                      nfFromTemplate, nfDefaultRefsParam,
                                      nfExecuteOnReload, nfLastRead, nfFirstWrite}
  namePos*          = 0 ## Name of the type/proc-like node
  patternPos*       = 1 ## empty except for term rewriting macros
  genericParamsPos* = 2 ## Generic parametesr in the procedure-like nodes
  paramsPos*        = 3 ## Formal parameters in the procedure-like nodes
  pragmasPos*       = 4 ## Position of the pragma in the procedure-like nodes
  miscPos*          = 5 ## used for undocumented and hacky stuff
  bodyPos*          = 6 ## position of body; use rodread.getBody() instead!
  resultPos*        = 7
  dispatcherPos*    = 8

  wrongNodePos*     = 0 ## Error the ast node we swapped
  errorKindPos*     = 1 ## Error kind enum as an intlit
  compilerInfoPos*  = 2 ## Error compiler source file as strlit, line & col
                        ## on info
  firstArgPos*      = 3 ## Error first 0..n additional nodes depends on
                        ## error kind


  nfAllFieldsSet* = nfBase2

  nkCallKinds* = {nkCall, nkInfix, nkPrefix, nkPostfix,
                  nkCommand, nkCallStrLit, nkHiddenCallConv}
  nkIdentKinds* = {nkIdent, nkSym, nkAccQuoted, nkOpenSymChoice,
                   nkClosedSymChoice}

  nkPragmaCallKinds* = {nkExprColonExpr, nkCall, nkCallStrLit}
  nkLiterals* = {nkCharLit..nkTripleStrLit}
  nkFloatLiterals* = {nkFloatLit..nkFloat128Lit}
  nkLambdaKinds* = {nkLambda, nkDo}
  declarativeDefs* = {nkProcDef, nkFuncDef, nkMethodDef, nkIteratorDef, nkConverterDef}
  routineDefs* = declarativeDefs + {nkMacroDef, nkTemplateDef}
  procDefs* = nkLambdaKinds + declarativeDefs
  callableDefs* = nkLambdaKinds + routineDefs

  nkSymChoices* = {nkClosedSymChoice, nkOpenSymChoice}
  nkStrKinds* = {nkStrLit..nkTripleStrLit}
  nkIntKinds* = {nkCharLit .. nkUInt64Lit}

  skLocalVars* = {skVar, skLet, skForVar, skParam, skResult}
  skProcKinds* = {skProc, skFunc, skTemplate, skMacro, skIterator,
                  skMethod, skConverter}

  defaultSize = -1
  defaultAlignment = -1
  defaultOffset* = -1

  nodeKindsProducedByParse* = {
    nkError, nkEmpty,
    nkIdent,

    nkCharLit,
    nkIntLit, nkInt8Lit, nkInt16Lit, nkInt32Lit, nkInt64Lit,
    nkUIntLit, nkUInt8Lit, nkUInt16Lit, nkUInt32Lit, nkUInt64Lit,
    nkFloatLit, nkFloat32Lit, nkFloat64Lit, nkFloat128Lit,
    nkStrLit, nkRStrLit, nkTripleStrLit,
    nkNilLit,

    nkCall, nkCommand, nkCallStrLit, nkInfix, nkPrefix, nkPostfix,

    nkExprEqExpr, nkExprColonExpr, nkIdentDefs, nkConstDef, nkVarTuple, nkPar,
    nkBracket, nkCurly, nkTupleConstr, nkObjConstr, nkTableConstr,
    nkBracketExpr, nkCurlyExpr,

    nkPragmaExpr, nkPragma, nkPragmaBlock,

    nkDotExpr, nkAccQuoted,

    nkIfExpr, nkIfStmt, nkElifBranch, nkElifExpr, nkElse, nkElseExpr,
    nkCaseStmt, nkOfBranch,
    nkWhenStmt,

    nkForStmt, nkWhileStmt,

    nkBlockExpr, nkBlockStmt,

    nkDiscardStmt, nkContinueStmt, nkBreakStmt, nkReturnStmt, nkRaiseStmt,
    nkYieldStmt,

    nkTryStmt, nkExceptBranch, nkFinally,

    nkDefer,

    nkLambda, nkDo,

    nkBind, nkBindStmt, nkMixinStmt,

    nkCast,
    nkStaticStmt,

    nkAsgn,

    nkGenericParams,
    nkFormalParams,

    nkStmtList, nkStmtListExpr,

    nkImportStmt, nkImportExceptStmt, nkImportAs, nkFromStmt,

    nkIncludeStmt,

    nkExportStmt, nkExportExceptStmt,

    nkConstSection, nkLetSection, nkVarSection,

    nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef, nkIteratorDef,
    nkMacroDef, nkTemplateDef,

    nkTypeSection, nkTypeDef,

    nkEnumTy, nkEnumFieldDef,

    nkObjectTy, nkTupleTy, nkProcTy, nkIteratorTy,

    nkRecList, nkRecCase, nkRecWhen,

    nkTypeOfExpr,

    # nkConstTy,
    nkRefTy, nkVarTy, nkPtrTy, nkStaticTy, nkDistinctTy,
    nkMutableTy,

    nkTupleClassTy, nkTypeClassTy,

    nkOfInherit,

    nkArgList,

    nkWith, nkWithout,

    nkAsmStmt,
    nkCommentStmt,

    nkUsingStmt,
  }

proc getPIdent*(a: PNode): PIdent {.inline.} =
  ## Returns underlying `PIdent` for `{nkSym, nkIdent}`, or `nil`.
  # xxx consider whether also returning the 1st ident for {nkOpenSymChoice, nkClosedSymChoice}
  # which may simplify code.
  case a.kind
  of nkSym: a.sym.name
  of nkIdent: a.ident
  else: nil

proc getnimblePkg*(a: PSym): PSym =
  result = a
  while result != nil:
    case result.kind
    of skModule:
      result = result.owner
      assert result.kind == skPackage
    of skPackage:
      if result.owner == nil:
        break
      else:
        result = result.owner
    else:
      assert false, $result.kind

const
  moduleShift = when defined(cpu32): 20 else: 24

template id*(a: PIdObj): int =
  let x = a
  (x.itemId.module.int shl moduleShift) + x.itemId.item.int

type
  IdGenerator* = ref object # unfortunately, we really need the 'shared mutable' aspect here.
    module*: int32
    symId*: int32
    typeId*: int32
    sealed*: bool

const
  PackageModuleId* = -3'i32

proc idGeneratorFromModule*(m: PSym): IdGenerator =
  assert m.kind == skModule
  result = IdGenerator(module: m.itemId.module, symId: m.itemId.item, typeId: 0)

proc nextSymId*(x: IdGenerator): ItemId {.inline.} =
  assert(not x.sealed)
  inc x.symId
  result = ItemId(module: x.module, item: x.symId)

proc nextTypeId*(x: IdGenerator): ItemId {.inline.} =
  assert(not x.sealed)
  inc x.typeId
  result = ItemId(module: x.module, item: x.typeId)

when false:
  proc nextId*(x: IdGenerator): ItemId {.inline.} =
    inc x.item
    result = x[]

when false:
  proc storeBack*(dest: var IdGenerator; src: IdGenerator) {.inline.} =
    assert dest.ItemId.module == src.ItemId.module
    if dest.ItemId.item > src.ItemId.item:
      echo dest.ItemId.item, " ", src.ItemId.item, " ", src.ItemId.module
    assert dest.ItemId.item <= src.ItemId.item
    dest = src

proc getnimblePkgId*(a: PSym): int =
  let b = a.getnimblePkg
  result = if b == nil: -1 else: b.id

var ggDebug* {.deprecated.}: bool ## convenience switch for trying out things
#var
#  gMainPackageId*: int

proc isCallExpr*(n: PNode): bool =
  result = n.kind in nkCallKinds

proc discardSons*(father: PNode)

type Indexable = PNode | PType

proc len*(n: Indexable): int {.inline.} =
  ## number of children, unsafe if called on leaf nodes, see `safeLen`
  result = n.sons.len

proc safeLen*(n: PNode): int {.inline.} =
  ## works even for leaves.
  if n.kind in {nkNone..nkNilLit}: result = 0
  else: result = n.len

proc safeArrLen*(n: PNode): int {.inline.} =
  ## works for array-like objects (strings passed as openArray in VM).
  if n.kind in {nkStrLit..nkTripleStrLit}: result = n.strVal.len
  elif n.kind in {nkNone..nkFloat128Lit}: result = 0
  else: result = n.len

proc addAllowNil*(father, son: Indexable) {.inline.} =
  father.sons.add(son)

proc add*(father, son: Indexable) {.inline.} =
  assert son != nil
  addAllowNil(father, son)

template `[]`*(n: Indexable, i: int): Indexable = n.sons[i]
template `[]=`*(n: Indexable, i: int; x: Indexable) =
  n.sons[i] = x

template `[]`*(n: Indexable, i: BackwardsIndex): Indexable = n[n.len - i.int]
template `[]=`*(n: Indexable, i: BackwardsIndex; x: Indexable) = n[n.len - i.int] = x

proc getDeclPragma*(n: PNode): PNode =
  ## return the `nkPragma` node for declaration `n`, or `nil` if no pragma was found.
  ## Currently only supports routineDefs + {nkTypeDef}.
  case n.kind
  of routineDefs:
    if n[pragmasPos].kind != nkEmpty: result = n[pragmasPos]
  of nkTypeDef:
    #[
    type F3*{.deprecated: "x3".} = int

    TypeSection
      TypeDef
        PragmaExpr
          Postfix
            Ident "*"
            Ident "F3"
          Pragma
            ExprColonExpr
              Ident "deprecated"
              StrLit "x3"
        Empty
        Ident "int"
    ]#
    if n[0].kind == nkPragmaExpr:
      result = n[0][1]
  else:
    # support as needed for `nkIdentDefs` etc.
    result = nilPNode
  if result != nil:
    assert result.kind == nkPragma, $(result.kind, n.kind)

when defined(useNodeIds):
  const nodeIdToDebug* = NodeId -1 # 2322968

template newNodeImpl(kind: TNodeKind, info2: TLineInfo) =
  # result = PNode(kind: kind, info: info2)
  {.cast(noSideEffect).}:
    let
      nodeId = state.nextNodeId()
      nodeIdx = nodeId.idx
    state.nodeList.add NodeData(kind: kind)
    state.nodeInf.add info2
    state.nodeFlag.add {}
    case extraDataKind(kind):
    of ExtraDataInt:
      state.nodeInt.add 0
      state.nodeList[nodeIdx].extra = ExtraDataId state.nodeInt.len
    of ExtraDataFloat:
      state.nodeFlt.add 0
      state.nodeList[nodeIdx].extra = ExtraDataId state.nodeFlt.len
    of ExtraDataString:
      state.nodeStr.add ""
      state.nodeList[nodeIdx].extra = ExtraDataId state.nodeStr.len
    of ExtraDataSymbol:
      state.nodeSym.add nil
      state.nodeList[nodeIdx].extra = ExtraDataId state.nodeSym.len
    of ExtraDataIdentifier:
      state.nodeIdt.add nil
      state.nodeList[nodeIdx].extra = ExtraDataId state.nodeIdt.len
    of ExtraDataAst, ExtraDataNone:
      state.astData.add @[]
      state.nodeList[nodeIdx].extra = ExtraDataId state.astData.len
      discard

    result = PNode(id: nodeId)

template checkNodeIdForDebug() =
  when defined(useNodeIds):
    if result.id == nodeIdToDebug:
      echo "KIND ", result.kind
      writeStackTrace()

func newNodeI*(kind: TNodeKind, info: TLineInfo): PNode =
  ## new node with line info, no type, and no children
  newNodeImpl(kind, info)
  {.cast(noSideEffect).}:
    checkNodeIdForDebug()
    when defined(nimsuggest):
      # this would add overhead, so we skip it; it results in a small amount of
      # leaked entries for old PNode that gets re-allocated at the same address
      # as a PNode that has `nfHasComment` set (and an entry in that table).
      # Only `nfHasComment` should be used to test whether a PNode has a
      # comment; gconfig.comments can contain extra entries for deleted PNode's
      # with comments.
      gconfig.comments.del(result.id)

proc newNode*(kind: TNodeKind): PNode =
  ## new node with unknown line info, no type, and no children
  result = newNodeI(kind, unknownLineInfo)

proc newNodeI*(kind: TNodeKind, info: TLineInfo, children: int): PNode =
  ## new node with line info, no type, and children
  result = newNodeI(kind, info)
  if children > 0:
    newSeq(result.sons, children)

proc newNodeIT*(kind: TNodeKind, info: TLineInfo, typ: PType): PNode =
  ## new node with line info, type, and no children
  result = newNodeI(kind, info)
  result.typ = typ

proc newNodeIT*(kind: TNodeKind, info: TLineInfo, typ: PType, children: int): PNode =
  ## new node with line info, type, and children
  result = newNodeIT(kind, info, typ)
  if children > 0:
    newSeq(result.sons, children)

proc newErrorNodeIT*(rep: ReportId, info: TLineInfo, typ: PType): PNode =
  ## new node with line info, type, report, and no children
  result = newNodeIT(nkError, info, typ)
  result.reportId = rep

proc newTree*(kind: TNodeKind; children: varargs[PNode]): PNode =
  result = newNode(kind)
  if children.len > 0:
    result.info = children[0].info
  result.sons = @children

proc newTreeI*(kind: TNodeKind; info: TLineInfo; children: varargs[PNode]): PNode =
  result = newNodeI(kind, info)
  result.sons = @children

proc newTreeIT*(kind: TNodeKind; info: TLineInfo; typ: PType; children: varargs[PNode]): PNode =
  result = newNodeIT(kind, info, typ)
  result.sons = @children

template previouslyInferred*(t: PType): PType =
  if t.sons.len > 1: t.lastSon else: nil

when false:
  import tables, strutils
  var x: CountTable[string]

  addQuitProc proc () {.noconv.} =
    for k, v in pairs(x):
      echo k
      echo v

proc newSym*(symKind: TSymKind, name: PIdent, id: ItemId, owner: PSym,
             info: TLineInfo; options: TOptions = {}): PSym =
  # generates a symbol and initializes the hash field too
  result = PSym(name: name, kind: symKind, flags: {}, info: info, itemId: id,
                options: options, owner: owner, offset: defaultOffset)
  when false:
    if id.module == 48 and id.item == 39:
      writeStackTrace()
      echo "kind ", symKind, " ", name.s
      if owner != nil: echo owner.name.s

proc astdef*(s: PSym): PNode =
  # get only the definition (initializer) portion of the ast
  if s.ast != nil and s.ast.kind == nkIdentDefs:
    s.ast[2]
  else:
    s.ast

proc isMetaType*(t: PType): bool =
  return t.kind in tyMetaTypes or
         (t.kind == tyStatic and t.n == nil) or
         tfHasMeta in t.flags

proc isUnresolvedStatic*(t: PType): bool =
  return t.kind == tyStatic and t.n == nil

proc linkTo*(t: PType, s: PSym): PType {.discardable.} =
  t.sym = s
  s.typ = t
  result = t

proc linkTo*(s: PSym, t: PType): PSym {.discardable.} =
  t.sym = s
  s.typ = t
  result = s

template fileIdx*(c: PSym): FileIndex =
  # XXX: this should be used only on module symbols
  c.position.FileIndex

template filename*(c: PSym): string =
  # XXX: this should be used only on module symbols
  c.position.FileIndex.toFilename

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

proc newSymNode*(sym: PSym): PNode =
  result = newNode(nkSym)
  result.sym = sym
  result.typ = sym.typ
  result.info = sym.info

proc newSymNode*(sym: PSym, info: TLineInfo): PNode =
  result = newNode(nkSym)
  result.sym = sym
  result.typ = sym.typ
  result.info = info

proc newIntNode*(kind: TNodeKind, intVal: BiggestInt): PNode =
  result = newNode(kind)
  result.intVal = intVal

proc newIntNode*(kind: TNodeKind, intVal: Int128): PNode =
  result = newNode(kind)
  result.intVal = castToInt64(intVal)

func lastSon*(n: Indexable): Indexable =
  {.cast(noSideEffect).}:
    # erroneously inferred side-effect
    n.sons[^1]

proc skipTypes*(t: PType, kinds: TTypeKinds): PType =
  ## Used throughout the compiler code to test whether a type tree contains or
  ## doesn't contain a specific type/types - it is often the case that only the
  ## last child nodes of a type tree need to be searched. This is a really hot
  ## path within the compiler!
  result = t
  while result.kind in kinds: result = lastSon(result)

proc skipDistincts*(t: PType): PType {.inline.} =
  ## Skips over all possible distinct instantiations, getting base.
  skipTypes(t, {tyAlias, tyGenericInst, tyDistinct})

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
  of tyBool, tyEnum:
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

proc newProcNode*(kind: TNodeKind, info: TLineInfo, body: PNode,
                 params,
                 name, pattern, genericParams,
                 pragmas, exceptions: PNode): PNode =
  result = newNodeI(kind, info)
  result.sons = @[name, pattern, genericParams, params,
                  pragmas, exceptions, body]

const
  UnspecifiedLockLevel* = TLockLevel(-1'i16)
  MaxLockLevel* = 1000'i16
  UnknownLockLevel* = TLockLevel(1001'i16)
  AttachedOpToStr*: array[TTypeAttachedOp, string] = [
    "=destroy", "=copy", "=sink", "=trace", "=deepcopy"]

proc `$`*(x: TLockLevel): string =
  if x.ord == UnspecifiedLockLevel.ord: result = "<unspecified>"
  elif x.ord == UnknownLockLevel.ord: result = "<unknown>"
  else: result = $int16(x)

proc `$`*(s: PSym): string =
  if s != nil:
    result = s.name.s & "@" & $s.id
  else:
    result = "<nil>"

proc newType*(kind: TTypeKind, id: ItemId; owner: PSym): PType =
  result = PType(kind: kind, owner: owner, size: defaultSize,
                 align: defaultAlignment, itemId: id,
                 lockLevel: UnspecifiedLockLevel,
                 uniqueId: id)
  when false:
    if result.itemId.module == 55 and result.itemId.item == 2:
      echo "KNID ", kind
      writeStackTrace()


proc mergeLoc(a: var TLoc, b: TLoc) =
  if a.k == low(typeof(a.k)): a.k = b.k
  if a.storage == low(typeof(a.storage)): a.storage = b.storage
  a.flags.incl b.flags
  if a.lode == nil: a.lode = b.lode
  if a.r == nil: a.r = b.r

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
      if dest.sym.annex == nil: dest.sym.annex = src.sym.annex
      mergeLoc(dest.sym.loc, src.sym.loc)
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
  result.loc = s.loc
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
  result.loc = s.loc
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

proc skipTypes*(t: PType, kinds: TTypeKinds; maxIters: int): PType =
  result = t
  var i = maxIters
  while result.kind in kinds:
    result = lastSon(result)
    dec i
    if i == 0: return nil

proc skipTypesOrNil*(t: PType, kinds: TTypeKinds): PType =
  ## same as skipTypes but handles 'nil'
  result = t
  while result != nil and result.kind in kinds:
    if result.len == 0: return nil
    result = lastSon(result)

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

  let mask = elem.flags * {tfHasAsgn, tfHasOwned}
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
  if not son.isNil: propagateToOwner(father, son, propagateHasAsgn)

proc rawAddSonNoPropagationOfTypeFlags*(father, son: PType) =
  father.sons.add(son)

proc addSonNilAllowed*(father, son: PNode) =
  father.sons.add(son)

proc delSon*(father: PNode, idx: int) =
  if father.len == 0: return
  for i in idx..<father.len - 1: father[i] = father[i + 1]
  father.sons.setLen(father.len - 1)

proc applyToNode*(src, dest: PNode) =
  ## used for the VM when we pass nodes "by value"
  # assert not dest.isNil
  # assert not src.isNil
  # assert dest.id != src.id, "applying to self, id: " & $src.id
  state.nodeList[dest.idx] = state.nodeList[src.idx]
  state.nodeFlag[dest.idx] = state.nodeFlag[src.idx]
  state.nodeInf[dest.idx] = state.nodeInf[src.idx]
  if state.nodeTyp.hasKey(src.id):
    state.nodeTyp[dest.id] = state.nodeTyp[src.id]
  if state.nodeRpt.hasKey(src.id):
    state.nodeRpt[dest.id] = state.nodeRpt[src.id]
  case src.kind.extraDataKind
  of ExtraDataInt:
    dest.intVal = src.intVal
  of ExtraDataFloat:
    dest.floatVal = src.floatVal
  of ExtraDataString:
    dest.strVal = src.strVal
  of ExtraDataSymbol:
    dest.sym = src.sym
  of ExtraDataIdentifier:
    dest.ident = src.ident
  of ExtraDataAst, ExtraDataNone:
    dest.sons = src.sons

template copyNodeImpl(dst, src, processSonsStmt) =
  # does not copy src's sons to dst
  if src == nil:
    return nilPNode
  dst = newNode(src.kind)
  dst.info = src.info
  dst.typ = src.typ
  dst.flags = src.flags * PersistentNodeFlags
  dst.comment = src.comment
  if src.kind == nkError:
    dst.reportId = src.reportId
  when defined(useNodeIds):
    if dst.id == nodeIdToDebug:
      echo "COMES FROM ", src.id
  case src.kind
  of nkCharLit..nkUInt64Lit: dst.intVal = src.intVal
  of nkFloatLiterals: dst.floatVal = src.floatVal
  of nkSym: dst.sym = src.sym
  of nkIdent: dst.ident = src.ident
  of nkStrLit..nkTripleStrLit: dst.strVal = src.strVal
  else: processSonsStmt

proc copyNode*(src: PNode): PNode =
  # does not copy its sons!
  copyNodeImpl(result, src):
    discard

type
  NodeDataToClear = enum
    nodeClearAst,
    nodeClearFlg,
    nodeClearInf,
    nodeClearSym,
    nodeClearIdt,
    nodeClearTyp,
    nodeClearInt,
    nodeClearFlt,
    nodeClearStr,
    nodeClearRpt,
    nodeClearCmt

func determineNodeDataToClear(k: TNodeKind): set[NodeDataToClear] {.inline.} =
  case k
  of nkCharLit..nkUInt64Lit:
    {nodeClearAst, nodeClearSym, nodeClearIdt, nodeClearFlt, nodeClearStr, nodeClearRpt}
  of nkFloatLit..nkFloat128Lit:
    {nodeClearAst, nodeClearSym, nodeClearIdt, nodeClearInt, nodeClearStr, nodeClearRpt}
  of nkStrLit..nkTripleStrLit:
    {nodeClearAst, nodeClearSym, nodeClearIdt, nodeClearInt, nodeClearFlt, nodeClearRpt}
  of nkSym:
    {nodeClearAst, nodeClearIdt, nodeClearInt, nodeClearFlt, nodeClearStr, nodeClearRpt}
  of nkIdent:
    {nodeClearAst, nodeClearSym, nodeClearInt, nodeClearFlt, nodeClearStr, nodeClearRpt}
  of nkError:
    {nodeClearIdt, nodeClearSym, nodeClearInt, nodeClearFlt, nodeClearStr}
  else:
    {nodeClearIdt, nodeClearSym, nodeClearInt, nodeClearFlt, nodeClearStr, nodeClearRpt}
    # xxx: this should handle empty and other special cases

template transitionNodeKindCommon(k, old: TNodeKind) =
  # xxx: this used to be a memory copy, but now we just change the kind
  # xxx: trace the transition for lineage information

  # clear old data: this was added as part of the data oriented design
  #                 refactor; might be a source of bugs wrt to how legacy code
  #                 expects things to work, try to favour fixing legacy code
  let
    kExtraDataKind = extraDataKind(k)
    oldExtraDataKind = extraDataKind(old)
    sameNodeVariety = kExtraDataKind == oldExtraDataKind ## kinds need equivalent storage
    clears =
      if sameNodeVariety: {}            # don't clear if the same
      else: determineNodeDataToClear(k)
    resetExtraDataId = clears != {} and oldExtraDataKind != ExtraDataNone

  for clear in clears.items:
    case clear
    of nodeClearAst:
      if oldExtraDataKind == ExtraDataAst:
        state.astData[n.extraIdx].setLen(0)
    of nodeClearFlg: state.nodeFlag[n.idx] = {}
    of nodeClearInf: state.nodeInf[n.idx] = unknownLineInfo
    of nodeClearTyp: state.nodeTyp.del(n.id)
    of nodeClearRpt: state.nodeRpt.del(n.id)
    of nodeClearSym:
      if oldExtraDataKind == ExtraDataSymbol:
        state.nodeSym[n.extraId.idx] = nil
    of nodeClearIdt:
      if oldExtraDataKind == ExtraDataIdentifier:
        state.nodeIdt[n.extraId.idx] = nil
    of nodeClearInt:
      if oldExtraDataKind == ExtraDataInt:
        state.nodeInt[n.extraId.idx] = 0
    of nodeClearFlt:
      if oldExtraDataKind == ExtraDataFloat:
        state.nodeFlt[n.extraId.idx] = 0.0
    of nodeClearStr:
      if oldExtraDataKind == ExtraDataString:
        state.nodeStr[n.extraId.idx] = ""
    of nodeClearCmt: gconfig.comments.del(n.id)
  
  state.nodeList[n.idx].kind = k

  # xxx: setup storage if we reset it
  if resetExtraDataId:
    state.nodeList[n.idx].extra = nilExtraDataId
  
  when defined(useNodeIds):
    if n.id == nodeIdToDebug:
      echo "KIND ", n.kind
      writeStackTrace()

proc transitionSonsKind*(n: PNode, kind: range[nkDotCall..nkTupleConstr]) =
  # xxx: just change the kind now, might need clearing/validation here
  transitionNodeKindCommon(kind, n.kind)
  # xxx: this used to copy sons, not sure if that's still needed.

proc transitionIntKind*(n: PNode, kind: range[nkCharLit..nkUInt64Lit]) =
  # xxx: just change the kind now, might need clearing/validation here
  transitionNodeKindCommon(kind, n.kind)

proc transitionToNilLit*(n: PNode) =
  ## used to reset a node to a nil literal
  transitionNodeKindCommon(nkNilLit, n.kind)

proc transitionNoneToSym*(n: PNode) =
  # xxx: just change the kind now, might need clearing/validation here
  #      see the hack in `semtypes.semTypeIdent`
  transitionNodeKindCommon(nkSym, n.kind)

template transitionSymKindCommon*(k: TSymKind) =
  let obj {.inject.} = s[]
  s[] = TSym(kind: k, itemId: obj.itemId, magic: obj.magic, typ: obj.typ, name: obj.name,
             info: obj.info, owner: obj.owner, flags: obj.flags, ast: obj.ast,
             options: obj.options, position: obj.position, offset: obj.offset,
             loc: obj.loc, annex: obj.annex, constraint: obj.constraint)
  when defined(nimsuggest):
    s.allUsages = obj.allUsages

proc transitionGenericParamToType*(s: PSym) =
  transitionSymKindCommon(skType)

proc transitionRoutineSymKind*(s: PSym, kind: range[skProc..skTemplate]) =
  transitionSymKindCommon(kind)
  s.gcUnsafetyReason = obj.gcUnsafetyReason
  s.transformedBody = obj.transformedBody

proc transitionToLet*(s: PSym) =
  transitionSymKindCommon(skLet)
  s.guard = obj.guard
  s.bitsize = obj.bitsize
  s.alignment = obj.alignment

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

proc hasSonWith*(n: PNode, kind: TNodeKind): bool =
  for i in 0..<n.len:
    if n[i].kind == kind:
      return true
  result = false

proc hasNilSon*(n: PNode): bool =
  for i in 0..<n.safeLen:
    if n[i] == nil:
      return true
    elif hasNilSon(n[i]):
      return true
  result = false

proc containsNode*(n: PNode, kinds: TNodeKinds): bool =
  if n == nil: return
  case n.kind
  of nkEmpty..nkNilLit: result = n.kind in kinds
  else:
    for i in 0..<n.len:
      if n.kind in kinds or containsNode(n[i], kinds): return true

proc hasSubnodeWith*(n: PNode, kind: TNodeKind): bool =
  case n.kind
  of nkEmpty..nkNilLit, nkFormalParams: result = n.kind == kind
  else:
    for i in 0..<n.len:
      if (n[i].kind == kind) or hasSubnodeWith(n[i], kind):
        return true
    result = false

proc getInt*(a: PNode): Int128 =
  case a.kind
  of nkCharLit, nkUIntLit..nkUInt64Lit:
    result = toInt128(cast[uint64](a.intVal))
  of nkInt8Lit..nkInt64Lit:
    result = toInt128(a.intVal)
  of nkIntLit:
    # XXX: enable this assert
    # assert a.typ.kind notin {tyChar, tyUint..tyUInt64}
    result = toInt128(a.intVal)
  else:
    raiseRecoverableError("cannot extract number from invalid AST node")

proc getInt64*(a: PNode): int64 {.deprecated: "use getInt".} =
  case a.kind
  of nkCharLit, nkUIntLit..nkUInt64Lit, nkIntLit..nkInt64Lit:
    result = a.intVal
  else:
    raiseRecoverableError("cannot extract number from invalid AST node")

proc getFloat*(a: PNode): BiggestFloat =
  case a.kind
  of nkFloatLiterals: result = a.floatVal
  of nkCharLit, nkUIntLit..nkUInt64Lit, nkIntLit..nkInt64Lit:
    result = BiggestFloat a.intVal
  else:
    raiseRecoverableError("cannot extract number from invalid AST node")
    #doAssert false, "getFloat"
    #internalError(a.info, "getFloat")
    #result = 0.0

proc getStr*(a: PNode): string =
  case a.kind
  of nkStrLit..nkTripleStrLit: result = a.strVal
  of nkNilLit:
    # let's hope this fixes more problems than it creates:
    result = ""
  else:
    raiseRecoverableError("cannot extract string from invalid AST node")
    #doAssert false, "getStr"
    #internalError(a.info, "getStr")
    #result = ""

proc getStrOrChar*(a: PNode): string =
  case a.kind
  of nkStrLit..nkTripleStrLit: result = a.strVal
  of nkCharLit..nkUInt64Lit: result = $chr(int(a.intVal))
  else:
    raiseRecoverableError("cannot extract string from invalid AST node")
    #doAssert false, "getStrOrChar"
    #internalError(a.info, "getStrOrChar")
    #result = ""

proc isGenericParams*(n: PNode): bool {.inline.} =
  ## used to judge whether a node is generic params.
  n != nil and n.kind == nkGenericParams

proc isGenericRoutine*(n: PNode): bool  {.inline.} =
  n != nil and n.kind in callableDefs and n[genericParamsPos].isGenericParams

proc isError*(n: PNode): bool {.inline.} =
  ## whether the node is an error, strictly checks nkError and is nil safe
  n != nil and n.kind == nkError

proc isError*(s: PSym): bool {.inline.} =
  ## whether the symbol is an error, strictly checks skError, an error node
  ## exists, and is nil safe.
  s != nil and s.kind == skError and s.ast.isError

proc isError*(t: PType): bool {.inline.} =
  ## whether the type is an error. useful because of compiler legacy, as
  ## `tyError` isn't an enum field rather a const refering to `tyProxy`.
  ##
  ## xxx: currently we have no way to disambiguate between legacy and new
  t != nil and t.kind == tyError

proc isErrorLike*(t: PType): bool {.inline.} =
  ## whether the type is an error. useful because of compiler legacy, as
  ## `tyError` isn't an enum field rather a const refering to `tyProxy`.
  ##
  ## xxx: currently we have no way to disambiguate between legacy and new
  t != nil and (t.kind == tyError or t.sym.isError)

proc isErrorLike*(s: PSym): bool {.inline.} =
  ## whether the symbol is an error. useful because of compiler legacy, as
  ## `skError` isn't an enum field rather a const refering to `skUnkonwn`. we
  ## disambiguate via the presence of the ast field being non-nil and of kind
  ## `nkError`
  s != nil and (s.isError or s.typ.isError)

proc isErrorLike*(n: PNode): bool {.inline.} =
  ## whether the node is an error, including error symbol, or error type
  ## xxx: longer term we should probably not produce nodes like these in the
  ##      first place and mark them as nkErrors with an appropriate error kind.
  n != nil and (
      case n.kind
      of nkError: true
      of nkSym: n.sym.isErrorLike
      of nkType: n.typ.isErrorLike
      else: n.typ.isError # if it has a type, it shouldn't be an error
    )

proc isGenericRoutineStrict*(s: PSym): bool {.inline.} =
  ## determines if this symbol represents a generic routine
  ## the unusual name is so it doesn't collide and eventually replaces
  ## `isGenericRoutine`
  s.kind in skProcKinds and s.ast.isGenericRoutine

proc isGenericRoutine*(s: PSym): bool {.inline.} =
  ## determines if this symbol represents a generic routine or an instance of
  ## one. This should be renamed accordingly and `isGenericRoutineStrict`
  ## should take this name instead.
  ##
  ## Warning/XXX: Unfortunately, it considers a proc kind symbol flagged with
  ## sfFromGeneric as a generic routine. Instead this should likely not be the
  ## case and the concepts should be teased apart:
  ## - generic definition
  ## - generic instance
  ## - either generic definition or instance
  s.kind in skProcKinds and (sfFromGeneric in s.flags or
                             s.ast.isGenericRoutine)

proc skipGenericOwner*(s: PSym): PSym =
  ## Generic instantiations are owned by their originating generic
  ## symbol. This proc skips such owners and goes straight to the owner
  ## of the generic itself (the module or the enclosing proc).
  result = if s.kind in skProcKinds and sfFromGeneric in s.flags:
             s.owner.owner
           else:
             s.owner

proc originatingModule*(s: PSym): PSym =
  result = s.owner
  while result.kind != skModule: result = result.owner

proc isRoutine*(s: PSym): bool {.inline.} =
  result = s.kind in skProcKinds

proc isCompileTimeProc*(s: PSym): bool {.inline.} =
  result = s.kind == skMacro or
           s.kind in {skProc, skFunc} and sfCompileTime in s.flags

proc isRunnableExamples*(n: PNode): bool =
  # Templates and generics don't perform symbol lookups.
  result = n.kind == nkSym and n.sym.magic == mRunnableExamples or
    n.kind == nkIdent and n.ident.s == "runnableExamples"

proc requiredParams*(s: PSym): int =
  # Returns the number of required params (without default values)
  # XXX: Perhaps we can store this in the `offset` field of the
  # symbol instead?
  for i in 1..<s.typ.len:
    if s.typ.n[i].sym.ast != nil:
      return i - 1
  return s.typ.len - 1

proc hasPattern*(s: PSym): bool {.inline.} =
  result = isRoutine(s) and s.ast[patternPos].kind != nkEmpty

iterator items*(n: PNode): PNode =
  for i in 0..<n.safeLen: yield n[i]

iterator pairs*(n: PNode): tuple[i: int, n: PNode] =
  for i in 0..<n.safeLen: yield (i, n[i])

proc isAtom*(n: PNode): bool {.inline.} =
  result = n.kind >= nkNone and n.kind <= nkNilLit

proc isEmptyType*(t: PType): bool {.inline.} =
  ## 'void' and 'typed' types are often equivalent to 'nil' these days:
  result = t == nil or t.kind in {tyVoid, tyTyped}

proc makeStmtList*(n: PNode): PNode =
  if n.kind == nkStmtList:
    result = n
  else:
    result = newNodeI(nkStmtList, n.info)
    result.add n

proc skipStmtList*(n: PNode): PNode =
  if n.kind in {nkStmtList, nkStmtListExpr}:
    for i in 0..<n.len-1:
      if n[i].kind notin {nkEmpty, nkCommentStmt}: return n
    result = n.lastSon
  else:
    result = n

proc toVar*(typ: PType; kind: TTypeKind; idgen: IdGenerator): PType =
  ## If ``typ`` is not a tyVar then it is converted into a `var <typ>` and
  ## returned. Otherwise ``typ`` is simply returned as-is.
  result = typ
  if typ.kind != kind:
    result = newType(kind, nextTypeId(idgen), typ.owner)
    rawAddSon(result, typ)

proc toRef*(typ: PType; idgen: IdGenerator): PType =
  ## If ``typ`` is a tyObject then it is converted into a `ref <typ>` and
  ## returned. Otherwise ``typ`` is simply returned as-is.
  result = typ
  if typ.skipTypes({tyAlias, tyGenericInst}).kind == tyObject:
    result = newType(tyRef, nextTypeId(idgen), typ.owner)
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
    of tyGenericBody: result = result.lastSon
    of tyRef, tyPtr, tyGenericInst, tyGenericInvocation, tyAlias: result = result[0]
      # automatic dereferencing is deep, refs #18298.
    else: break
  assert result.sym != nil

proc isImportedException*(t: PType; conf: ConfigRef): bool =
  assert t != nil

  if conf.exc != excCpp:
    return false

  let base = t.skipTypes({tyAlias, tyPtr, tyDistinct, tyGenericInst})

  if base.sym != nil and {sfCompileToCpp, sfImportc} * base.sym.flags != {}:
    result = true

proc isInfixAs*(n: PNode): bool =
  return n.kind == nkInfix and n[0].kind == nkIdent and n[0].ident.s == "as"

proc skipColon*(n: PNode): PNode =
  result = n
  if n.kind == nkExprColonExpr:
    result = n[1]

proc findUnresolvedStatic*(n: PNode): PNode =
  # n.typ == nil: see issue #14802
  if n.kind == nkSym and n.typ != nil and n.typ.kind == tyStatic and n.typ.n == nil:
    return n

  for son in n:
    let n = son.findUnresolvedStatic
    if n != nil: return n

  return nilPNode

when false:
  proc containsNil*(n: PNode): bool =
    # only for debugging
    if n.isNil: return true
    for i in 0..<n.safeLen:
      if n[i].containsNil: return true


template hasDestructor*(t: PType): bool = {tfHasAsgn, tfHasOwned} * t.flags != {}

template incompleteType*(t: PType): bool =
  t.sym != nil and {sfForward, sfNoForward} * t.sym.flags == {sfForward}

template typeCompleted*(s: PSym) =
  incl s.flags, sfNoForward

template detailedInfo*(sym: PSym): string =
  sym.name.s

proc isInlineIterator*(typ: PType): bool {.inline.} =
  typ.kind == tyProc and tfIterator in typ.flags and typ.callConv != ccClosure

proc isClosureIterator*(typ: PType): bool {.inline.} =
  typ.kind == tyProc and tfIterator in typ.flags and typ.callConv == ccClosure

proc isClosure*(typ: PType): bool {.inline.} =
  typ.kind == tyProc and typ.callConv == ccClosure

proc isSinkParam*(s: PSym): bool {.inline.} =
  s.kind == skParam and (s.typ.kind == tySink or tfHasOwned in s.typ.flags)

proc isSinkType*(t: PType): bool {.inline.} =
  t.kind == tySink or tfHasOwned in t.flags

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

const magicsThatCanRaise = {
  mNone, mSlurp, mStaticExec, mParseExprToAst, mParseStmtToAst, mEcho}

proc canRaiseConservative*(fn: PNode): bool =
  if fn.kind == nkSym and fn.sym.magic notin magicsThatCanRaise:
    result = false
  else:
    result = true

proc canRaise*(fn: PNode): bool =
  if fn.kind == nkSym and (fn.sym.magic notin magicsThatCanRaise or
      {sfImportc, sfInfixCall} * fn.sym.flags == {sfImportc} or
      sfGeneratedOp in fn.sym.flags):
    result = false
  elif fn.kind == nkSym and fn.sym.magic == mEcho:
    result = true
  else:
    # TODO check for n having sons? or just return false for now if not
    if fn.typ != nil and fn.typ.n != nil and fn.typ.n[0].kind == nkSym:
      result = false
    else:
      result = fn.typ != nil and fn.typ.n != nil and ((fn.typ.n[0].len < effectListLen) or
        (fn.typ.n[0][exceptionEffects] != nil and
        fn.typ.n[0][exceptionEffects].safeLen > 0))

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

proc skipAddr*(n: PNode): PNode {.inline.} =
  (if n.kind == nkHiddenAddr: n[0] else: n)

proc isNewStyleConcept*(n: PNode): bool {.inline.} =
  assert n.kind == nkTypeClassTy
  result = n[0].kind == nkEmpty
