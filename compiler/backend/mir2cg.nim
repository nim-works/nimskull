## Implements the MIR -> CGIR translation. At the moment, a lot of the
## NimSkull's semantics are decided here.
##
## Nomenclature:
## * translating MIR trees / concepts to CGIR -> `xToCgir`, where `x` is the
##   MIR level tree/concept
## * routines that emit one more CGIR statements -> `emitX`
## * routines that produce CGIR AST -> `genX` or just `X`

import
  std/[
    algorithm,
    packedsets,
    strtabs,
    tables,
    sets,
    sugar
  ],
  std/private/[
    containers
  ],
  compiler/front/[
    options
  ],
  compiler/sem/[
    modulelowering
  ],
  compiler/ast/[
    ast,
    ast_types,
    ast_query,
    idents,
    lineinfos,
    types
  ],
  compiler/modules/[
    modulegraphs,
    magicsys
  ],
  compiler/utils/[
    bitsets
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/backend/[
    ccgutils,
    cgir2,
    cgirbuilder,
    mirflow,
    mirtypes2cg
  ],
  compiler/mir/[
    datatables,
    mirenv,
    mirtrees,
    mirtypes,
    sourcemaps
  ]

import std/options as std_options
from compiler/mir/mirbodies import MirBody, Local, `[]`, resultId

from compiler/backend/extccomp import CC, TInfoCCProp

from compiler/front/msgs import unquotedFilename, toFullPath, localReport, `$`

from compiler/ast/reports_sem import SemReport, reportStr, reportSem, reportSym
from compiler/ast/report_enums import ReportKind

from std/importutils import privateAccess

when defined(nimCompilerStacktraceHints):
  import compiler/utils/debugutils

type
  Capability* = enum
    ## A capability of the targeted code generator.
    capExceptions ## the code generator supports checked calls, raise, etc.

  Node = CgNode ## shorthand

  ExprMode = enum
    emValue
    emSym
    emLvalue
    emIndirect

  Expr = object
    ## A CGIR expression with enough information to use it in all
    ## possible contexts.
    mode: ExprMode
    typ: TypeId
    n: NodeRef

  ProcContext = object
    ## Translation context local to a single procedure.
    body: MirBody
      ## the original MIR body of the procedure
    localMap: OrdinalSeq[LocalId, StringId]
      # MIR local -> name
    accessors: OrdinalSeq[LocalId, tuple[e: NodeRef, mode: ExprMode]]
      ## MIR local -> accessor expression
    nextLabel: uint32
      ## the integer prefix to use for the next-created label
    nextLocal: uint32
      ## the integer prefix to use for the next-created temporary
    useLineTrace: bool
      ## whether emitting line traces is enabled
    unwindLabel: LabelId
      ## the label to jump to when unwinding/returning
    errorLocal: Expr
      ## access of the local storing the pointer-to-error-flag
    frameLocal: Expr
      ## access of the local storing the stack-trace entry. Cached here
      ## for convenience and to speed up line-trace generation
    lastLine: TLineInfo.line
      ## line number used for the previous static line update
    lastFileIndex: TLineInfo.fileIndex
      ## file index used for the previous static file update

  Context* = object
    graph {.requiresInit.}: ModuleGraph
    caps: set[Capability]
      ## capability of the targeted code generator
    module: CgModule
      ## the in-progress module

    tctx: mirtypes2cg.Context

    rttiV1Map: Table[SigHash, StringId]
      ## maps unified types (via their sighash) to their RTTIv1 global
    rttiV2Map: Table[SigHash, StringId]
      ## maps unified types (via their sighash) to their RTTIv2 global
    rtti: seq[tuple[global: StringId, id: ItemId]]
      ## all rtti globals together with their home module

    rttiV1Type: TypeId
      ## cached type ID for the ``TNimType`` type
    rttiV1NodeType: TypeId
      ## cached type ID for the ``TNimNode`` type
    rttiV2Type: TypeId
      ## cached type ID for the ``TNimTypeV2`` type

    names: Table[int, StringId]
      ## symbol IDs -> external names

    dataMap: Table[DataId, Datum]
      ## maps MIR data to the corresponding CGIR data. Populated on demand
    defaults: Table[TypeId, Datum]
      ## type -> default value for the type

    sourceLocs: BiTable[SourceLoc]
      # TODO: use an "intrusive BiTable", that is, a bitable where only the
      #       LitId sequence is stored here, with the other half being
      #       represented directly by `module.infos`. This'll get rid of the
      #       privateAccess hack for extracting the data sequence
    fullPaths: Table[FileIndex, StringId]
      ## caches the packed string for full module paths
    fileNames: Table[FileIndex, StringId]
      ## caches the packed string for module names/paths. Populated on demand
      ## and used to speed up line-trace generation

    prc: ProcContext

const
  NonMagics* = {mNewSeq, mSetLengthSeq, mAppendSeqElem,
               mEnumToStr, mDotDot, mEqCString, mAbsI, mExit, mIsolate}
  StrLitFlag = 1'i64 shl 62
    ## or'ed into the capacity of seqs and strings in order to mark the
    ## data as being immutable

using
  c: var Context
  bu: var Builder
  env: var MirEnv
  tree: MirTree
  n: NodePosition
  stmts: var seq[NodeRef]

proc checkProc(c; info: SourceId, s: PSym) =
  ## Makes sure routine symbol is not a compile-time only procedure, emitting
  ## an error if it is.
  # TODO: perform this check during semantic analysis
  if sfCompileTime in s.flags:
    localReport(c.graph.config, c.prc.body.source[info],
      reportSym(rsemCannotCodegenCompiletimeProc, s))

proc sameType(env: TypeEnv, a, b: TypeId): bool =
  ## Compares two types for equality at the MIR level, *not the CGIR level*.
  env.canonical(a) == env.canonical(b)

proc payloadPtrType(env: TypeEnv, typ: TypeId): TypeId =
  ## Returns the ID of a seq/string type's payload type.
  env[env.lookupField(typ, 1)].typ

proc seqElemType(env: TypeEnv, typ: TypeId): TypeId =
  let desc = env.headerFor(typ, Original)
  if desc.kind == tkString: CharType
  else:                     desc.elem

proc getFieldType(env: TypeEnv, typ: TypeId, pos: int32): TypeId =
  env[env.lookupField(typ, pos)].typ

func isOpenArray(env: TypeEnv, typ: TypeId): bool =
  env.headerFor(typ, Canonical).kind == tkOpenArray

func isObject(env: TypeEnv, typ: TypeId): bool =
  env.headerFor(typ, Canonical).kind in {tkStruct, tkUnion}

func isPointerLike(env: TypeEnv, typ: TypeId): bool =
  env.headerFor(typ, Canonical).kind in {tkRef, tkPtr, tkVar, tkLent}

proc isVoidReturn(env: TypeEnv, typ: TypeId): bool =
  ## Whether the procedure type `typ` has a 'void' return type at the
  ## CGIR level.
  typ == VoidType or env.headerFor(typ, Lowered).kind == tkArray

func isPassByRef(env: TypeEnv; desc: TypeHeader, i: int): bool =
  ## Whether the `i`-th parameter of proc/closure type `desc` is a pass-by-
  ## reference parameter.
  for (j, _, flags) in env.params(desc):
    if j == i:
      return pfByRef in flags
  # this case is possible for C-interop varargs
  result = false

proc skip(env: TypeEnv, t: TypeId): TypeId =
  ## Return `t` if its not an imported type. For imported types, returns the
  ## "underlying" type.
  result = t
  while env.headerFor(result, Original).kind == tkImported:
    result = env.skip(env.headerFor(result, Original).elem)

proc newPayloadType(env; elem: TypeId, count: Positive): TypeId =
  ## Returns a sized payload type for a sequence with element type `elem` and
  ## `count` number of items.
  let elem = env.types.canonical(elem)
  env.types.newTuple(env.types.sizeType, env.types.newArray(count, elem))

proc newPtrType(env; typ: TypeId): TypeId =
  env.types.newPtr(env.types.canonical(typ))

proc newPtrToArrayType(env; typ: TypeId): TypeId =
  env.types.newPtrToArray(env.types.canonical(typ))

proc makeExpr(typ: TypeId, n: NodeRef): Expr {.inline.} =
  Expr(mode: emValue, typ: typ, n: n)

template buildExpr(bu: var Builder, t: TypeId, e: untyped): Expr =
  Expr(mode: emValue, typ: t, n: bu.build(e))

template buildLval(bu: var Builder, t: TypeId, e: untyped): Expr =
  Expr(mode: emLvalue, typ: t, n: bu.build(e))

template buildInd(bu: var Builder, t: TypeId, e: untyped): Expr =
  Expr(mode: emIndirect, typ: t, n: bu.build(e))

template buildTree(c: var Context, body: untyped): cgir2.NodeIndex =
  if true:
    var bu {.inject.}: Builder
    let got = bu.build(body)
    c.module.ast.append(bu, got)
  else:
    unreachable()

proc addDatum(c; bu: sink Builder, n: NodeRef): Datum =
  c.module.data.add c.module.ast.append(bu, n)

template buildDatum(c: var Context, body: untyped): Datum =
  if true:
    var bu {.inject.}: Builder
    let got = bu.build(body)
    c.addDatum(bu, got)
  else:
    unreachable()

template buildDatumExpr(c: var Context, t: TypeId, body: untyped): Expr =
  let x = t # evaluate once
  let d = buildDatum(c, body)
  # keep the template simpler by not requiring a builder
  Expr(mode: emSym, typ: x, n: NodeRef(datumRef(d)))

proc typeRef(typ: StringId): Node =
  node(cnkType, cast[uint32](typ))

proc localRef(name: StringId): Node =
  node(cnkLocal, cast[uint32](name))

proc procRef(prc: StringId): Node =
  node(cnkProc, cast[uint32](prc))

proc globalRef(name: StringId): Node =
  node(cnkGlobal, cast[uint32](name))

proc datumRef(name: Datum): Node =
  node(cnkDatum, cast[uint32](name))

proc labelRef(label: LabelId): Node =
  node(cnkLabel, label.uint32)

proc typeRef(c; env: TypeEnv, typ: TypeId): Node =
  typeRef(typeToCgir(c.tctx, env, c.module, typ))

proc typeRef(c; env: MirEnv, typ: TypeId): Node =
  typeRef(c, env.types, typ)

proc intNode(val: BiggestInt, c): CgNode {.inline.} =
  node(cnkInt, c.module.pack(val))
proc strNode(val: string, c): CgNode {.inline.} =
  node(cnkString, cast[uint32](c.module.put(val)))

template valToNode(x: bool): Node = node(cnkBool, ord(x))
template valToNode(x: BiggestInt): Node = intNode(x, c)
template valToNode(x: string): Node = strNode(x, c)
template valToNode(x: StringId): Node = node(cnkString, cast[uint32](x))
template valToNode(x: TypeId): Node =
  let y = x # prevent issues when expression `x` modifies `env`
  typeRef(c, env, y)
template valToNode(x: enum): Node = intNode(ord(x), c)
template valToNode[E](x: set[E]): Node = intNode(cast[int](x), c)

proc genInt(c; env: MirEnv, val: BiggestInt, typ: TypeId, bu): NodeRef =
  bu.build Value(typ, val)

proc genFloat(c; env: MirEnv, val: float64, typ: TypeId, bu): NodeRef =
  bu.build Value(typ, ^node(cnkFloat, c.module.pack(val)))

proc use(c; env: MirEnv, e: Expr, bu): NodeRef =
  ## Constructs the CGIR for using an expression in a normal value context.
  case e.mode
  of emIndirect:
    bu.build Load(^e.typ, ^e.n)
  of emSym:
    bu.build Use(^e.typ, ^e.n)
  of emLvalue, emValue:
    e.n

proc root(c; env: MirEnv, e: Expr, bu): NodeRef =
  ## Constructs the CGIR for using an expression in a path root position.
  case e.mode
  of emSym:
    bu.build Use(^e.typ, ^e.n)
  of emLvalue, emValue, emIndirect:
    e.n

# templates for shortening invocations of common routines:
template use(bu: var Builder, e: Expr): NodeRef =
  mixin c, env
  use(c, env, e, bu)
template root(bu: var Builder, e: Expr): NodeRef =
  mixin c, env
  root(c, env, e, bu)

proc genAddr(c; env; e: Expr, bu): NodeRef =
  ## Constructs an address-of operation on `e`.
  case e.mode
  of emIndirect:
    e.n
  of emLvalue, emSym:
    let pt = env.newPtrType(e.typ)
    bu.build Addr(pt, ^e.n)
  of emValue:
    # happens when some rvalue expression appears where it shouldn't
    unreachable()

proc genArrayAddr(c; env; e: Expr, bu): NodeRef =
  ## Constructs an address-of operation producing a ptr-to-array for an
  ## array expression.
  let elem = env.types.headerFor(e.typ, Lowered).elem
  let pt = env.newPtrToArrayType(elem)
  bu.build Addr(pt, Path(elem, ^c.root(env, e, bu), 0))

proc pushSourceLoc(c; id: SourceId, bu): (uint32, TLineInfo) =
  ## Sets the source location `bu` uses when creating nodes to that associated
  ## with `id`, returning the previous state.
  result = bu.get()
  let info = c.prc.body.source[id].info
  if result[1] != info:
    # simple caching that should reduce the amount of slow lookup considerably
    let idx = info.fileIndex
    if idx == InvalidFileIdx:
      bu.update(0, unknownLineInfo)
    else:
      var loc = SourceLoc(line: info.line, column: uint16(info.col + 1))
      c.fullPaths.withValue idx, val:
        loc.file = val[]
      do:
        loc.file = c.module.put(toFullPath(c.graph.config, idx))
        c.fullPaths[idx] = loc.file
      # LitId 256 corresponds to index 0, 257 to 1, etc.
      let id = c.sourceLocs.getOrIncl(loc).uint32 - 255
      bu.update(id, info)

template useSourceLoc(c: var Context, info: SourceId, bu: var Builder) =
  ## For the remainder of the current scope, pushes the source location
  ## corresponding to `info` to be used as the source-loc info for CGIR trees.
  let prev = pushSourceLoc(c, info, bu)
  defer: bu.update(prev[0], prev[1])

template addStmt(stmts; bu; body: untyped) =
  let got = bu.build body
  # don't use as LHS directly to allow for `stmts` being modified in between
  stmts.add got

proc mangledName(g: ModuleGraph, s: PSym): string =
  ## Computes the mangled name for `s`, which is a name within the
  ## current compilation.
  # note: thanks to inlining/generics, even non-exported symbols need a
  # globally unique name, since they might end up in another module
  result = s.name.s.mangle
  result.add "__"
  result.add g.ifaces[s.itemId.module].uniqueName
  result.add "_"
  result.addInt s.itemId.item

proc symToName(c; s: PSym): StringId =
  ## Generates and caches the external name for `s`.
  c.names.withValue s.id, val:
    result = val[]
  do:
    # use the mangled name for dynlib symbols
    if s.extname.len > 0 and exfDynamicLib notin s.extFlags:
      result = c.module.put(s.extname)
    else:
      result = c.module.put(mangledName(c.graph, s))
    c.names[s.id] = result

proc computeAttribs(s: PSym): set[CgProcAttrib] =
  case s.typ.callConv
  of ccInline:
    result.incl Inline
  of ccNoInline:
    result.incl NoInline
  else:
    discard "nothing to do"

proc refToExtern(bu; c; s: PSym): NodeRef =
  ## Returns a CGIR reference to the external symbol `s`.
  assert exfNoDecl in s.extFlags
  let name = symToName(c, s)
  if exfHeader in s.extFlags:
    let str = getStr(c.graph.getLib(s.annex).path)
    bu.build Unknown(str, name)
  else:
    bu.build Unknown("", name)

proc access(c; env; prc: ProcedureId, bu): NodeRef =
  ## Returns a CGIR reference to the given procedure.
  let s = env[prc]
  if exfImportCompilerProc in s.extFlags:
    # redirect to the compilerproc
    let id = env.procedures.add(c.graph.getCompilerProc(s.name.s))
    access(c, env, id, bu)
  elif exfNoDecl in s.extFlags:
    refToExtern(bu, c, s)
  elif exfDynamicLib in s.extFlags:
    # dynlib procedures are really globals, storing a pointer to the
    # actual procedure
    # TODO: lower dynlib procedures much earlier, possibly in transf, maybe
    #       even in sem
    bu.build(^globalRef(symToName(c, s)))
  else:
    bu.build(^procRef(symToName(c, s)))

proc access(c; env; g: GlobalId, bu): NodeRef =
  ## Returns a CGIR reference to the given global.
  let s = env[g]
  if exfNoDecl in s.extFlags:
    refToExtern(bu, c, s)
  else:
    bu.build(^globalRef(symToName(c, s)))

proc access(c; env; cnst: ConstId, bu): NodeRef =
  ## Returns a CGIR reference to the given constant.
  let s = env[cnst]
  if exfNoDecl in s.extFlags:
    refToExtern(bu, c, s)
  else:
    bu.build(^globalRef(symToName(c, s)))

proc useCompilerProc(bu; c; env; name: string): NodeRef =
  let p = c.graph.getCompilerProc(name)
  let id = env.procedures.add(p)
  let typ = env.types.addSignature(p.typ)
  bu.build Use(typ, ^access(c, env, id, bu))

proc findType(env: TypeEnv, typ: TypeId, pos: int32): (TypeId, int) =
  let desc = env.headerFor(typ, Lowered)
  if desc.kind == tkImported:
    result = (typ, 0) # imported types must not use inheritance
  elif desc.fieldOffset(env) > pos:
    result = findType(env, desc.base(env), pos)
    result[1] += 1
  else:
    result = (typ, 0)

proc genAsgn(c; env; dest: Expr, src: NodeRef, bu): NodeRef =
  if dest.typ == VoidType:
    bu.build Drop(src)
  elif dest.mode == emIndirect:
    bu.build Store(^dest.n, src)
  elif dest.mode == emSym:
    bu.build Asgn(^dest.n, src)
  else:
    bu.build Asgn(*use(dest), src)

proc genAsgn(c; env; dest, src: Expr, bu): NodeRef =
  if env.types.headerFor(dest.typ, Lowered).kind == tkArray:
    # XXX: C code generator accommodation. In C, using expressions of array
    #      type on the left of an assignment is not possible
    bu.build Call(
      ^bu.useCompilerProc(c, env, "nimCopyMem"),
      PtrCast(PointerType, ^c.genAddr(env, dest, bu)),
      PtrCast(PointerType, ^c.genAddr(env, src, bu)),
      Sizeof(^env.types.sizeType, ^dest.typ))
  else:
    genAsgn(c, env, dest, bu.use(src), bu)

template asgn(bu: var Builder, dest: Expr, src: NodeRef): NodeRef =
  mixin c, env
  genAsgn(c, env, dest, src, bu)

template putInto(stmts; bu; dest: Expr, e: untyped) =
  let r = bu.build(e)
  stmts.add asgn(bu, dest, r)

proc rawFieldAccess(c; env: MirEnv; typ: TypeId, id: FieldId,
                    to: var seq[NodeRef], bu) =
  ## Ads the CGIR path indices to `to` for accessing the field with ID `id`
  ## in struct type `typ`.
  proc findField(c; env: TypeEnv, typ: TypeId, id: FieldId,
                 to: var seq[NodeRef], bu): bool =
    let desc = env.headerFor(typ, Lowered)
    var pos = 0
    block search:
      for (f, strf) in env.fields(desc):
        if isEmbedded(strf):
          # it's an embedded struct/union type
          if findField(c, env, strf.typ, id, to, bu):
            break search
        elif env.canonical(strf.typ) == VoidType:
          # void fields are dropped
          dec pos
        elif f == id:
          break search

        inc pos

      # not found
      return false

    if desc.kind == tkStruct and desc.base(env) != VoidType:
      # account for the base field (which is located at the start of the
      # struct)
      inc pos

    result = true
    to.add bu.build(pos)

  let start = to.len
  doAssert findField(c, env.types, typ, id, to, bu)
  # the indices were added in reverse; fix the order
  reverse(to.toOpenArray(start, to.high))

proc rawFieldAccess(c; env; typ: TypeId, pos: int32, to: var seq[NodeRef],
                    bu): TypeId =
  ## Adds the CGIR path indicies to `to` for accessthat make up an access of the `pos`-th field
  ## of `typ`. Returns the type of the field.
  let f = env.types.lookupField(typ, pos)
  case env.types.headerFor(typ, Lowered).kind
  of tkStruct:
    let (owner, depth) = findType(env.types, typ, pos)
    # add the parent field access first...
    for _ in 0..<depth:
      to.add bu.build(0)
    # then the main part:
    c.rawFieldAccess(env, owner, f, to, bu)
  of tkUnion:
    # simple case: the position is equal to the index
    to.add bu.build(pos)
  of tkImported:
    to.add bu.build(
      ExtField(^env.types[f].typ, ^env.types.name(env.types[f])))
  else:
    unreachable()
  result = env.types[f].typ

include compiler/backend/rtti

proc fieldAccess(c; env; e: Expr; pos: int32, bu): NodeRef =
  ## Builds an access of the field of `e` with position `pos`.
  var acc: seq[NodeRef]
  let typ = c.rawFieldAccess(env, e.typ, pos, acc, bu)
  bu.build Path(typ, *root(e), acc)

proc fieldAccessExpr(c; env; e: Expr; pos: int32, bu): Expr =
  ## Builds an access of the field of `e` with position `pos`.
  var acc: seq[NodeRef]
  let typ = c.rawFieldAccess(env, e.typ, pos, acc, bu)
  bu.buildLval typ, Path(typ, *root(e), acc)

proc genConstDefault(c; env; typ: TypeId, bu): NodeRef =
  ## Generates a constructor expression for constructing the default value
  ## of `typ`.
  proc aux(c; env; typ: TypeId, bu): Option[NodeRef] =
    let desc = env.types.headerFor(typ, Canonical)
    case desc.kind
    of tkStruct:
      proc struct(c; env; outer, curr, typ: TypeId,
                  elems, path: var seq[NodeRef], bu) {.nimcall.} =
        # `outer` is the outermost struct type
        # `curr` is the innermost non-embedded struct
        # `typ` is the struct type to process
        let start = path.len
        let desc = env.types.headerFor(typ, Lowered)
        # the RTTI header is part of the least-derived type
        let base = desc.base(env.types)
        if base != VoidType:
          path.add bu.build(0)
          struct(c, env, outer, base, base, elems, path, bu)
        elif hasRttiHeader(env.types, typ):
          path.add bu.build(0)
          elems.add bu.build do:
            FieldInit(path, *use(^c.getTypeInfoV2(env, env.types[outer], bu)))

        for (id, strf) in env.types.fields(desc):
          var got = none NodeRef
          if strf.isTagged:
            # initialize the default variant of the tagged union
            let branch = env.types.getBranch(curr, typ, id, Zero)
            path.shrink(start)
            if env.types[branch].isEmbedded:
              struct(c, env, outer, curr, env.types[branch].typ, elems,
                     path, bu)
            else:
              got = aux(c, env, strf.typ, bu)
          else:
            got = aux(c, env, strf.typ, bu)

          if got.isSome:
            path.shrink(start)
            c.rawFieldAccess(env, curr, id, path, bu)
            elems.add bu.build(FieldInit(path, ^got.unsafeGet))

      var elems, path: seq[NodeRef]
      struct(c, env, typ, typ, typ, elems, path, bu)
      if elems.len > 0:
        some bu.build(RecConstr(typ, elems))
      else:
        none NodeRef # zero initializing is fine
    of tkArray:
      # the initializer (if any) is the same for every array element
      let elem = aux(c, env, desc.elem, bu)
      if elem.isSome:
        var s = newSeq[NodeRef](desc.arrayLen(env.types))
        for it in s.mitems:
          it = elem.unsafeGet
        some bu.build(Constr(typ, s))
      else:
        none NodeRef # zero initializing is fine
    else:
      none NodeRef

  let got = aux(c, env, typ, bu)
  if got.isSome: got.unsafeGet
  else:          bu.build Constr(typ)

proc constToCgir(c; env; tree; n; bu): NodeRef =
  ## Translates a MIR constant expression to the corresponding CGIR
  ## constant expression.
  iterator args(tree; n): NodePosition =
    for it in tree.items(n, 0, ^1):
      yield tree.last(it)

  template recurse(n: NodePosition): NodeRef =
    c.constToCgir(env, tree, n, bu)

  let typ = tree[n].typ
  case tree[n].kind
  of mnkNilLit:
    if env.types.headerFor(typ, Canonical).kind == tkClosure:
      # the 'nil' is really a closure constructor
      bu.build Constr(typ, NilLit(), NilLit())
    else:
      bu.build NilLit()
  of mnkIntLit, mnkUIntLit:
    case env.types.headerFor(typ, Lowered).kind
    of tkBool:
      if env.getInt(tree[n].number) == 0:
        bu.build Value(BoolType, false)
      else:
        bu.build Value(BoolType, true)
    of tkInt, tkUInt, tkChar, tkImported:
      c.genInt(env, env.getInt(tree[n].number), typ, bu)
    of tkPointer, tkPtr:
      bu.build PtrCast(typ,
        ^c.genInt(env, env.getInt(tree[n].number), env.types.usizeType, bu))
    else:
      unreachable(env.types.headerFor(typ, Lowered).kind)
  of mnkFloatLit:
    c.genFloat(env, env.getFloat(tree[n].number), typ, bu)
  of mnkProcVal:
    checkProc(c, tree[n].info, env[tree[n].prc])
    if env.types.headerFor(typ, Canonical).kind == tkClosure:
      # TODO: this is too late. Lower the value earlier
      bu.build Constr(typ,
        PtrCast(^env.types.getFieldType(typ, 0),
          Addr(^env.types.add(env[tree[n].prc].typ),
            ^access(c, env, tree[n].prc, bu))),
        NilLit())
    else:
      bu.build Addr(typ, ^access(c, env, tree[n].prc, bu))
  of mnkTupleConstr:
    bu.build Constr(typ,
      ^collect(for it in tree.args(n): recurse(it)))
  of mnkArrayConstr:
    let desc = env.types.headerFor(typ, Canonical)
    case desc.kind
    of tkArray:
      bu.build Constr(typ,
        ^collect(for it in tree.args(n): recurse(it)))
    of tkOpenArray:
      # construct a constant array and have the openArray point to it
      let
        arr = env.types.newArray(tree[n].len, desc.elem)
        content = c.buildDatumExpr arr:
          Constr(arr, ^collect(for it in tree.args(n): recurse(it)))
      bu.build Constr(typ,
        PtrCast(^env.types.getFieldType(typ, 0),
          ^c.genArrayAddr(env, content, bu)),
        ^c.genInt(env, tree[n].len.int64, env.types.sizeType, bu))
    else:
      unreachable()
  of mnkClosureConstr:
    # needs a ptrcast for the procedure pointer (which can/should only be
    # a .nimcall)
    bu.build Constr(typ,
      PtrCast(^env.types.getFieldType(typ, 0),
        ^recurse(tree.last(tree.child(n, 0)))),
      NilLit())
  of mnkObjConstr:
    # go over the type's fields and:
    # 1. if the field has an explicit value supplied, use that
    # 2. if zero initialization is not enough and no explicit value is
    #    supplied, fill in the default value
    proc aux(c; env; tree; n; bu): NodeRef {.nimcall.} =
      # an inner procedure is used so that the closure environment isn't
      # unconditionally allocated in `constToCgir`
      var elems, path: seq[NodeRef]
      var preproc: Table[FieldId, NodePosition]
      let outer = env.types.canonical(tree[n].typ)

      for it in tree.subNodes(n):
        preproc[env.types.lookupField(outer, tree[it, 0].field)] =
          tree.last(tree.child(it, 1))

      proc traverse(c; env; curr, typ: TypeId, bu) =
        let start = path.len
        # imported types are treated as their underlying struct/union type when
        # inspecting their structure here
        let desc = env.types.headerFor(env.types.skip(typ), Lowered)
        if desc.kind == tkStruct:
          let base = desc.base(env.types)
          if base != VoidType:
            path.add bu.build(0)
            traverse(c, env, base, base, bu)
          elif hasRttiHeader(env.types, typ):
            path.add bu.build(0)
            elems.add bu.build do:
              FieldInit(path,
                *use(^c.getTypeInfoV2(env, env.types[outer], bu)))

        proc field(c; env; curr: TypeId, id: FieldId, strf: StructField, bu) =
          proc access(c; env; curr: TypeId; id: FieldId, bu) =
            if env.types.headerFor(curr, Lowered).kind == tkImported:
              path.add bu.build(
                ExtField(^env.types[id].typ, ^env.types.name(env.types[id])))
            else:
              c.rawFieldAccess(env, curr, id, path, bu)

          if strf.isEmbedded:
            traverse(c, env, curr, strf.typ, bu)
          elif id in preproc:
            # field has an explicit value
            c.access(env, curr, id, bu)
            elems.add bu.build do:
              FieldInit(path, ^recurse(preproc[id]))
          elif containsTypeHeaders(env.types, strf.typ):
            # zero-filling is not enough
            c.access(env, curr, id, bu)
            elems.add bu.build do:
              FieldInit(path, ^c.genConstDefault(env, strf.typ, bu))

        for (id, strf) in env.types.fields(desc):
          path.shrink(start)
          if strf.isTagged:
            let discr = env.types.lookupTag(desc, id)
            # the discriminator might have a non-zero value
            var val = Zero
            if discr in preproc:
              # values greater than high(int) are not possible, so `getInt` is
              # always valid
              val = toInt128(env.getInt(tree[preproc[discr]].number))

            # pick the branch corresponding to the disriminator's value
            let b = env.types.getBranch(curr, typ, id, val)
            field(c, env, curr, b, env.types[b], bu)
          else:
            field(c, env, curr, id, strf, bu)

      traverse(c, env, outer, outer, bu)
      bu.build RecConstr(outer, elems)

    aux(c, env, tree, n, bu)
  of mnkStrLit:
    let str {.cursor.} = env[tree[n].strVal]

    case env.types.headerFor(typ, Canonical).kind
    of tkCstring:
      # a cstring is a pointer to a raw character sequence followed by a NUL
      # terminator
      if str.len == 0:
        # an empty cstring is represented with a nil pointer
        bu.build NilLit()
      else:
        bu.build Value(typ, str)
    of tkString:
      # it's a NimSkull string (a length + payload pointer)
      if str.len > 0:
        let payloadTyp = newPayloadType(env, CharType, str.len + 1)
        let payload = c.buildDatumExpr payloadTyp:
          Constr(payloadTyp,
            ^c.genInt(env, str.len or StrLitFlag, env.types.sizeType, bu),
            Value(^env.types.getFieldType(payloadTyp, 1), str))

        bu.build Constr(typ,
          ^c.genInt(env, str.len, env.types.sizeType, bu),
          PtrCast(^env.types.getFieldType(typ, 1),
            ^c.genAddr(env, payload, bu)))
      else:
        # empty string
        bu.build Constr(typ,
          ^c.genInt(env, 0, env.types.sizeType, bu),
          NilLit())
    else:
      unreachable()
  of mnkSeqConstr:
    let elem = env.types.headerFor(typ, Canonical).elem
    if tree.len(n) > 0:
      # only create a payload for non-empty seqs
      let payloadTyp = newPayloadType(env, elem, tree.len(n))
      let payload = c.buildDatumExpr payloadTyp:
        Constr(payloadTyp,
          ^c.genInt(env, tree.len(n) or StrLitFlag, env.types.sizeType, bu),
          Constr(^env.types.getFieldType(payloadTyp, 1),
            ^collect(for it in tree.args(n): recurse(it))))

      bu.build Constr(typ,
        ^c.genInt(env, tree.len(n), env.types.sizeType, bu),
        PtrCast(^env.types.getFieldType(typ, 1),
          ^c.genAddr(env, payload, bu)))
    else:
      bu.build Constr(typ,
        ^c.genInt(env, 0, env.types.sizeType, bu),
        NilLit())
  of mnkSetConstr:
    let first = castToUInt64(c.graph.config.firstOrd(env.types[typ]))
    let desc = env.types.headerFor(typ, Lowered)

    # evaluate the constant set first:
    var bitset: TBitSet
    bitset.bitSetInit(desc.size(env.types).int)

    for it in tree.items(n, 0, ^1):
      template elem(n: MirNode): int64 =
        # use unsigned arithmetic to handle both unsigned/signed values
        cast[int64](env.getUInt(n.number) - first)

      if tree[it].kind == mnkRange:
        bitSetInclRange(bitset, elem(tree[it, 0]) .. elem(tree[it, 1]))
      else:
        bitSetIncl(bitset, elem(tree[it]))

    if bitset.len > 8:
      # the set is implemented as an array
      bu.build Constr(typ,
        ^collect(
          for it in bitset.items:
            c.genInt(env, it.int, UInt8Type, bu)))
    else:
      # the set fits into an integer
      var value = 0'i64
      for i, it in bitset.pairs:
        value = value or (int64(it) shl (i * 8))
      c.genInt(env, value, typ, bu)
  else:
    unreachable(tree[n].kind)

proc genConv(c; env; val: Expr, to: TypeId, bu): NodeRef =
  ## Builds an unchecked C-style conversion/cast of `val` to `to`.
  # TODO: split the procedure into `genConv` (build a NimSkull conversion) and
  #       `genCast` (build a C-style cast). This would make it clearer what the
  #       semantics of NimSkull conversions are (in terms of CGIR conversions)
  if sameType(env.types, val.typ, to):
    # same backend type -> no op
    return bu.use(val)

  let dst = env.types.headerFor(to, Lowered)
  var src = env.types.headerFor(val.typ, Lowered)
  var val = val

  # special case: the destination is an imported type
  if dst.kind == tkImported:
    return bu.build(Conv(to, *use(val)))

  if src.kind == tkImported:
    # convert the imported type to the type that's said to have the
    # same semantics
    # XXX: this is nonsense. Imported types need to be treated as fully opaque
    let newType = env.types.skip(src.elem)
    val = bu.buildExpr(newType, Conv(newType, *use(val)))
    src = env.types.headerFor(newType, Lowered)

    if sameType(env.types, newType, to):
      # now the types are the same; nothing else to do
      return bu.use(val)

  template convOp(kind: untyped): NodeRef =
    bu.build kind(to, *use(val))

  template sizedConv(narrow, widen: untyped): NodeRef =
    if dst.size(env.types) < src.size(env.types):
      convOp narrow
    elif dst.size(env.types) > src.size(env.types):
      convOp widen
    else: # same size conversion
      convOp Bitcast

  template boolOrCharConv(): NodeRef =
    ## Implements char/bool to int/uint conversion.
    if src.size(env.types) < dst.size(env.types):
      bu.build Zext(to,
        Bitcast(UInt8Type, *use(val)))
    else:
      bu.build Bitcast(to, *use(val))

  case dst.kind
  of tkUInt:
    case src.kind
    of tkInt:   sizedConv Trunc, Sext
    of tkUInt:  sizedConv Trunc, Zext
    of tkFloat: convOp FToU
    of tkBool, tkChar: boolOrCharConv()
    else:
      unreachable(src.kind)
  of tkInt:
    case src.kind
    of tkInt:   sizedConv Trunc, Sext
    of tkUInt:  sizedConv Trunc, Zext
    of tkFloat: convOp FToI
    of tkBool, tkChar: boolOrCharConv()
    else:
      unreachable(src.kind)
  of tkFloat:
    case src.kind
    of tkInt:   convOp IToF
    of tkUInt:  convOp UToF
    of tkFloat: sizedConv Demote, Promote
    of tkBool, tkChar:
      # TODO: chars being convertible to float is nonsense. Disallow these at
      #       the source language level and remove this case
      bu.build UToF(to,
        Bitcast(UInt8Type, *use(val)))
    else:
      unreachable()
  of tkBool:
    case src.kind
    of tkInt, tkUInt:
      bu.build Not(BoolType,
        Eq(BoolType, ^val.typ, *use(val), ^c.genInt(env, 0, val.typ, bu)))
    of tkFloat:
      bu.build Not(BoolType,
        Eq(BoolType, ^val.typ, *use(val), ^c.genFloat(env, 0, val.typ, bu)))
    of tkChar:
      convOp Bitcast
    else:
      unreachable()
  of tkChar:
    case src.kind
    of tkInt, tkUInt:
      if src.size(env.types) > 1:
        bu.build Bitcast(to, Trunc(UInt8Type, *use(val)))
      else:
        convOp Bitcast
    of tkFloat:
      bu.build Bitcast(to, FToU(UInt8Type, *use(val)))
    of tkBool:
      convOp Bitcast
    else:
      unreachable()
  of tkPtr, tkPointer, tkCstring:
    # a conversion between pointer types
    convOp PtrCast
  else:
    unreachable(dst.kind)

proc newLabel(c: var ProcContext): LabelId =
  result = LabelId(c.nextLabel)
  inc c.nextLabel

proc newTempName(c): StringId =
  let id = c.prc.nextLocal
  inc c.prc.nextLocal
  c.module.put("_" & $id)

proc newTemp(c; env: MirEnv; typ: TypeId, stmts; bu): Expr =
  assert typ != VoidType
  let name = newTempName(c)
  stmts.addStmt bu, Def(0, 0, typ, ^localRef(name))
  result = Expr(mode: emSym, typ: typ, n: bu.build ^localRef(name))

proc exit(bu; target: MirNode): NodeRef =
  case target.kind
  of mnkUnwind: bu.build Unwind()
  of mnkLabel:  bu.build ^labelRef(target.label)
  else:         unreachable()

proc genGotoExit(c; env; target: MirNode, bu): NodeRef {.nimcall.} =
  if target.kind == mnkUnwind:
    bu.build Break(^labelRef(c.prc.unwindLabel))
  else:
    bu.build Break(^labelRef(target.label))

proc valueToCgir(c; env; tree; n; bu): Expr

proc gen(c; env; tree; n; bu): Expr =
  valueToCgir(c, env, tree, n, bu)

proc pathElemsToCgir(c; env; tree; n; bu): seq[NodeRef] =
  ## Translates a MIR path to the constituents of a CGIR path expression.
  template recurse(n: NodePosition): seq[NodeRef] =
    pathElemsToCgir(c, env, tree, n, bu)

  template value(n: NodePosition): NodeRef =
    bu.use c.valueToCgir(env, tree, n, bu)

  case tree[n].kind
  of mnkLocal, mnkTemp, mnkAlias, mnkGlobal, mnkConst, mnkParam,
     mnkDeref, mnkDerefView:
    result = @[bu.root(c.valueToCgir(env, tree, n, bu))]
  of mnkPathPos:
    result = recurse(tree.child(n, 0))
    result.add bu.build(^int(tree[n, 1].imm))
  of mnkPathNamed:
    result = recurse(tree.child(n, 0))
    discard c.rawFieldAccess(env, tree[n, 0].typ, tree[n, 1].field, result, bu)
  of mnkPathVariant:
    # a no-op by itself
    result = recurse(tree.child(n, 0))
  of mnkPathArray:
    let typ = tree[n, 0].typ
    let desc = env.types.headerFor(typ, Canonical)
    var index = value(tree.child(n, 1))
    if env.types.headerFor(tree[n, 1].typ, Lowered).kind in {tkBool, tkChar}:
      # turn into an integer first
      index = bu.build Bitcast(UInt8Type, index)

    case desc.kind
    of tkArray:
      result = recurse(tree.child(n, 0))
      let bias = c.graph.config.firstOrd(env.types[tree[n, 0].typ])
      if bias != Zero:
        # make the index' range start at zero
        let val = castToInt64(bias)
        # ^^ using a cast makes sure index ranges outside the int64 range work
        result.add(bu.build Sub(^tree[n, 1].typ,
          index,
          ^c.genInt(env, val, tree[n, 1].typ, bu)))
      else:
        result.add index
    of tkUncheckedArray:
      if tree[n, 0].kind in {mnkDeref, mnkDerefView}:
        # in the CGIR, a pointer-to-unchecked-array may be used as a path root
        # directly, without an explicit dereference
        result = @[value(tree.child(tree.child(n, 0), 0))]
      else:
        result = recurse(tree.child(n, 0))
      result.add index
    of tkCstring:
      # indexing into a cstring yields a NimSkull `char` in the source
      # language, but down here it yields a C char. ptrcast the cstring first
      # TODO: implement this target-specific behaviour in `system`, not down
      #       here
      let arr = env.newPtrToArrayType(CharType)
      result = @[bu.build PtrCast(arr, ^value(tree.child(n, 0)))]
      result.add index
    of tkSeq, tkString:
      result = @[
        bu.build Path(^payloadPtrType(env.types, typ),
          ^recurse(tree.child(n, 0)),
          1),
        bu.build 1,
        index
      ]
    of tkOpenArray:
      result = @[
        bu.build Path(^env.types.getFieldType(typ, 0),
          ^recurse(tree.child(n, 0)),
          0),
        index
      ]
    else:
      unreachable()
  of mnkPathConv:
    let a = tree[n].typ
    let b = tree[n, 0].typ
    if sameType(env.types, a, b):
      # must be a 'distinct' conversion, which is a no-op
      result = recurse(tree.child(n, 0))
    else:
      assert isObject(env.types, a)
      # either an object up- or down-conversion
      let diff = inheritanceDiff(env.types[a].skipTypes(skipPtrs),
                                 env.types[b].skipTypes(skipPtrs))
      if diff < 0:
        # it's an up conversion
        result = recurse(tree.child(n, 0))
        for _ in diff..<0:
          result.add bu.build(0)
      else:
        # it must be a down conversion
        assert diff > 0
        # take the address and cast it to the target type
        let dstp = env.newPtrType(a)
        result = @[bu.build do:
          PtrCast(dstp,
            ^c.genAddr(env, c.gen(env, tree, tree.child(n, 0), bu), bu))]
  else:
    unreachable(tree[n].kind)

proc pathToCgir(c; env; tree; n; bu): Expr =
  ## Translates the MIR path expression at `n` to a CGIR lvalue expression.
  let got = pathElemsToCgir(c, env, tree, n, bu)
  if got.len == 1:
    # this can only be a down conversion
    Expr(mode: emIndirect, typ: tree[n].typ, n: got[0])
  else:
    Expr(mode: emLvalue, typ: tree[n].typ,
         n: bu.build(Path(^tree[n].typ, got)))

proc isSimpleConst(env: TypeEnv, typ: TypeId): bool =
  env.headerFor(typ, Lowered).kind in {tkInt, tkUInt, tkPointer, tkPtr}

proc valueToCgir(c; env; tree; n; bu): Expr =
  ## Translates a MIR value expression to the analogous CGIR expression.
  template recurse(n: NodePosition): Expr =
    c.valueToCgir(env, tree, n, bu)

  template expr(m: ExprMode, e: untyped): Expr =
    Expr(mode: m, typ: typ, n: bu.build(e))

  c.useSourceLoc(tree[n].info, bu)

  let typ = tree[n].typ
  case tree[n].kind
  of mnkIntLit, mnkUIntLit, mnkFloatLit:
    expr emValue, ^c.constToCgir(env, tree, n, bu)
  of mnkProcVal:
    let s = env[tree[n].prc]
    checkProc(c, tree[n].info, s)
    if exfDynamicLib in s.extFlags:
      # the entity refers to a global storing the address
      expr emSym, ^access(c, env, tree[n].prc, bu)
    else:
      var real = typ
      if env.types.headerFor(real, Canonical).kind == tkClosure:
        # .closure proc types are translated to closure types by default,
        # but here we're intrested in the pointer type
        real = env.types.getFieldType(real, 0)
      expr emValue, Addr(real,
        ^access(c, env, tree[n].prc, bu))
  of mnkNilLit:
    if env.types.headerFor(typ, Canonical).kind == tkClosure:
      # the 'nil' represents the "none" closure (a tuple), for which the
      # construction code cannot be emitted here. Use a single-use datum
      # TODO: lower nil closure values at an earlier stage
      let datum = c.buildDatum:
        Constr(typ, NilLit(), NilLit())
      expr emValue, Use(typ, ^datumRef(datum))
    else:
      expr emValue, NilLit()
  of mnkTemp, mnkLocal, mnkParam, mnkAlias:
    let (e, mode) = c.prc.accessors[tree[n].local]
    expr mode, ^e
  of mnkGlobal:
    let s = env[tree[n].global]
    if exfDynamicLib in s.extFlags:
      # the global stores a pointer to the actual location
      let pt = env.newPtrType(typ)
      expr emIndirect, Use(pt, ^c.access(env, tree[n].global, bu))
    else:
      expr emSym, ^access(c, env, tree[n].global, bu)
  of mnkConst:
    let id = tree[n].cnst
    if isSimpleConst(env.types, typ):
      # simple constants are inlined at the usage site
      let da = env.dataFor(id)
      expr emValue, ^c.constToCgir(env, env[da], NodePosition(0), bu)
    elif isAnon(id):
      if extract(id) notin c.dataMap:
        # add the datum
        var dbu = initBuilder()
        let got = c.constToCgir(env, env[env.dataFor(id)], NodePosition(0), dbu)
        c.dataMap[extract(id)] = c.addDatum(dbu, got)

      expr emSym, ^datumRef(c.dataMap[extract(id)])
    else:
      expr emSym, ^access(c, env, id, bu)
  of mnkDeref, mnkDerefView:
    expr emIndirect, ^bu.use(recurse(tree.child(n, 0)))
  of mnkPathPos, mnkPathArray, mnkPathNamed:
    c.pathToCgir(env, tree, n, bu)
  of mnkPathVariant:
    # a no-op
    recurse(tree.child(n, 0))
  of mnkPathConv:
    # there are some lvalue conversion that aren't actually lvalue conversions
    let src = tree[n, 0].typ
    if sameType(env.types, typ, src):
      # must be a 'distinct' lvalue conversion; a no-op
      recurse(tree.child(n, 0))
    elif isObject(env.types, typ):
      c.pathToCgir(env, tree, n, bu)
    elif isPointerLike(env.types, typ):
      # up- or down-conversion of pointer. To keep the expression as an
      # lvalue, take the adress and cast that
      let pt = env.newPtrType(typ)
      expr emIndirect, PtrCast(pt,
        ^c.genAddr(env, recurse(tree.child(n, 0)), bu))
    else:
      # this only happens for bogus MIR code produced when converting a
      # 'distinct' type to an imported numeric type and both share the same
      # base. Emit an rvalue conversion and hope for the best
      # TODO: this is obviously wrong, but there's nothing we can do about it
      #       this far down. The source langauge needs to be fixed
      expr emValue, ^c.genConv(env, recurse(tree.child(n, 0)), typ, bu)
  of mnkStrLit:
    # can only be a cstring
    expr emValue, Value(typ, ^env[tree[n].strVal])
  of mnkAstLit:
    unreachable("literal ast not supported")
  of AllNodeKinds - LvalueExprKinds - LiteralDataNodes - {mnkProcVal}:
    unreachable($tree[n].kind)

proc stableId(typ: PType): ItemId =
  ## Returns the item ID of `typ`, making sure that at least the module is
  ## stable (i.e., related to the type definition's source position).
  case typ.kind
  of tyObject, tyEnum, tyDistinct:
    typ.itemId
  else:
    # structural types are unified and thus don't have a stable home module
    ItemId(module: int32(InvalidFileIdx), item: typ.itemId.item)

proc getTypeInfoV1(c; env; typ: PType, bu): Expr =
  ## Returns a pointer expression referring to the RTTIv1 object for `typ`.
  ## The RTTI object and storage are created first if they weren't already.
  var global: StringId
  let (hash, typ) = hashTypeForRttiV1(typ)
  c.rttiV1Map.withValue hash, val:
    global = val[]
  do:
    # RTTI may by cyclic, so add a mapping right away
    # XXX: unfortunately, `computeTypeName` cannot be used here, as 'range'
    #      types aren't part of the MIR type system. The sighash has to be
    #      used instead
    global = c.module.put("NTI" & genTypeInfoV1Prefix(typ) & $hash & "_")
    c.rttiV1Map[hash] = global
    c.rtti.add (global, stableId(typ))

    if c.rttiV1Type == VoidType:
      # the RTTI types are cached on first use
      c.rttiV1Type = env.types.add(c.graph.getCompilerProc("TNimType").typ)
      c.rttiV1NodeType = env.types.add(c.graph.getCompilerProc("TNimNode").typ)

    var bu = initBuilder()
    let got = bu.build GlobalDef(
      ^CgStorage.Const,
      0, # no custom alignment
      0, # no flags
      ^c.rttiV1Type,
      ^globalRef(global),
      ^genTypeInfoV1(c, env, typ, bu))
    c.module.globals[global] = c.module.ast.append(bu, got)

  let pt = env.newPtrType(c.rttiV1Type)
  bu.buildExpr pt, Addr(pt, ^globalRef(global))

proc getTypeInfoV2(c; env; typ: PType, bu): Expr =
  ## Returns a pointer expression referring to the RTTI global for `typ`.
  ## The RTTI data is created first if it wasn't already.
  var global: StringId
  let (hash, typ) = hashTypeForRttiV2(typ)
  c.rttiV2Map.withValue hash, val:
    global = val[]
  do:
    # RTTI may by cyclic, so add a mapping right away
    # XXX: unfortunately, `computeTypeName` cannot be used here, as 'distinct'
    #      and 'range' types aren't part of the MIR type system. The sighash
    #      has to be used instead
    global = c.module.put("NTIv2" & $hash & "_")
    c.rttiV2Map[hash] = global
    c.rtti.add (global, stableId(typ))

    if c.rttiV2Type == VoidType:
      # the RTTI types are cached on first use
      c.rttiV2Type = env.types.add(c.graph.getCompilerProc("TNimTypeV2").typ)

    var bu = initBuilder()
    let got = bu.build GlobalDef(
      ^CgStorage.Const,
      0, # no custom alignment
      0, # no flags
      ^c.rttiV2Type,
      ^globalRef(global),
      ^genTypeInfoV2(c, env, typ, bu))
    c.module.globals[global] = c.module.ast.append(bu, got)

  let pt = env.newPtrType(c.rttiV2Type)
  bu.buildExpr pt, Addr(pt, ^globalRef(global))

proc emitClear(c; env; dest: Expr, stmts; bu) =
  stmts.addStmt bu, Call(
    ^bu.useCompilerProc(c, env, "nimZeroMem"),
    PtrCast(PointerType, ^c.genAddr(env, dest, bu)),
    Sizeof(^env.types.sizeType, ^dest.typ))

proc emitDefault(c; env; dest: Expr, stmts; bu) =
  ## Emits an assignment for setting `dest` to its default value.
  let typ = dest.typ
  case env.types.headerFor(typ, Lowered).kind
  of tkChar, tkInt, tkUInt:
    stmts.putInto bu, dest, ^c.genInt(env, 0, typ, bu)
  of tkBool:
    stmts.putInto bu, dest, Value(BoolType, false)
  of tkFloat:
    stmts.putInto bu, dest, ^c.genFloat(env, 0.0, typ, bu)
  of tkRef, tkPtr, tkVar, tkLent, tkPointer, tkCstring:
    stmts.putInto bu, dest, NilLit()
  elif hasEmbeddedRttiHeaders(env.types, typ):
    # TODO: move everything related to RTTI headers out of MIR-to-CGIR
    #       translation
    let typ = env.types.canonical(typ)
    if typ notin c.defaults:
      let got = c.buildDatum(^c.genConstDefault(env, typ, bu))
      c.defaults[typ] = got

    # might need a blit copy, so use `genAsgn`
    stmts.add c.genAsgn(env, dest,
      Expr(mode: emSym, typ: typ, n: NodeRef(datumRef(c.defaults[typ]))),
      bu)
  elif hasRttiHeader(env.types, typ):
    # zero-fill the location and then initialize the type header
    c.emitClear(env, dest, stmts, bu)
    stmts.addStmt bu, Asgn(
      ^c.fieldAccess(env, dest, -1, bu),
      *use(^c.getTypeInfoV2(env, env.types[typ], bu)))
  else:
    # just zero-filling is enough
    c.emitClear(env, dest, stmts, bu)

proc genOf(c; env; tree; e: Expr, typ: TypeId; bu): NodeRef =
  bu.build Call(
    ^bu.useCompilerProc(c, env, "isObj"),
    ^c.fieldAccess(env, e, -1, bu),
    Value(CstringType, ^genTypeInfo2Name(env[typ])))

proc emitLength(c; env; dest, val: Expr, stmts, bu) =
  ## Emits a statement for storing the length of sequence-like `val` in `dest`.
  case env.types.headerFor(val.typ, Canonical).kind
  of tkSeq, tkString:
    stmts.putInto bu, dest, Path(^env.types.sizeType, *root(val), 0)
  of tkArray:
    let L = env.types.headerFor(val.typ, Lowered).arrayLen(env.types)
    stmts.putInto bu, dest, ^c.genInt(env, L, env.types.sizeType, bu)
  of tkOpenArray:
    stmts.putInto bu, dest, Path(^env.types.sizeType, *root(val), 1)
  of tkCstring:
    # generate ``if x.isNil: 0 else: nimCStrLen(x)``
    stmts.addStmt bu, If(
      Eq(BoolType, CstringType,
        *use(val),
        NilLit()),
      *asgn(dest, ^c.genInt(env, 0, env.types.sizeType, bu)),
      *asgn(dest,
        Call(
          ^bu.useCompilerProc(c, env, "nimCStrLen"),
          *use(val))))
  else:
    unreachable()

proc setElemToCgir(c; env; val: Expr; styp: TypeId, bu): NodeRef =
  ## Translates a MIR set element operand to a CGIR expresssion. Elements of
  ## sets whose range doesn't start at zero are first brough into a range
  ## starting at zero.
  assert env.types[styp].kind == tySet
  let first = c.graph.config.firstOrd(env.types[styp])
  let val =
    if first != Zero:
      # shift the operand's value so that its range starts at 0
      bu.buildExpr val.typ, Sub(^val.typ,
        *use(val),
        ^c.genInt(env, first.toInt, val.typ, bu))
    else:
      val

  if env.types.headerFor(styp, Lowered).kind == tkArray:
    # sets cannot have more than 2^16 elements, hence a uint16
    c.genConv(env, val, UInt16Type, bu)
  else:
    c.genConv(env, val, styp, bu)

proc setOpToCgir(c; env; tree; n; dest: Expr, stmts, bu) =
  ## Translates a binary MIR set operation to CGIR.
  let
    a = c.valueToCgir(env, tree, NodePosition(tree.argument(n, 0)), bu)
    # ^^ always a set
    b = c.valueToCgir(env, tree, NodePosition(tree.argument(n, 1)), bu)
    # ^^ for some operations a set, for others not
    m = tree[n, 1].magic
    typ  = tree[tree.argument(n, 0)].typ
    desc = env.types.headerFor(typ, Lowered)

  # sets with a number of elements <= 64 fit into unsigned integers. All
  # other sets are implemented as an array of small sets (i.e., sets with
  # 8 elements, fitting into a uint8)

  template takeAddr(e: Expr): NodeRef =
    c.genArrayAddr(env, e, bu)

  template elem(e: Expr): NodeRef =
    # watch out! Don't use the canonical set type, because then the set
    # range's start value cannot be looked up anymore
    c.setElemToCgir(env, e, typ, bu)

  template lenValue(): NodeRef =
    let len = desc.arrayLen(env.types)
    c.genInt(env, len, env.types.sizeType, bu)

  if desc.kind == tkArray:
    case m
    of mMulSet, mPlusSet, mMinusSet:
      const Ops = [mMulSet: "nimSetMul",
                   mPlusSet: "nimSetPlus",
                   mMinusSet: "nimSetMinus"]
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, Ops[m]),
        ^takeAddr(dest),
        ^takeAddr(a),
        ^takeAddr(b),
        ^lenValue())
    of mEqSet:
      let cintTyp = env.types.add(c.graph.getCompilerProc("nimCmpMem").typ[0])
      stmts.putInto bu, dest, Eq(BoolType, cintTyp,
        Call(
          ^bu.useCompilerProc(c, env, "nimCmpMem"),
          PtrCast(PointerType, ^takeAddr(a)),
          PtrCast(PointerType, ^takeAddr(b)),
          ^lenValue()),
        ^c.genInt(env, 0, cintTyp, bu))
    of mLeSet, mLtSet:
      const Ops = [mLeSet: "nimSetLe", mLtSet: "nimSetLt"]
      stmts.putInto bu, dest, Call(
        ^bu.useCompilerProc(c, env, Ops[m]),
        ^takeAddr(a),
        ^takeAddr(b),
        ^lenValue())
    of mIncl, mExcl:
      const Ops = [mIncl: "nimSetIncl", mExcl: "nimSetExcl"]
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, Ops[m]),
        ^takeAddr(a),
        ^elem(b))
    of mInSet:
      stmts.putInto bu, dest, Call(
        ^bu.useCompilerProc(c, env, "nimSetIn"),
        ^takeAddr(a),
        ^elem(b))
    else:
      unreachable()
  else:
    case m
    of mMulSet:
      stmts.putInto bu, dest, BitAnd(typ, *use(a), *use(b))
    of mPlusSet:
      stmts.putInto bu, dest, BitOr(typ, *use(a), *use(b))
    of mMinusSet:
      stmts.putInto bu, dest, BitAnd(typ, *use(a), BitNot(typ, *use(b)))
    of mEqSet:
      stmts.putInto bu, dest, Eq(BoolType, typ, *use(a), *use(b))
    of mLtSet:
      # generate ``((a and not b) == 0) and (a != b)``
      stmts.addStmt bu, If(
        Eq(BoolType, typ,
          BitAnd(typ, *use(a), BitNot(typ, *use(b))),
          ^c.genInt(env, 0, typ, bu)),
        *asgn(dest, Not(BoolType, Eq(BoolType, typ, *use(a), *use(b)))),
        *asgn(dest, Value(BoolType, false)))
    of mLeSet:
      # generate ``(a and not b) == 0``
      stmts.putInto bu, dest, Eq(BoolType, typ,
        BitAnd(typ, *use(a), BitNot(typ, *use(b))),
        ^c.genInt(env, 0, typ, bu))
    of mIncl:
      # generate ``dest = dest or (1 shl elem)``
      stmts.putInto bu, a, BitOr(typ,
        *use(a),
        Shl(typ, ^c.genInt(env, 1, typ, bu), ^elem(b)))
    of mExcl:
      # generate ``dest = dest and not (1 shl elem)``
      stmts.putInto bu, a, BitAnd(typ,
        *use(a),
        BitNot(typ, Shl(typ, ^c.genInt(env, 1, typ, bu), ^elem(b))))
    of mInSet:
      # generate ``(set bitand (1 shl elem)) != 0``
      stmts.putInto bu, dest, Not(BoolType,
        Eq(BoolType, typ,
          BitAnd(typ,
            *use(a),
            Shl(typ, ^c.genInt(env, 1, typ, bu), ^elem(b))),
          ^c.genInt(env, 0, typ, bu)))
    else:
      unreachable()

proc shiftRhsToCgir(c; env; tree; n; typ: TypeId, bu): NodeRef =
  ## Translates the RHS operand of a shift operation. MIR shifts may use
  ## differing types for the left- and right-hand side operands, but CGIR
  ## shifts may not.
  var src = c.valueToCgir(env, tree, n, bu)
  src = bu.buildExpr(typ, ^c.genConv(env, src, typ, bu))
  # TODO: make the behaviour well-defined for all distance operands, e.g., by
  #       using `(distance % bitwidth)`
  bu.use(src)

proc magicToCgir(c; env; tree; n; dest: Expr, stmts, bu) =
  ## Translates a MIR magic call to the corresponding CGIR statement(s),
  ## lowering the magic as appropriate.
  template value(n: OpValue): Expr =
    c.valueToCgir(env, tree, NodePosition(n), bu)

  # some shorthand templates for common expressions
  template argp(pos: int): NodePosition =
    NodePosition tree.argument(n, pos)
  template argt(pos: int): TypeId =
    tree[argp(pos)].typ
  template arge(pos: int): Expr =
    c.valueToCgir(env, tree, argp(pos), bu)
  template arg(pos: int): NodeRef =
    bu.use arge(pos)

  template root(e: Expr): NodeRef =
    bu.root e
  template addrOp(e: Expr): NodeRef =
    c.genAddr(env, e, bu)

  template wrapAsgn(e: untyped) =
    stmts.putInto bu, dest, e

  case tree[n, 1].magic
  of mNot:
    wrapAsgn Not(BoolType, ^arg(0))
  of mLtI, mLtF64, mLtEnum, mLtU, mLtPtr:
    wrapAsgn Lt(BoolType, ^argt(0), ^arg(0), ^arg(1))
  of mLeI, mLeF64, mLeEnum, mLeU, mLePtr:
    wrapAsgn Le(BoolType, ^argt(0), ^arg(0), ^arg(1))
  of mEqI, mEqF64, mEqEnum, mEqRef, mEqCh, mEqB:
    wrapAsgn Eq(BoolType, ^argt(0), ^arg(0), ^arg(1))
  of mLtCh:
    wrapAsgn Lt(BoolType, UInt8Type,
      Bitcast(UInt8Type, ^arg(0)),
      Bitcast(UInt8Type, ^arg(1)))
  of mLeCh:
    wrapAsgn Le(BoolType, UInt8Type,
      Bitcast(UInt8Type, ^arg(0)),
      Bitcast(UInt8Type, ^arg(1)))
  of mEqProc:
    let typ = argt(0)
    if env.types.headerFor(typ, Lowered).kind == tkPtr:
      # simple pointer equality suffices
      wrapAsgn Eq(BoolType, typ, ^arg(0), ^arg(1))
    else:
      # both the procedure pointer and environment pointer need to be
      # compared (shallow equality)
      let a = arge(0)
      let b = arge(1)
      let at = env.types.getFieldType(typ, 0)
      let bt = env.types.getFieldType(typ, 1)
      stmts.addStmt bu, If(
        Eq(BoolType, at,
          Path(at, *root(a), 0),
          Path(at, *root(b), 0)),
        *asgn(dest,
          Eq(BoolType, bt,
            Path(bt, *root(a), 1),
            Path(bt, *root(b), 1))),
        *asgn(dest, Value(BoolType, false)))
  of mIsNil:
    let arg = arge(0)
    case env.types.headerFor(arg.typ, Canonical).kind
    of tkClosure:
      let p = env.types.getFieldType(arg.typ, 0)
      wrapAsgn Eq(BoolType, p,
        Path(p, *root(arg), 0),
        NilLit())
    else:
      # must be a pointer-like type
      wrapAsgn Eq(BoolType, ^arg.typ, *use(arg), NilLit())
  of mXor:
    # booleans cannot be used with bitxor directly; bitcast to uint8 first
    wrapAsgn Not(BoolType,
      Eq(BoolType, UInt8Type,
        BitXor(UInt8Type,
          Bitcast(UInt8Type, ^arg(0)),
          Bitcast(UInt8Type, ^arg(1))),
        ^c.genInt(env, 0, UInt8Type, bu)))
  of mAddU, mSubU, mMulU, mDivU, mModU:
    const Map = [mAddU: cnkAdd, mSubU: cnkSub,
                 mMulU: cnkMul, mDivU: cnkDiv, mModU: cnkMod]
    wrapAsgn (Map[tree[n, 1].magic])(
      ^tree[n].typ, ^arg(0), ^arg(1))
  of mBitandI:
    wrapAsgn BitAnd(^tree[n].typ, ^arg(0), ^arg(1))
  of mBitxorI:
    wrapAsgn BitXor(^tree[n].typ, ^arg(0), ^arg(1))
  of mBitorI:
    wrapAsgn BitOr(^tree[n].typ, ^arg(0), ^arg(1))
  of mBitnotI:
    wrapAsgn BitNot(^tree[n].typ, ^arg(0))
  of mShlI:
    let typ = tree[n].typ
    if env.types.headerFor(typ, Lowered).kind == tkInt:
      # left-shifting is only allowed on unsigned integers. Bitcast to uint,
      # shift, then bitcast back
      let unsigned =
        case env.types.headerFor(typ, Lowered).size(env.types)
        of 1: UInt8Type
        of 2: UInt16Type
        of 4: UInt32Type
        of 8: UInt64Type
        else: unreachable()
      wrapAsgn Bitcast(typ,
        Shl(unsigned,
          Bitcast(unsigned, ^arg(0)),
          Bitcast(unsigned, ^c.shiftRhsToCgir(env, tree, argp(1), typ, bu))))
    else:
      wrapAsgn Shl(typ,
        ^arg(0),
        ^c.shiftRhsToCgir(env, tree, argp(1), typ, bu))
  of mAshrI, mShrI:
    let typ = tree[n].typ
    wrapAsgn Shr(typ,
      ^arg(0),
      ^c.shiftRhsToCgir(env, tree, argp(1), typ, bu))
  of mOrd, mChr:
    # it's a conversion, nothing more
    let input = arge(0)
    wrapAsgn ^c.genConv(env, input, tree[n].typ, bu)
  of mIncl, mExcl, mLtSet, mLeSet, mEqSet, mMinusSet, mPlusSet, mMulSet,
     mInSet:
    c.setOpToCgir(env, tree, n, dest, stmts, bu)
  of mCard:
    let a = arge(0)
    let desc = env.types.headerFor(a.typ, Lowered)
    if desc.kind == tkArray:
      wrapAsgn Call(
        ^bu.useCompilerProc(c, env, "cardSet"),
        ^c.genArrayAddr(env, a, bu),
        ^c.genInt(env, desc.size(env.types), env.types.sizeType, bu))
    elif desc.size(env.types) == 8:
      wrapAsgn Call(
        ^bu.useCompilerProc(c, env, "countBits64"),
        *use(a))
    elif desc.size(env.types) == 4:
      wrapAsgn Call(
        ^bu.useCompilerProc(c, env, "countBits32"),
        *use(a))
    else:
      # also use countBits32, but widen the operand first
      wrapAsgn Call(
        ^bu.useCompilerProc(c, env, "countBits32"),
        Zext(UInt32Type, *use(a)))
  of mDefault:
    c.emitDefault(env, dest, stmts, bu)
  of mMaxI, mMinI:
    let (arg1, arg2) =
      if tree[n, 1].magic == mMaxI:
        (0, 1)
      else:
        (1, 0)
    stmts.addStmt bu, If(
      Lt(BoolType, ^tree[n].typ, ^arg(arg1), ^arg(arg2)),
      *asgn(dest, ^arg(1)),
      *asgn(dest, ^arg(0)))
  of mSizeOf:
    let typ = argt(0)
    let desc = env.types.headerFor(typ, Canonical)
    if desc.size(env.types) >= 0:
      wrapAsgn:
        ^c.genInt(env, desc.size(env.types), env.types.sizeType, bu)
    else:
      wrapAsgn Sizeof(^env.types.sizeType, typ)
  of mAlignOf:
    let typ = argt(0)
    let desc = env.types.headerFor(typ, Canonical)
    if desc.size(env.types) >= 0:
      wrapAsgn ^c.genInt(env, desc.align, tree[n].typ, bu)
    else:
      wrapAsgn Alignof(^env.types.sizeType, typ)
  of mOffsetOf:
    let
      typ    = argt(0)
      field  = env.getInt(tree[argp(1)].number).int32
    var path = newSeq[NodeRef]()
    discard c.rawFieldAccess(env, typ, field, path, bu)
    wrapAsgn Offsetof(^env.types.sizeType, typ, path)
  of mEqStr:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "eqStrings"),
      ^arg(0),
      ^arg(1))
  of mLeStr:
    wrapAsgn Le(BoolType, ^env.types.sizeType,
      Call(
        ^bu.useCompilerProc(c, env, "cmpStrings"),
        ^arg(0),
        ^arg(1)),
      ^c.genInt(env, 0, env.types.sizeType, bu))
  of mLtStr:
    wrapAsgn Lt(BoolType, ^env.types.sizeType,
      Call(
        ^bu.useCompilerProc(c, env, "cmpStrings"),
        ^arg(0),
        ^arg(1)),
      ^c.genInt(env, 0, env.types.sizeType, bu))
  of mFinished:
    # the status is stored in the second (absolute) field of the env object;
    # load it and test whether the value is < 0
    # TODO: make all iterator envs inherit from a base type in which the
    #       status field is stored, so that the awkward pointer casting here
    #       can be removed
    let pt = env.newPtrToArrayType(env.types.sizeType)
    wrapAsgn Lt(BoolType, ^env.types.sizeType,
      Path(^env.types.sizeType,
        PtrCast(pt,
          Path(^env.types.sizeType, ^root(arge(0)), 1)),
        1),
      ^c.genInt(env, 0, env.types.sizeType, bu))
  of mCopyInternal:
    # the internal part of an object is the RTTI pointer stored in the hidden
    # field
    stmts.addStmt bu, Asgn(
      ^c.fieldAccess(env, arge(0), -1, bu),
      ^c.fieldAccess(env, arge(1), -1, bu))
  of mArrToSeq:
    let
      seqType  = tree[n].typ
      arg      = argt(0)
      elem     = env.types.headerFor(arg, Canonical).elem
      elemDesc = env.types.headerFor(elem, Canonical)
      len      = env.types.headerFor(arg, Canonical).arrayLen(env.types)
    # emit the length initialization:
    stmts.addStmt bu, Asgn(
      Path(^env.types.sizeType, *root(dest), 0),
      ^c.genInt(env, len, env.types.sizeType, bu))
    # emit the seq allocation:
    stmts.addStmt bu, Asgn(
      Path(^payloadPtrType(env.types, seqType), *root(dest), 1),
      PtrCast(^payloadPtrType(env.types, seqType),
        Call(
          ^bu.useCompilerProc(c, env, "newSeqPayload"),
          ^c.genInt(env, len, env.types.sizeType, bu),
          ^c.genInt(env, elemDesc.size(env.types), env.types.sizeType, bu),
          ^c.genInt(env, elemDesc.align, env.types.sizeType, bu))))

    if len < 10:
      let src = bu.root(arge(0))
      for i in 0..<len:
        stmts.add c.genAsgn(env,
          bu.buildLval(elem,
            Path(elem,
              Path(^payloadPtrType(env.types, seqType), *root(dest), 1),
              1,
              i)),
          bu.buildLval(elem, Path(elem, src, i)),
          bu)
    else:
      # too many elements. Use a blit copy in order to not explode code size
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "nimCopyMem"),
        PtrCast(PointerType,
          Addr(^env.types.getFieldType(seqType, 1),
            Path(elem,
              Path(^payloadPtrType(env.types, seqType), *root(dest), 1),
              1,
              0))),
        PtrCast(PointerType, ^addrOp(arge(0))),
        Mul(^env.types.sizeType,
          ^c.genInt(env, len, env.types.sizeType, bu),
          Sizeof(^env.types.sizeType, elem)))
  of mSamePayload:
    let typ = payloadPtrType(env.types, argt(0))
    wrapAsgn Eq(BoolType, typ,
      Path(typ, ^root(arge(0)), 1),
      Path(typ, ^root(arge(1)), 1))
  of mLengthSeq, mLengthOpenArray, mLengthStr:
    c.emitLength(env, dest, arge(0), stmts, bu)
  of mHigh:
    let tmp = c.newTemp(env, env.types.sizeType, stmts, bu)
    c.emitLength(env, tmp, arge(0), stmts, bu)
    wrapAsgn Sub(^env.types.sizeType,
      *use(tmp),
      ^c.genInt(env, 1, env.types.sizeType, bu))
  of mSetLengthStr:
    stmts.addStmt bu, Call(
      ^bu.useCompilerProc(c, env, "setLengthStrV2"),
      ^addrOp(arge(0)),
      ^arg(1))
  of mNewString:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "mnewString"),
      ^arg(0))
  of mNewStringOfCap:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "rawNewString"),
      ^arg(0))
  of mBoolToStr:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "nimBoolToStr"),
      ^arg(0))
  of mCharToStr:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "nimCharToStr"),
      ^arg(0))
  of mCStrToStr:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "cstrToNimstr"),
      ^arg(0))
  of mStrToCStr:
    wrapAsgn Call(
      ^bu.useCompilerProc(c, env, "nimToCStringConv"),
      ^arg(0))
  of mAppendStrStr:
    # in theory, the appendStrStr magic supports being merged, but this never
    # happens in practice, meaning that the call expression only ever has
    # two parameters here
    let to = arge(0)
    stmts.addStmt bu, Call(
      ^bu.useCompilerProc(c, env, "prepareAdd"),
      ^addrOp(to),
      Add(^env.types.sizeType,
        Path(^env.types.sizeType, *root(to), 0),
        Path(^env.types.sizeType, ^root(arge(1)), 0)))

    stmts.addStmt bu, Call(
      ^bu.useCompilerProc(c, env, "appendString"),
      ^addrOp(to),
      ^arg(1))
  of mConStrStr:
    var
      temp  = c.newTemp(env, StringType, stmts, bu)
      len   = Expr(typ: VoidType)

    # compute the length expression:
    for (_, _, it) in tree.arguments(n):
      let val =
        if tree[it].typ == CharType:
          bu.buildExpr env.types.sizeType:
            ^c.genInt(env, 1, env.types.sizeType, bu)
        else:
          let L = c.newTemp(env, env.types.sizeType, stmts, bu)
          c.emitLength(env, L, value(it), stmts, bu)
          L

      if len.typ == VoidType:
        len = val
      else:
        len = bu.buildExpr env.types.sizeType:
          Add(^env.types.sizeType, *use(len), *use(val))

    stmts.putInto bu, temp, Call(
      ^bu.useCompilerProc(c, env, "rawNewString"),
      *use(len))

    # emit the append calls:
    for (_, _, it) in tree.arguments(n):
      if tree[it].typ == CharType:
        stmts.addStmt bu, Call(
          ^bu.useCompilerProc(c, env, "appendChar"),
          ^addrOp(temp),
          *use(^value(it)))
      else:
        stmts.addStmt bu, Call(
          ^bu.useCompilerProc(c, env, "appendString"),
          ^addrOp(temp),
          *use(^value(it)))

    stmts.add c.genAsgn(env, dest, temp, bu)
  of mAppendStrCh:
    stmts.addStmt bu, Call(
      ^bu.useCompilerProc(c, env, "nimAddCharV1"),
      ^addrOp(arge(0)),
      ^arg(1))
  of mNewSeqOfCap:
    let typ = tree[n].typ
    stmts.addStmt bu, Asgn(
      Path(^env.types.sizeType, *root(dest), 0),
      ^c.genInt(env, 0, env.types.sizeType, bu))
    stmts.addStmt bu, Asgn(
      Path(^payloadPtrType(env.types, typ), *root(dest), 1),
      PtrCast(^payloadPtrType(env.types, typ),
        Call(
          ^bu.useCompilerProc(c, env, "newSeqPayload"),
          ^arg(0),
          Sizeof(^env.types.sizeType,
            ^env.types.headerFor(typ, Canonical).elem),
          Alignof(^env.types.sizeType,
            ^env.types.headerFor(typ, Canonical).elem))))
  of mDestroy:
    let
      target = arge(0)
      typ    = target.typ
      ptrTyp = payloadPtrType(env.types, typ)

    # emit the following:
    #   if x.p != nil and (x.p.cap and NIM_STRLIT_FLAG) == 0:
    #     dealloc(x.p)
    let inner =
      case env.types.headerFor(typ, Canonical).kind
      of tkString:
        let dealloc =
          if optThreads in c.graph.config.globalOptions:
            "deallocShared"
          else:
            "dealloc"
        bu.build Call(
          ^bu.useCompilerProc(c, env, dealloc),
          PtrCast(PointerType,
            Path(ptrTyp, *root(target), 1)))
      of tkSeq:
        # selecting the shared variant is handled by the implementation
        bu.build Call(
          ^bu.useCompilerProc(c, env, "alignedDealloc"),
          PtrCast(PointerType,
            Path(ptrTyp, *root(target), 1)),
          Alignof(^env.types.sizeType,
            ^env.types.headerFor(typ, Canonical).elem))
      else:
        unreachable("destroy was not lowered?")

    stmts.addStmt bu, If(
      Not(BoolType,
        Eq(BoolType, ptrTyp,
          Path(ptrTyp, *root(target), 1),
          NilLit())),
      If(
        Eq(BoolType, ^env.types.sizeType,
          BitAnd(^env.types.sizeType,
            Path(^env.types.sizeType,
              Path(^payloadPtrType(env.types, typ), *root(target), 1),
              0),
            ^c.genInt(env, StrLitFlag, env.types.sizeType, bu)),
          ^c.genInt(env, 0, env.types.sizeType, bu)),
        inner))

  of mEcho:
    if tree.numArgs(n) == 0:
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "echoBinSafe"),
        NilLit(),
        ^c.genInt(env, 0, env.types.sizeType, bu))
    else:
      # emit the array construction:
      let tmp = c.newTemp(env, argt(0), stmts, bu)
      for i in 1..<tree.numArgs(n):
        stmts.addStmt bu, Asgn(
          Path(^argt(i), *root(tmp), ^(i - 1)),
          ^arg(i))

      # the procedure takes an openArray, which are passed as two parameters
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "echoBinSafe"),
        ^c.genArrayAddr(env, tmp, bu),
        ^c.genInt(env, tree.numArgs(n) - 1, env.types.sizeType, bu))
  of mOf:
    let e = arge(0)
    let typ = e.typ
    var check = argt(1)
    # the type is a typedesc, and the only way to retrieve the inner type is
    # by going through the PType
    check = env.types.add(env.types[check].skipTypes(abstractPtrs))

    case env.types.headerFor(typ, Canonical).kind
    of tkStruct:
      wrapAsgn:
        ^c.genOf(env, tree, e, check, bu)
    of tkRef, tkPtr:
      # emit ``if p == nil: false else: p[] of typ``
      let elem = env.types.headerFor(typ, Lowered).elem
      let base = bu.buildInd(elem, *use(e))
      stmts.addStmt bu, If(
        Eq(BoolType, typ, *use(e), NilLit()),
        *asgn(dest, Value(BoolType, false)),
        *asgn(dest, ^c.genOf(env, tree, base, check, bu)))
    of tkVar, tkLent:
      # the pointer cannot be nil
      let elem = env.types.headerFor(typ, Lowered).elem
      let base = bu.buildInd(elem, *use(e))
      wrapAsgn:
        ^c.genOf(env, tree, base, check, bu)
    else:
      unreachable()
  of mGetTypeInfo:
    let typ = env.types[argt(0)]
    wrapAsgn PtrCast(PointerType,
      *use(^c.getTypeInfoV1(env, typ, bu)))
  of mGetTypeInfoV2:
    let t = argt(0)
    var res =
      if tree[argp(0)].kind == mnkType or isFinal(env.types[t]):
        # static type information
        c.getTypeInfoV2(env, env.types[t], bu)
      else:
        # dynamic type information; query the object's type header
        makeExpr env.types.getFieldType(t, -1):
          c.fieldAccess(env, arge(0), -1, bu)

    if not sameType(env.types, tree[n].typ, res.typ):
      # TODO: remove this upstream accomodation. Either the result always has
      #       type ``ptr TNimTypeV2``, or ``pointer``
      res = bu.buildExpr(tree[n].typ, PtrCast(^tree[n].typ, *use(res)))

    stmts.add c.genAsgn(env, dest, res, bu)
  of mAsgnDynlibVar:
    let arg = argp(0)
    let dst =
      case tree[arg].kind
      of mnkProcVal: c.access(env, tree[arg].prc, bu)
      of mnkGlobal:  c.access(env, tree[arg].global, bu)
      else:          unreachable()
    stmts.addStmt bu, Asgn(
      dst,
      PtrCast(^argt(0), ^arg(1)))
  of mStoreParams:
    # a simple blit copy
    stmts.addStmt bu, Call(
      ^bu.useCompilerProc(c, env, "nimCopyMem"),
      ^arg(0),
      PtrCast(PointerType, ^addrOp(arge(1))),
      Sizeof(^env.types.sizeType, ^argt(1)))
  of mDeepCopy:
    if optEnableDeepCopy notin c.graph.config.globalOptions:
      # TODO: emit the error during semantic analysis, or, better yet,
      #       implement the deep copy dispatcher in `system`
      localReport(c.graph.config, c.prc.body.source[tree[n].info],
        reportSem rsemRequiresDeepCopyEnabled)

    let dst = arge(0)
    let typ = dst.typ
    case env.types.headerFor(typ, Canonical).kind
    of tkPtr, tkRef, tkClosure, tkStruct, tkArray, tkSeq, tkString:
      # TODO: only use deep copy when really required by the type (e.g., when
      #       it contains a ref)
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "genericDeepCopy"),
        ^addrOp(arge(0)),
        ^addrOp(arge(1)),
        ^bu.use(c.getTypeInfoV1(env, env[typ], bu)))
    of tkOpenArray:
      # the elements of the source are deep-copied to the destination array
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "genericDeepCopyOpenArray"),
        PtrCast(PointerType,
          Path(^env.types.getFieldType(typ, 0), *root(dst), 0)),
        PtrCast(PointerType,
          Path(^env.types.getFieldType(typ, 0), ^root(arge(1)), 0)),
        Path(^env.types.sizeType, *root(dst), 1),
        ^bu.use(c.getTypeInfoV1(env, env[typ], bu)))
    of tkPointer, tkChar, tkBool, tkCstring, tkInt, tkUInt, tkFloat, tkSet:
      # nothing to deep copy; a normal copy is enough
      stmts.add c.genAsgn(env, dst, arge(1), bu)
    else:
      unreachable(env.types.headerFor(typ, Canonical).kind)
  of mNLen..mNError, mStatic..mQuoteAst:
    # TODO: move this error reporting into semantic analysis
    localReport(c.graph.config, c.prc.body.source[tree[n].info],
      reportSem rsemConstExpressionExpected)
  else:
    unreachable(tree[n, 1].magic)

proc calleeToCgir(c; env; tree; n; bu): Expr =
  ## Translates a MIR callee expression to a CGIR expression, without
  ## performing closure unpacking.
  if tree[n].kind == mnkProc:
    let p = tree[n].prc
    let typ = env.types.addSignature(env[p].typ)
    if exfDynamicLib in env[p].extFlags:
      # an indirection is used
      Expr(typ: env.newPtrType(typ),
           mode: emSym,
           n: bu.build(^c.access(env, p, bu)))
    else:
      checkProc(c, tree[n].info, env[p])
      bu.buildExpr typ, Use(typ, ^c.access(env, p, bu))
  else:
    c.valueToCgir(env, tree, n, bu)

proc closureCallee(c; env; e: Expr; withEnv: bool, bu): NodeRef =
  ## Generates the CGIR for a callee expression.
  # a dynamic call
  var typ = env.types.getFieldType(e.typ, 0)
  # for closure invocations where passing the env is omitted, the
  # signature type needs to have no env parameter too
  if withEnv:
    # the .closure proc is the callee's type
    bu.build Path(typ, *root(e), 0)
  else:
    # the callee is really a .nimcall. The pointer has to be cast to the
    # correct dynamic type
    let desc = env.types.headerFor(e.typ, Canonical)
    # create the correct type first
    var pt = env.types.buildProc(tkProc, ccNimCall, desc.retType(env.types), bu):
      for (_, typ, flags) in params(env.types, desc):
        bu.addParam(flags, typ)
    pt = env.newPtrType(pt)

    bu.build PtrCast(pt, Path(typ, *root(e), 0))

proc argsToCgir(c; env; tree; n; callee: TypeId, stmts; bu): seq[NodeRef] =
  ## Translates a MIR argument list to a list of CGIR expressions (to be used
  ## in a CGIR call AST). May emit some statements when creating temporaries
  ## is necessary.
  const AutoConv = {tkInt, tkUInt, tkChar, tkPtr, tkPointer, tkImported, tkRef}

  var typ = callee
  # resolve pointer indirections
  if env.types.headerFor(typ, Lowered).kind == tkPtr:
    typ = env.types.headerFor(typ, Lowered).elem

  let desc = env.types.headerFor(typ, Canonical)
  var i = 0
  for kind, _, it in tree.arguments(n):
    # ignore compile-time-only arguments
    if tree[it].kind != mnkNone:
      let arg = c.valueToCgir(env, tree, NodePosition it, bu)
      # note: not all arguments to pass-by-reference parameters use the
      # ``mnkName`` mode
      if kind == mnkName or isPassByRef(env.types, desc, i):
        case arg.mode
        of emValue:
          # the expression doesn't have an address; introduce a temporary
          let tmp = c.newTemp(env, arg.typ, stmts, bu)
          stmts.add c.genAsgn(env, tmp, arg, bu)
          result.add c.genAddr(env, tmp, bu)
        of emLvalue, emSym, emIndirect:
          result.add c.genAddr(env, arg, bu)
      elif isOpenArray(env.types, arg.typ):
        # the procedure receives the pointer and length as two parameters
        result.add bu.build do:
          Path(^env.types.getFieldType(arg.typ, 0), *root(arg), 0)
        result.add bu.build do:
          Path(^env.types.sizeType, *root(arg), 1)
      elif i < numParams(desc):
        # HACK: various type errors make there way here. They're fixed-up by
        #       injecting unchecked conversions (in simple cases), but this is
        #       fundamentally wrong
        let param = paramType(desc, env.types, uint32 i)
        if not sameType(env.types, param, arg.typ) and
           env.types.headerFor(param, Canonical).kind in AutoConv and
           env.types.headerFor(arg.typ, Canonical).kind in AutoConv:
          result.add c.genConv(env, arg, param, bu)
        else:
          result.add bu.use(arg)
      else:
        # varargs arguments or some environment pointer
        result.add bu.use(arg)

    inc i

proc callToCgir(c; env; tree; n; dest: Expr, callee: NodeRef,
                args: seq[NodeRef], bu): NodeRef =
  ## Translates a MIR call to its CGIR equivalent, but without lowered
  ## exception handling.
  case tree[n].kind
  of mnkCall:
    if env.types.isVoidReturn(tree[n].typ):
      bu.build Call(callee, args)
    else:
      bu.build *asgn(dest, Call(callee, args))
  of mnkTailCall:
    bu.build TailCall(callee, args)
  of mnkCheckedCall:
    if capExceptions in c.caps:
      let exit = bu.exit(tree[tree.last(n)])
      if env.types.isVoidReturn(tree[n].typ):
        bu.build CheckedCall(callee, args, exit)
      elif dest.mode == emIndirect:
        # `dest` cannot be used directly. Go through a temporary
        let tmp = c.newTempName()
        bu.build StmtList(
          Def(0, 0, ^dest.typ, ^localRef(tmp)),
          CheckedCallAsgn(^localRef(tmp), callee, args, exit),
          *asgn(dest, Use(^dest.typ, ^localRef(tmp))))
      else:
        bu.build CheckedCallAsgn(*use(dest), callee, args, exit)
    else:
      # turn into a normal call + error flag test. The error flag test is
      # emitted separately
      if env.types.isVoidReturn(tree[n].typ):
        bu.build Call(callee, args)
      else:
        bu.build *asgn(dest, Call(callee, args))
  else:
    unreachable()

proc emitPostCall(c; env; tree; n; stmts; bu) =
  ## Emits the post-return handling for a MIR call.
  let callee = tree.callee(n)
  let isNoreturn = tree[callee].kind == mnkProc and
                   sfNoReturn in env[tree[callee].prc].flags
  case tree[n].kind
  of mnkCall:
    if isNoreturn:
      # there is no noreturn marker for CGIR procedures, so use unreachable to
      # mark the call as not returning
      stmts.addStmt bu, Unreachable()
  of mnkCheckedCall:
    if isNoreturn:
      if capExceptions in c.caps:
        stmts.addStmt bu, Unreachable()
      else:
        # no need to check the error flag for noreturn calls; they can only
        # exit via exceptional unwinding
        stmts.add c.genGotoExit(env, tree[tree.last(n)], bu)
    elif capExceptions notin c.caps:
      stmts.addStmt bu, If(
        Unlikely(*use(^c.prc.errorLocal)),
        ^c.genGotoExit(env, tree[tree.last(n)], bu))
  of mnkTailCall:
    discard "nothing to do"
  of AllNodeKinds - CallKinds:
    unreachable()

proc exprToCgir(c; env; tree; n; dest: Expr, stmts, bu) =
  ## Translates a MIR assignment RHS into an analogous CGIR assignment,
  ## lowering where appropriate.
  when defined(nimCompilerStacktraceHints):
    frameMsg(c.graph.config, c.prc.body.source[tree[n].info])

  template operand(n: NodePosition): Expr =
    c.valueToCgir(env, tree, n, bu)
  template value(n: NodePosition): NodeRef =
    bu.use operand(n)
  template root(n: NodePosition): NodeRef =
    bu.root operand(n)

  template takeAddr(n: NodePosition): NodeRef =
    c.genAddr(env, c.valueToCgir(env, tree, n, bu), bu)

  template wrapAsgn(body: untyped) =
    stmts.putInto bu, dest, body

  template asgn(dest, src: Expr) =
    stmts.add c.genAsgn(env, dest, src, bu)

  c.useSourceLoc(tree[n].info, bu)
  let typ = tree[n].typ
  case tree[n].kind
  of LvalueExprKinds, LiteralDataNodes, mnkProcVal:
    asgn dest, operand(n)
  of mnkConv, mnkStdConv:
    # the high-level MIR conversions are lowered into the more specific
    # operations of the target IL
    let input = c.valueToCgir(env, tree, tree.child(n, 0), bu)
    stmts.putInto bu, dest, ^c.genConv(env, input, typ, bu)
  of mnkCopy, mnkMove, mnkSink:
    asgn dest, operand(tree.child(n, 0))
  of mnkCall, mnkCheckedCall:
    let callee = tree.callee(n)
    if tree[callee].kind == mnkMagic:
      c.magicToCgir(env, tree, n, dest, stmts, bu)
    else:
      let cc = c.calleeToCgir(env, tree, tree.callee(n), bu)
      var args = c.argsToCgir(env, tree, n, cc.typ, stmts, bu)
      # XXX: C code generator accommodation: arrays are returned via an
      #      out parameter
      if env.types.headerFor(dest.typ, Lowered).kind == tkArray:
        # in-place return is possible
        args.add c.genAddr(env, dest, bu)
      # closures require special handling, as the dynamic callee might not have
      # an env parameter. If the closure's env value is non-nil, the dynamic
      # callee must have one, otherwise it must not
      if env.types.headerFor(cc.typ, Canonical).kind == tkClosure:
        let c1 = c.closureCallee(env, cc, false, bu)
        let c2 = c.closureCallee(env, cc, true, bu)
        let a = c.callToCgir(env, tree, n, dest, c1, args, bu)
        let envP = bu.build Path(PointerType, ^root(callee), 1)
        args.add envP
        stmts.addStmt bu, If(
          Eq(BoolType, PointerType, ^envP, NilLit()),
          a,
          ^c.callToCgir(env, tree, n, dest, c2, args, bu))
      else:
        stmts.add c.callToCgir(env, tree, n, dest, bu.use(cc), args, bu)
      c.emitPostCall(env, tree, n, stmts, bu)
  of mnkAddr, mnkView, mnkMutView:
    let desc = env.types.headerFor(tree[n, 0].typ, Lowered)
    if desc.kind == tkUncheckedArray:
      # cannot take the address of flexible struct members directly
      let inner = c.pathElemsToCgir(env, tree, tree.child(n, 0), bu)
      wrapAsgn Addr(typ, Path(^desc.elem, inner, 0))
    else:
      wrapAsgn ^takeAddr(tree.child(n, 0))
  of mnkAdd, mnkSub, mnkMul, mnkDiv, mnkModI:
    const Map = [mnkAdd: cnkAdd, mnkSub: cnkSub, mnkMul: cnkMul, mnkDiv: cnkDiv, mnkModI: cnkMod]
    wrapAsgn (Map[tree[n].kind])(
      typ,
      ^value(tree.child(n, 0)),
      ^value(tree.child(n, 1)))
  of mnkNeg:
    wrapAsgn Neg(typ, ^value(tree.child(n, 0)))
  of mnkObjConstr:
    c.emitClear(env, dest, stmts, bu)
    # note: initialization for the RTTI headers is handled at the MIR level
    for it in tree.items(n, 0, ^1):
      asgn c.fieldAccessExpr(env, dest, tree[it, 0].field, bu),
        operand(tree.last(tree.child(it, 1)))
  of mnkTupleConstr:
    # TODO: omit the zeromem if there's no padding in the tuple
    c.emitClear(env, dest, stmts, bu)
    var i = 0
    for it in tree.items(n, 0, ^1):
      asgn c.fieldAccessExpr(env, dest, int32 i, bu), operand(tree.last(it))
      inc i
  of mnkClosureConstr:
    # .nimcall procedure pointers are cast into .closure pointers
    # on construction
    let prc = tree.last(tree.child(n, 0))
    if sameType(env.types, tree[prc].typ, env.types.getFieldType(typ, 0)):
      # the pointer is a .closure pointer already -> no ptrcast needed
      stmts.addStmt bu, Asgn(
        ^c.fieldAccess(env, dest, 0, bu),
        ^value(prc))
    else:
      stmts.addStmt bu, Asgn(
        ^c.fieldAccess(env, dest, 0, bu),
        PtrCast(^env.types.getFieldType(typ, 0), ^value(prc)))
    stmts.addStmt bu, Asgn(
      ^c.fieldAccess(env, dest, 1, bu),
      PtrCast(PointerType,
        ^value(tree.last(tree.child(n, 1)))))
  of mnkArrayConstr:
    # arrays never have padding between elements (it's part of the elements
    # themselves), hence no zeroMem
    let elem = env.types.headerFor(typ, Canonical).elem
    var i = 0
    for it in tree.items(n, 0, ^1):
      asgn bu.buildLval(elem, Path(elem, *root(dest), i)),
        operand(tree.last(it))
      inc i
  of mnkSeqConstr:
    let
      elem = env.types.headerFor(typ, Canonical).elem
      desc = env.types.headerFor(elem, Canonical)
      payloadPtrTy = payloadPtrType(env.types, typ)

    # length initialization:
    stmts.addStmt bu, Asgn(
      Path(^env.types.sizeType, *root(dest), 0),
      ^c.genInt(env, tree.len(n), env.types.sizeType, bu))

    # payload initialization:
    stmts.addStmt bu, Asgn(
      Path(payloadPtrTy, *root(dest), 1),
      PtrCast(payloadPtrTy,
        Call(
          ^bu.useCompilerProc(c, env, "newSeqPayload"),
          ^c.genInt(env, tree.len(n), env.types.sizeType, bu),
          ^c.genInt(env, desc.size(env.types), env.types.sizeType, bu),
          ^c.genInt(env, desc.align, env.types.sizeType, bu))))

    # element initialization:
    var i = 0
    for it in tree.items(n, 0, ^1):
      asgn bu.buildLval(elem,
          Path(elem,
            Path(payloadPtrTy, *root(dest), 1),
            1,
            i)),
        operand(tree.child(it, 0))
      inc i

  of mnkToMutSlice, mnkToSlice:
    let
      arg      = c.valueToCgir(env, tree, tree.child(n, 0), bu)
      argTyp   = arg.typ
      elem     = env.types.headerFor(typ, Canonical).elem
      ptrToArr = env.types.getFieldType(typ, 0)

    let startExpr = makeExpr env.types.sizeType:
      if tree[n].len == 1:
        c.genInt(env, 0, env.types.sizeType, bu)
      elif env.types.headerFor(argTyp, Lowered).kind == tkArray:
        let bias = c.graph.config.firstOrd(env.types[argTyp])
        if bias == Zero:
          value tree.child(n, 1)
        else:
          bu.build Sub(^env.types.sizeType,
            ^value(tree.child(n, 1)),
            ^c.genInt(env, castToInt64(bias), env.types.sizeType, bu))
      else:
        value tree.child(n, 1)

    var dataExpr =
      case env.types.headerFor(argTyp, Canonical).kind
      of tkCstring:
        bu.buildExpr ptrToArr, Addr(ptrToArr,
          Path(elem,
            PtrCast(ptrToArr, *use(arg)),
            *use(startExpr)))
      of tkPtr:
        # can only be a pointer to an unchecked array
        bu.buildExpr argTyp, Addr(argTyp,
          Path(elem, *root(arg), *use(startExpr)))
      of tkArray:
        # toOpenArrayByte doesn't accept arrays, so `ptrToArr` is guaranteed to
        # be correct already
        bu.buildExpr ptrToArr, Addr(ptrToArr,
          Path(elem, *root(arg), *use(startExpr)))
      of tkSeq, tkString:
        let elemTyp = seqElemType(env.types, argTyp)
        let elemPtr = env.newPtrToArrayType(elemTyp)
        bu.buildExpr elemPtr, Addr(elemPtr,
          Path(elemTyp,
            Path(^payloadPtrType(env.types, argTyp),
              *root(arg),
              1),
            1,
            *use(startExpr)))
      of tkOpenArray:
        let elemPtr = env.types.getFieldType(argTyp, 0)
        bu.buildExpr elemPtr, Addr(elemPtr,
          Path(elem,
            Path(elemPtr, *root(arg), 0),
            *use(startExpr)))
      else:
        unreachable()

    if not sameType(env.types, dataExpr.typ, ptrToArr):
      # this happens for `toOpenArrayByte`; cast the pointer
      dataExpr = bu.buildExpr ptrToArr:
        PtrCast(ptrToArr, *use(dataExpr))

    # else part: if the requested length is zero (or less), an empty slice
    # is created
    let els = bu.build StmtList(
      Asgn(
        Path(ptrToArr, *root(dest), 0),
        NilLit()),
      Asgn(
        Path(^env.types.sizeType, *root(dest), 1),
        ^c.genInt(env, 0, env.types.sizeType, bu)))

    if tree[n].len == 1:
      let tmp = c.newTemp(env, env.types.sizeType, stmts, bu)
      c.emitLength(env, tmp, arg, stmts, bu)
      stmts.addStmt bu, If(
        Lt(BoolType, ^env.types.sizeType,
          ^c.genInt(env, 0, env.types.sizeType, bu),
          *use(tmp)),
        StmtList(
          Asgn(
            Path(ptrToArr, *root(dest), 0),
            *use(dataExpr)),
          Asgn(
            Path(^env.types.sizeType, *root(dest), 1),
            *use(tmp))),
        els)
    else:
      stmts.addStmt bu, If(
        Le(BoolType, ^env.types.sizeType,
          ^value(tree.child(n, 1)),
          ^value(tree.child(n, 2))),
        StmtList(
          Asgn(
            Path(ptrToArr, *root(dest), 0),
            *use(dataExpr)),
          Asgn(
            Path(^env.types.sizeType, *root(dest), 1),
            Add(^env.types.sizeType,
              Sub(^env.types.sizeType,
                ^value(tree.child(n, 2)),
                ^value(tree.child(n, 1))),
              ^c.genInt(env, 1, env.types.sizeType, bu)))),
        els)
  of mnkSetConstr:
    c.emitDefault(env, dest, stmts, bu)

    proc genIncl(c; env; dest, elem: Expr, bu): NodeRef {.nimcall.} =
      let desc = env.types.headerFor(dest.typ, Lowered)
      if desc.kind == tkArray:
        bu.build Call(
          ^bu.useCompilerProc(c, env, "nimSetIncl"),
          ^c.genArrayAddr(env, dest, bu),
          *use(elem))
      else:
        bu.build *asgn(dest, BitOr(^dest.typ,
          *use(dest),
          Shl(^dest.typ,
            ^c.genInt(env, 1, dest.typ, bu),
            *use(elem))))

    template elemOperand(n): NodeRef =
      c.setElemToCgir(env, c.valueToCgir(env, tree, n, bu), typ, bu)

    let elem =
      if env.types.headerFor(typ, Lowered).kind == tkArray:
        UInt16Type
      else:
        typ

    for it in tree.items(n, 0, ^1):
      if tree[it].kind == mnkRange:
        # a range constructor. Include all elements part of the range in the
        # set
        let idx = c.newTemp(env, elem, stmts, bu)
        stmts.addStmt bu, *asgn(idx, ^elemOperand(tree.child(it, 0)))

        stmts.addStmt bu, While(
          Le(BoolType, elem,
            *use(idx),
            ^elemOperand(tree.child(it, 1))),
          StmtList(
            ^c.genIncl(env, dest, idx, bu),
            # increment the index:
            *asgn(idx,
              Add(elem,
                *use(idx),
                ^c.genInt(env, 1, elem, bu)))))
      else:
        let e = makeExpr(tree[it].typ, elemOperand(it))
        stmts.add c.genIncl(env, dest, e, bu)
  of mnkCast:
    const BlitTypes = {tkStruct, tkArray, tkUnion, tkFloat}
    const PointerTypes = {tkPtr, tkRef, tkPointer, tkCstring}
    let src = tree[n, 0].typ
    var arg = c.valueToCgir(env, tree, tree.child(n, 0), bu)
    if sameType(env.types, typ, src):
      # a no-op; assign the source expression unchanged
      stmts.putInto bu, dest, *use(arg)
    elif env.types.headerFor(env.types.skip(typ), Lowered).kind in BlitTypes or
         env.types.headerFor(env.types.skip(src), Lowered).kind in BlitTypes:
      # either the target or source type is something that requires a
      # blit copy for casting
      let size = min(env.types.headerFor(typ, Lowered).size(env.types),
                     env.types.headerFor(src, Lowered).size(env.types))
      if arg.mode == emValue:
        # not something that the address can be taken of; commit to a temporary
        let tmp = c.newTemp(env, arg.typ, stmts, bu)
        stmts.add c.genAsgn(env, tmp, arg, bu)
        arg = tmp

      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "nimCopyMem"),
        PtrCast(PointerType, ^c.genAddr(env, dest, bu)),
        PtrCast(PointerType, ^c.genAddr(env, arg, bu)),
        ^c.genInt(env, size, env.types.sizeType, bu))
    elif env.types.headerFor(typ, Lowered).kind == tkImported or
         env.types.headerFor(src, Lowered).kind == tkImported:
      # use an opaque type conversion for any non-blit types where the cast
      # involves opaque types
      stmts.putInto bu, dest, Conv(typ, *use(arg))
    elif env.types.headerFor(typ, Lowered).kind in PointerTypes:
      if env.types.headerFor(src, Lowered).kind in PointerTypes:
        stmts.putInto bu, dest, PtrCast(typ, *use(arg))
      else:
        # convert to uint first, then cast to pointer
        stmts.putInto bu, dest, PtrCast(typ,
          ^c.genConv(env, arg, env.types.usizeType, bu))
    elif env.types.headerFor(src, Lowered).kind in PointerTypes:
      # bitcast to uint first, then convert to target type
      let e = bu.buildExpr env.types.usizeType:
        PtrCast(^env.types.usizeType, *use(arg))
      stmts.putInto bu, dest, ^c.genConv(env, e, typ, bu)
    else:
      # it's not a bitcast, but a numeric conversion...
      stmts.putInto bu, dest, ^c.genConv(env, arg, typ, bu)
  else:
    unreachable()

proc getFilePath(c; info: TLineInfo): StringId =
  c.fileNames.withValue info.fileIndex, val:
    result = val[]
  do:
    result = c.module.put(unquotedFilename(c.graph.config, info))
    c.fileNames[info.fileIndex] = result

proc emitLineTrace(c; env; info: TLineInfo, stmts; bu) =
  ## Emits, if enabled, an update of the frame entry's line and file.
  if c.prc.useLineTrace and info.fileIndex != InvalidFileIdx:
    # the current line and file tend to change at a much different frequence,
    # so they're cached/treated separately
    if c.prc.lastLine != info.line:
      stmts.addStmt bu, Asgn(
        ^c.fieldAccess(env, c.prc.frameLocal, 2, bu),
        ^c.genInt(env, info.line.int, env.types.sizeType, bu))
      c.prc.lastLine = info.line

    if c.prc.lastFileIndex != info.fileIndex:
      let name = c.getFilePath(info)
      stmts.addStmt bu, Asgn(
        ^c.fieldAccess(env, c.prc.frameLocal, 3, bu),
        Value(CstringType, name))
      c.prc.lastFileIndex = info.fileIndex

  # FIXME: the `FR_` state tracking used is naive and doesn't take
  #        into account unstructured control-flow, which can lead to incorrect
  #        line numbers / files being reported in some edge cases
  # TODO: make inserting the line tracing a dedicated pass (and use a proper
  #       control-flow analysis while at it)

proc emitRaise(c; env; target: MirNode, stmts, bu) =
  if capExceptions in c.caps:
    stmts.addStmt bu, Raise(*exit(target))
  else:
    # set error flag to true and jump to specified target
    stmts.addStmt bu, Store(^c.prc.errorLocal.n, Value(BoolType, true))
    stmts.add c.genGotoExit(env, target, bu)

proc wrapImpl(stmts: var seq[NodeRef], start: int, bu): NodeRef =
  ## Folds the statements from `stmts` starting at `start` into a
  ## statement list.
  if stmts.len > start + 1:
    # wrap in a statement list
    let r = bu.build StmtList(^stmts[start..^1])
    stmts.shrink(start)
    r
  elif stmts.len > start:
    # a single statement, no need to wrap
    stmts.pop()
  else:
    bu.build StmtList()

template wrap(stmts: var seq[NodeRef], body: untyped): NodeRef =
  ## Evaluates `body` and afterwards pops all statements added to `stmts`
  ## from the list, folding them into a StmtList when necessary.
  let start = stmts.len
  body
  wrapImpl(stmts, start, bu)

proc defineLocal(c; env; id: LocalId, typ: TypeId, isIndirect: bool, bu): StringId =
  ## Adds a MIR local to the translation context.
  result = c.prc.localMap[id]
  if isIndirect:
    c.prc.accessors[id] = (bu.build(Use(typ, ^localRef(result))), emIndirect)
  else:
    c.prc.accessors[id] = (bu.build(^localRef(result)), emSym)

proc genLocalDef*(c; env; id: LocalId, bu): NodeRef =
  ## Generates the `Def` for the given local, also adding the local to the
  ## translation context.
  let loc {.cursor.} = c.prc.body[id]
  var flags: set[CgLocAttrib]
  if sfVolatile in loc.flags: flags.incl Volatile
  if sfNoalias in loc.flags:  flags.incl CgLocAttrib.NoAlias
  if sfRegister in loc.flags: flags.incl Register

  let name = c.defineLocal(env, id, loc.typ, false, bu)
  bu.build Def(^int64(loc.alignment), flags, ^loc.typ, ^localRef(name))

proc emitToCgir(c; env; tree; n; bu): NodeRef =
  ## Translates a MIR emit/asm statement to its CGIR counterpart.
  var elems: seq[NodeRef]
  for it in tree.subNodes(n):
    case tree[it].kind
    of mnkStrLit:
      # use the string verbatim
      elems.add bu.build(^env[tree[it].strVal])
    of mnkType:
      elems.add bu.build(^tree[it].typ)
    of mnkProcVal:
      # emit a raw proc name, not a proc address
      elems.add bu.build(^c.access(env, tree[it].prc, bu))
    else:
      elems.add bu.use(c.valueToCgir(env, tree, it, bu))

  if tree[n].kind == mnkAsm:
    let mode =
      if c.graph.config.backend == backendC:
        if hasGnuAsm in CC[c.graph.config.cCompiler].props:
          asmGnu
        else:
          asmMsvc
      else:
        asmJs
    elems.insert bu.build(^ord(mode))

  case tree[n].kind
  of mnkAsm:  bu.build Asm(elems)
  of mnkEmit: bu.build Emit(elems)
  else:       unreachable()

proc stmtToCgir(c; env; tree; n; stmts; bu) =
  ## Translates simple MIR statements to the semantically equivalent CGIR
  ## statement(s).
  when defined(nimCompilerStacktraceHints):
    frameMsg(c.graph.config, c.prc.body.source[tree[n].info])
  c.useSourceLoc(tree[n].info, bu)
  case tree[n].kind
  of mnkDef, mnkDefCursor:
    if tree[n, 0].kind != mnkParam:
      # defs for globals are also valid in the MIR. They're translated to
      # normal assignments
      # note: the def itself doesn't need a line trace because it's
      # no statement / has no effect
      if tree[n, 0].kind in {mnkLocal, mnkTemp}:
        stmts.add c.genLocalDef(env, tree[n, 0].local, bu)

      let dest = c.gen(env, tree, tree.child(n, 0), bu)
      if tree[n, 1].kind != mnkNone:
        c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
        c.exprToCgir(env, tree, tree.child(n, 1), dest, stmts, bu)
      elif tree[n, 0].kind in {mnkLocal, mnkTemp} and
           sfNoInit notin c.prc.body.locals[tree[n, 0].local].flags:
        c.emitDefault(env, dest, stmts, bu)
      elif tree[n, 0].kind == mnkGlobal and
           sfNoInit notin env[tree[n, 0].global].flags:
        c.emitDefault(env, dest, stmts, bu)
  of mnkBind, mnkBindMut:
    # translated to a local storing the address of the RHS
    # TODO: translate to unique pointer (i.e., not aliased pointer) once the
    #       MIR semantics regarding bind are cleared up. Using a unique pointer
    #       is too risky at the time of writing
    let typ = env.newPtrType(tree[n, 0].typ)
    let name = c.defineLocal(env, tree[n, 0].local, typ, true, bu)
    c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
    stmts.addStmt bu, Def(0, 0, typ, ^localRef(name))
    stmts.addStmt bu, Asgn(
      ^localRef(name),
      ^c.genAddr(env, c.valueToCgir(env, tree, tree.child(n, 1), bu), bu))
  of mnkAsgn, mnkInit:
    c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
    let dest = c.valueToCgir(env, tree, tree.child(n, 0), bu)
    c.exprToCgir(env, tree, tree.child(n, 1), dest, stmts, bu)
  of mnkSwitch:
    # the destination is a `mnkPathVariant` referring to the discriminator
    let src = tree.child(n, 0)
    let root = c.valueToCgir(env, tree, tree.child(src, 0), bu)
    let dest = makeExpr tree[n, 1].typ:
      c.fieldAccess(env, root, tree[src, 1].field, bu)
    c.exprToCgir(env, tree, tree.child(n, 1), dest, stmts, bu)
  of mnkVoid:
    c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
    c.exprToCgir(env, tree, tree.child(n, 0), Expr(typ: VoidType), stmts, bu)
  of mnkExcept:
    if capExceptions notin c.caps:
      stmts.addStmt bu, Store(^c.prc.errorLocal.n, Value(BoolType, false))

    if tree[n].len > 1:
      # it's not a catch-all branch. The exception's dynamic type needs to be
      # compared against the expected types
      let
        excType = env.types.add(c.graph.getCompilerProc("Exception").typ)
        excPtr  = env.newPtrType(excType)
        ex      = c.newTemp(env, excPtr, stmts, bu)
        expr    = bu.buildInd(excType, *use(ex))

      c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
      stmts.putInto bu, ex, Call(
        ^bu.useCompilerProc(c, env, "nimBorrowCurrentException"))

      let then = c.prc.newLabel()
      let body = wrap stmts:
        for it in tree.items(n, 1, ^2):
          stmts.addStmt bu, If(
            ^c.genOf(env, tree, expr, tree[it].typ, bu),
            Break(^labelRef(then)))

        # looking for a handler needs to continue if none of the types match
        c.emitRaise(env, tree[tree.last(n)], stmts, bu)

      stmts.addStmt bu, Block(^labelRef(then), body)
    # else: a catch-all handler needs no extra handling
  of mnkFinally:
    if capExceptions notin c.caps:
      stmts.addStmt bu, Store(^c.prc.errorLocal.n, Value(BoolType, false))
  of mnkRaise:
    c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
    c.emitRaise(env, tree[tree.last(n)], stmts, bu)
  of mnkContinue:
    c.emitRaise(env, tree[tree.last(n)], stmts, bu)
  of mnkEmit, mnkAsm:
    c.emitLineTrace(env, c.prc.body.source[tree[n].info].info, stmts, bu)
    stmts.add c.emitToCgir(env, tree, n, bu)
  else:
    unreachable(tree[n].kind)

proc defineGlobal*(c; env: var MirEnv, id: GlobalId): StringId =
  ## Adds a CGIR definition for the given global to the module, returning the
  ## global's external name.
  let s = env[id]
  result = c.symToName(s)

  if result in c.module.globals:
    if sfImportc in s.flags:
      return # imported or defined already, ignore
    elif c.module.ast[c.module.globals[result]].kind != cnkGlobalImp:
      # external name collision
      c.graph.config.localReport(s.info,
        reportStr(rsemNameCollision, c.module.get(result)))
      return
    # overwriting the entity is fine when its an imported/foreign one

  var typ = env.types.add(s.typ)
  if exfDynamicLib in s.extFlags:
    # the global is a pointer to the actual location
    typ = env.newPtrType(typ)

  var attribs: set[CgLocAttrib]
  if sfNoalias in s.flags:  attribs.incl CgLocAttrib.NoAlias
  if sfVolatile in s.flags: attribs.incl Volatile

  let kind =
    if sfImportc in s.flags:         cnkGlobalImp
    elif exfExportLib in s.extFlags: cnkGlobalExp
    else:                            cnkGlobalDef
  let storage =
    if optThreads in c.graph.config.globalOptions and sfThread in s.flags:
      CgStorage.Thread
    else:
      Normal
  let got = c.buildTree:
    (kind)(storage, ^s.alignment, attribs, typ, ^globalRef(result))
  c.module.globals[result] = got

proc defineConst*(c; env: var MirEnv, id: ConstId): StringId =
  ## Adds a CGIR definition for the given constant, returning the constant's
  ## external named.
  # constants are translated to CGIR globals too
  let s = env[id]
  result = c.symToName(s)

  if result in c.module.globals:
    if sfImportc in s.flags:
      return # imported or defined already, ignore
    elif c.module.ast[c.module.globals[result]].kind != cnkGlobalImp:
      c.graph.config.localReport(s.info,
        reportStr(rsemNameCollision, c.module.get(result)))
      return

  let got =
    if sfImportc in s.flags:
      c.buildTree GlobalImp(^CgStorage.Const, 0, 0,
        ^env.types.add(s.typ),
        ^globalRef(result))
    else:
      c.buildTree GlobalDef(^CgStorage.Const, 0, 0,
        ^env.types.add(s.typ),
        ^globalRef(result),
        ^c.constToCgir(env, env[env.dataFor(id)], NodePosition(0), bu))
  c.module.globals[result] = got

proc defineDynlibProc*(c; env; id: ProcedureId): StringId =
  ## Adds a definition for the .dynlib procedure identified by `id`
  ## to the module.
  let s = env[id]
  result = c.symToName(s)
  # a .dynlib procedure is really a global variable
  let got = c.buildTree GlobalDef(
    ^CgStorage.Normal,
    0, # use natural alignment
    0, # no attributes
    ^env.types.add(s.typ),
    ^globalRef(result))
  c.module.globals[result] = got

proc defineForeignProc*(c; env; id: ProcedureId): StringId =
  ## Adds a CGIR definition for the given foreign procedure to the module,
  ## returning the procedure's external name.
  let s = env[id]
  assert exfNoDecl notin s.extFlags
  result = c.symToName(s)
  if result in c.module.procs:
    # happens when there's a procedure with the same name. Keep the
    # existing definition
    return

  let typ = env.types.addSignature(s.typ)
  c.module.procs[result] = c.buildTree:
    ProcImp(^computeAttribs(s), typ, ^procRef(result))

proc toTree(c; env; tree; list: seq[Stmt], i: int, stmts; bu)

proc toTreeStmts(c; env; tree; list: seq[Stmt], i: int, stmts, bu) =
  var i = i
  while i != 0:
    toTree(c, env, tree, list, i, stmts, bu)
    i = list[i].next

proc isSimpleCase(tree; n: NodePosition): bool =
  # if any filter is a range, the 'case' is said to be "not simple"
  for it in tree.subNodes(n, 1):
    for filter in tree.items(it, 0, ^2):
      if tree[filter].kind == mnkRange:
        return false
  result = true

proc simpleCaseToCgir(c; env; tree; list: seq[Stmt], i: int, stmts, bu) =
  ## Translates a MIR case statement to a CGIR dispatcher statement.
  let n = list[i].n
  var i = list[i].sub
  var branches, filters: seq[NodeRef]
  while i != 0:
    let b = list[i].n
    filters.shrink(0)
    for it in tree.items(b, 0, ^2):
      filters.add bu.use(c.valueToCgir(env, tree, it, bu))

    branches.add bu.build(Target(
      filters,
      ^wrap(stmts, toTreeStmts(c, env, tree, list, list[i].sub, stmts, bu))))

    i = list[i].next

  stmts.addStmt bu, Dispatch(
    *use(^c.valueToCgir(env, tree, tree.child(n, 0), bu)),
    branches)

proc complexCaseToCgir(c; env; tree; list: seq[Stmt], i: int, stmts, bu) =
  ## Translates a MIR case statement to a chain of CGIR 'if' statements.
  var i = i
  let n = list[i].n
  # the selector expression is side-effect free and can thus be duplicated
  # without problem
  let sel = c.gen(env, tree, tree.child(n, 0), bu)

  var cmpType = env.types.canonical(tree[n, 0].typ)
    ## the type to use for Le comparisons
  if cmpType == CharType:
    cmpType = UInt8Type

  template operand(bu: var Builder, e: Expr): NodeRef =
    let x = e
    if env.types.canonical(x.typ) == CharType:
      bu.build Bitcast(UInt8Type, *use(x))
    else:
      bu.use(x)

  i = list[i].sub
  while i != 0:
    let b = list[i].n
    let next = c.prc.newLabel()
    let search = c.prc.newLabel()

    if tree[b].len > 1:
      # look for a match with one of the accepted values. If there's none,
      # jump to the next 'of' branch
      let body = wrap stmts:
        for it in tree.items(b, 0, ^2):
          if tree[it].kind == mnkRange:
            stmts.addStmt bu, If(
              Le(BoolType, cmpType,
                *operand(^c.valueToCgir(env, tree, tree.child(it, 0), bu)),
                *operand(sel)),
              If(
                Le(BoolType, cmpType,
                  *operand(sel),
                  *operand(^c.valueToCgir(env, tree, tree.child(it, 1), bu))),
                Break(^labelRef(search))))
          else:
            stmts.addStmt bu, If(
              Eq(BoolType, ^tree[n, 0].typ,
                *use(sel),
                *use(^c.valueToCgir(env, tree, it, bu))),
              Break(^labelRef(search)))

        # jump to the next handler when none of the filter values apply
        stmts.addStmt bu, Break(^labelRef(next))

      stmts.addStmt bu, Block(
        ^labelRef(next),
        StmtList(
          Block(^labelRef(search), body),
          # the actual body of the dispatcher target:
          ^wrap(stmts,
            toTreeStmts(c, env, tree, list, list[i].sub, stmts, bu))))
    else:
      # it's an else branch
      toTreeStmts(c, env, tree, list, list[i].sub, stmts, bu)

    i = list[i].next

  if tree[tree.last(n)].len != 1:
    # there's no 'else' branch; mark the path as unreachable
    stmts.addStmt bu, Unreachable()

proc toTree(c; env; tree; list: seq[Stmt], i: int, stmts, bu) =
  template sub(): NodeRef =
    wrap stmts:
      toTreeStmts(c, env, tree, list, list[i].sub, stmts, bu)

  case list[i].kind
  of Stmts:
    c.stmtToCgir(env, tree, list[i].n, stmts, bu)
  of Scope:
    if list[i].sub == 0:
      stmts.addStmt bu, StmtList()
    else:
      stmts.addStmt bu, Scope(^sub())
  of Try:
    c.useSourceLoc(tree[list[i].n].info, bu)
    if capExceptions in c.caps:
      stmts.addStmt bu, Try(
        ^labelRef(tree[list[i].n].label),
        ^sub())
    else:
      stmts.addStmt bu, Block(
        ^labelRef(tree[list[i].n].label),
        ^sub())
  of Block:
    c.useSourceLoc(tree[list[i].n].info, bu)
    stmts.addStmt bu, Block(
      ^node(cnkLabel, tree[list[i].n].label.uint32),
      ^sub())
  of Loop:
    c.useSourceLoc(tree[list[i].n].info, bu)
    stmts.addStmt bu, While(Value(BoolType, true), ^sub())
  of Break:
    c.useSourceLoc(tree[list[i].n].info, bu)
    stmts.addStmt bu, Break(^node(cnkLabel, tree[list[i].n].label.uint32))
  of Raise:
    c.useSourceLoc(tree[list[i].n].info, bu)
    c.emitRaise(env, tree[tree.last(list[i].n)], stmts, bu)
  of Dispatch:
    c.useSourceLoc(tree[list[i].n].info, bu)
    if isSimpleCase(tree, list[i].n):
      c.simpleCaseToCgir(env, tree, list, i, stmts, bu)
    else:
      c.complexCaseToCgir(env, tree, list, i, stmts, bu)
  of If:
    c.useSourceLoc(tree[list[i].n].info, bu)
    stmts.addStmt bu, If(
      *use(^c.valueToCgir(env, tree, tree.child(list[i].n, 0), bu)),
      ^sub())
  of Target:
    unreachable()
  of Return:
    c.useSourceLoc(tree[list[i].n].info, bu)
    stmts.addStmt bu, Break(^labelRef(c.prc.unwindLabel))
  of None:
    discard "emit nothing"

proc fix(tree: var MirTree) =
  ## mirgen produces malformed trees in some cases. This procedure is a
  ## best-effort at fixing this.
  # TODO: everything about this is wrong. `mirgen` must not produce malformed
  #       trees, period
  for it in tree.mitems:
    # only mnkPathConv seems to be affected, let's hope that this is enough
    if it.kind == mnkPathConv and it.len == 0:
      it.len = 1

proc hasExit(tree): bool =
  ## Whether the procedure exits via either returning or unwinding.
  result = false
  for it in tree.items:
    if it.kind in {mnkUnwind, mnkReturn}:
      result = true
      break

proc needsErrorFlag(tree): bool =
  ## Detects whether the body requires access to the error flag (when EH
  ## emulation is enabled).
  result = false
  for it in tree.items:
    if it.kind in {mnkRaise, mnkCheckedCall}:
      result = true
      break

proc procToCgir(c; env; sym: PSym): StringId =
  ## Implements the
  result = symToName(c, sym)
  if result in c.module.procs and
     c.module.ast[c.module.procs[result]].kind != cnkProcImp:
    c.graph.config.localReport(sym.info,
      reportStr(rsemNameCollision, c.module.get(result)))

  let procType = env.types.addSignature(sym.typ)

  c.prc.localMap.synchronize(c.prc.body.locals)
  c.prc.accessors.synchronize(c.prc.body.locals)

  # local names are mangled without taking scope into consideration
  var conflicts: CountTable[string]
  for id, it in c.prc.body.locals.pairs:
    if env.types.canonical(it.typ) != VoidType:
      var name: string
      if it.name.isNil:
        # use the ID prefixed by an underscore, which is guaranteed to be a
        # unique name within the procedure
        name = "_" & $id.int
      else:
        # mangle the user-defined name, also accounting for shadowed names
        var key = it.name.s.mangle
        let counter = conflicts.getOrDefault(key)
        name = key
        if counter != 0 or isKeyword(it.name) or
           c.graph.config.cppDefines.contains(key):
          name.add "_"
          name.addInt counter+1
        conflicts.inc(key)

      c.prc.localMap[id] = c.module.put(name)

  reset(conflicts) # not needed anymore

  c.prc.useLineTrace =
    ({optLineTrace, optStackTrace} * sym.options == {optLineTrace, optStackTrace}) and
    sfPure notin sym.flags
  let useStackTrace = optStackTrace in sym.options and sfPure notin sym.flags
  let useErrorFlag = capExceptions notin c.caps and needsErrorFlag(c.prc.body.code)

  var stmts: seq[NodeRef]
  var bu = initBuilder()

  if useStackTrace:
    # why is the stack-frame managment implemented here and not by the code
    # generators? Because it's part of NimSkull's operational semantics

    # fecth the ``TFrame`` type from the ``nimFrame`` signature
    let pt = env.types.add(c.graph.getCompilerProc("nimFrame").typ[1])
    let name = c.module.put("FR_")
    # ^^ the name is something that cannot collide with user-provided names
    let fr = Expr(
      mode: emSym,
      typ: env.types.headerFor(pt, Lowered).elem,
      n: bu.build ^localRef(name)
    )
    # emit the setup for the frame entry (file, name, line):
    let fname = c.getFilePath(sym.info)
    stmts.addStmt bu, Def(0, 0, ^fr.typ, ^localRef(name))
    stmts.addStmt bu, Asgn(
      ^c.fieldAccess(env, fr, 3, bu),
      Value(CstringType, fname))
    stmts.addStmt bu, Asgn(
      ^c.fieldAccess(env, fr, 1, bu),
      Value(CstringType, ^sym.name.s))
    stmts.addStmt bu, Asgn(
      ^c.fieldAccess(env, fr, 2, bu),
      ^c.genInt(env, 0, env.types.sizeType, bu))
    stmts.addStmt bu, Call(
      ^bu.useCompilerProc(c, env, "nimFrame"),
      Addr(pt,
        ^localRef(name)))

    c.prc.frameLocal = fr
    c.prc.lastLine = 0
    c.prc.lastFileIndex = sym.info.fileIndex

  if useErrorFlag:
    # keep the code smaller by querying and modyfying the error flag through
    # a pointer that's fetched on procedure entry
    let typ = env.newPtrType(BoolType)
    let name = c.module.put("error_")
    stmts.addStmt bu, Def(0, 0, typ, ^localRef(name))
    c.prc.errorLocal = bu.buildInd(BoolType, Use(typ, ^localRef(name)))
    stmts.addStmt bu, Asgn(
      ^c.prc.errorLocal.n,
      Call(^bu.useCompilerProc(c, env, "nimErrorFlag")))

  # add the def for the result variable
  if not env.types.isVoidReturn(c.prc.body[resultId].typ):
    stmts.add c.genLocalDef(env, resultId, bu)

  let procTypeDesc = env.types.headerFor(procType, Canonical)

  template param(noAlias: bool, name: StringId): NodeRef =
    let attribs = if noAlias: {CgParamAttrib.NoAlias} else: {}
    bu.build Param(attribs, ^localRef(name))

  var params: seq[NodeRef]
  # gather the list of parameter and emit parameter-related setup:
  for (i, typ, flags) in env.types.params(procTypeDesc):
    if env.types.canonical(typ) != VoidType:
      let id = LocalId(i + 1)
      # the noalias flag can only be retrieved from the original symbol
      let isNoAlias = sfNoalias in sym.typ.n[i + 1].sym.flags
      if pfByRef in flags:
        # can use noalias attribute, since the location pointed to by immutable
        # pass-by-reference is guaranteed to not be (safely) accessible from
        # any other pointer
        let pt = env.newPtrType(typ)
        let name = c.defineLocal(env, id, pt, true, bu)
        params.add param(true, name)
      elif env.types.isOpenArray(typ):
        # for historical reasons, the openArray is passed as two parameters.
        # Combine them back into a single value
        let
          name = c.defineLocal(env, id, typ, false, bu)
          nameStr {.cursor.} = c.module.get(name)
          p1 = c.module.put(nameStr & "Data_")
          p2 = c.module.put(nameStr & "Len_")
        stmts.addStmt bu, Def(0, 0, typ, ^localRef(name))
        stmts.addStmt bu, Asgn(
          Path(^env.types.getFieldType(typ, 0),
            Use(typ, ^localRef(name)), 0),
          Use(^env.types.getFieldType(typ, 0), ^localRef(p1)))
        stmts.addStmt bu, Asgn(
          Path(^env.types.sizeType,
            Use(typ, ^localRef(name)), 1),
          Use(^env.types.sizeType, ^localRef(p2)))

        params.add param(isNoAlias, p1)
        params.add param(false, p2)
      else:
        let name = c.defineLocal(env, id, typ, false, bu)
        params.add param(isNoAlias, name)

  # out parameter:
  if env.types.headerFor(c.prc.body[resultId].typ, Lowered).kind == tkArray:
    let typ = env.newPtrType(c.prc.body[resultId].typ)
    let name = c.defineLocal(env, resultId, typ, true, bu)
    params.add param(true, name)

  # handle the extra parameter(s):
  case procTypeDesc.callConv(env.types)
  of ccTailcall:
    let id = LocalId(procTypeDesc.numParams() + 1)
    let name = c.defineLocal(env, id, PointerType, false, bu)
    params.add param(true, name)
  of ccClosure:
    params.add param(false, c.module.put("ClE_0"))
    let id = LocalId(procTypeDesc.numParams() + 1)
    # ptrcast the generic pointer to the expected environment type and
    # assign the internal env parameter local to it
    stmts.add c.genLocalDef(env, id, bu)
    stmts.addStmt bu, Asgn(
      ^localRef(c.prc.localMap[id]),
      PtrCast(^c.prc.body[id].typ,
        Use(PointerType,
          ^localRef(c.module.put("ClE_0")))))
  else:
    discard "nothing to do"

  c.prc.nextLabel = c.prc.body.nextLabel.uint32
  c.prc.nextLocal = c.prc.body.locals.nextId.ord.uint32

  # create the structured control-flow view and use it to guide translation
  var list = toStructured(c.prc.body.code)
  optimize(list)
  if hasExit(c.prc.body.code):
    # the body is wrapped in a block, which is used as the target for both
    # `Return` and `Unwind`
    c.prc.unwindLabel = c.prc.newLabel()
    stmts.addStmt bu, Block(
      ^labelRef(c.prc.unwindLabel),
      ^wrap(stmts, toTree(c, env, c.prc.body.code, list, 0, stmts, bu)))
    if useStackTrace:
      stmts.addStmt bu, Call(
        ^bu.useCompilerProc(c, env, "popFrame"))
    if env.types.isVoidReturn(c.prc.body[resultId].typ):
      stmts.addStmt bu, Return()
    else:
      stmts.addStmt bu, Return(
        Use(^c.prc.body[resultId].typ,
          ^localRef(c.prc.localMap[resultId])))
  else:
    toTree(c, env, c.prc.body.code, list, 0, stmts, bu)

  assert stmts.len > 0
  # assemble into the final definition and it to the module:
  let kind = (if exfExportLib in sym.extFlags: cnkProcExp else: cnkProcDef)
  let got = bu.build (kind)(
    ^computeAttribs(sym),
    procType,
    ^procRef(result),
    Params(params),
    ^wrapImpl(stmts, 0, bu))
  c.module.procs[result] = c.module.ast.append(bu, got)

proc defineProc*(c; env: var MirEnv, id: ProcedureId, body: sink MirBody): StringId =
  ## Translates the MIR procedure to a CGIR procedure, adding it to the module
  ## under the returned name.
  fix(body.code)
  # the `prc` context is expected to default-initialized at this point
  c.prc.body = move body
  let s = env[id]
  result = procToCgir(c, env, s)
  reset(c.prc) # free the memory associated with the context

proc translateTopLevelEmit*(c; env: var MirEnv, s: sink MirBody): cgir2.NodeIndex =
  ## Translates the top-level emit or asm statement `s` to the corresponding
  ## CGIR statement, returning its position in the AST.
  assert s.code.len > 0 and s.code[0].kind in {mnkEmit, mnkAsm}
  c.prc.body = s
  var bu = initBuilder()
  let r = emitToCgir(c, env, c.prc.body.code, NodePosition(0), bu)
  result = c.module.ast.append(bu, r)
  c.prc.reset()

proc initContext*(g: ModuleGraph, caps: set[Capability]): Context =
  ## Creates a translation context for a target with the given capabilities.
  Context(graph: g, tctx: mirtypes2cg.initContext(g), caps: caps)

proc name*(c; s: PSym): Option[StringId] =
  ## For symbols referring to external symbols, returns none.
  ## For everything else, returns the external name.
  if exfNoDecl in s.extFlags:
    none(StringId)
  else:
    some(symToName(c, s))

proc current*(c: Context): lent CgModule =
  ## Returns the in-progress and thus possibly incomplete CG module
  ## constructed so far.
  c.module

proc genAbiCheck*(c; env: MirEnv, id: TypeId): cgir2.NodeIndex =
  ## Generates an emit statement for a C ABI check.
  # TODO: move this routine to a better place. It has nothing to do with
  #       MIR->CGIR translation
  var msg = ", \"C compiler & NimSkull disagree on size for: "
  msg.addTypeHeader(c.graph.config, env.types[id])
  msg.add "\");"
  c.buildTree Emit(
    "NIM_STATIC_ASSERT(sizeof(", id, ") == ",
    ^c.genInt(env, env.types.headerFor(id, Lowered).size(env.types),
              env.types.usizeType, bu),
    msg
  )

proc getRtti*(c: Context): lent seq[(StringId, ItemId)] =
  ## Returns the list of all RTTI globals created so far, together with their
  ## originating-from type's ID.
  c.rtti

proc close*(c: sink Context): CgModule =
  ## Produces the completed CGIR module.
  privateAccess(BiTable[SourceLoc])
  c.module.infos = c.sourceLocs.vals
  c.module
