## Implements the translation from the MIR type representation to that of
## the CGIR.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types,
    idents
  ],
  compiler/backend/[
    cgir2,
    ccgutils,
    mangling
  ],
  compiler/ic/[
    bitabs
  ],
  compiler/mir/[
    mirtypes,
    mirtrees,
    mirenv
  ],
  compiler/modules/[
    modulegraphs
  ]

type
  Node = CgNode
  Context* = object
    ## Context for type translation.
    graph: ModuleGraph
    map: Table[TypeId, StringId]
      ## canonical type -> type name

  Builder = object
    ## Builder for constructing `CgNode` AST instances.
    buf: seq[Node]
    start: int

using
  c: var Context
  m: var CgModule
  env: TypeEnv
  bu: var Builder

const
  CallingConvMap = [
    ccNimCall: Nimcall, ccStdCall: Stdcall,
    ccCDecl: Cdecl, ccSafeCall: Safecall,
    ccSysCall: Syscall, ccInline: Default,
    ccNoInline: Default, ccFastCall: Fastcall,
    ccClosure: Default, ccNoConvention: Default,
    ccTailcall: Nimcall
  ]

template subTree(bu: var Builder, k: untyped, body: untyped) =
  if true:
    if bu.buf.len > 0:
      inc bu.buf[bu.start].val
    var start = bu.start
    bu.start = bu.buf.len
    bu.buf.add node(k, 0)
    body
    bu.start = start

proc add(bu: var Builder, n: Node) =
  if bu.buf.len > 0:
    inc bu.buf[bu.start].val
  bu.buf.add n

proc finish(bu: sink Builder): seq[Node] =
  bu.buf

proc boolNode(val: bool; m): Node =
  node(cnkBool, m.pack(ord val))

proc intNode(val: int64; m): Node =
  node(cnkInt, m.pack(val))

proc setNode[E](val: set[E]; m): Node =
  node(cnkInt, m.pack(cast[int64](val)))

proc pack(m; val: string): uint32 =
  cast[uint32](m.strings.getOrIncl(val))

proc strNode(val: string; m): Node =
  node(cnkString, m.pack(val))

proc mangleField(name: PIdent): string =
  result = mangle(name.s)
  if isKeyword(name):
    result.add "_0"

proc typeToCgir*(c; env; m; typ: TypeId): StringId
proc translate(c; env; m; desc: TypeHeader, bu)

proc translate(c; env; m; id: TypeId, bu) =
  ## Translates the given MIR type to its CGIR analogue. Types that don't
  ## need a name are inlined, all others use an indirection.
  let desc = env.headerFor(id, Lowered)
  case desc.kind
  of tkStruct, tkUnion, tkProc, tkArray, tkImported:
    # note: array and proc types always being named is done for convenience of
    # the C code generator
    bu.add node(cnkType, cast[uint32](c.typeToCgir(env, m, id)))
  else:
    c.translate(env, m, desc, bu)

proc translateProcType(c; env; m; desc: TypeHeader, bu) =
  bu.subTree cnkProcTy:
    bu.add intNode(ord(CallingConvMap[desc.callConv(env)]), m)
    if desc.retType(env) == VoidType:
      bu.subTree cnkVoidTy: discard
    else:
      c.translate(env, m, desc.retType(env), bu)

    for (i, typ, flags) in env.params(desc):
      # ignore compile-time-only parameters
      if env.canonical(typ) != VoidType:
        if pfByRef in flags:
          bu.subTree cnkPtrTy:
            c.translate(env, m, typ, bu)
        elif env.headerFor(env.canonical(typ), Canonical).kind == tkOpenArray:
          # TODO: only unpack the tuple when the procedure uses the C-interop
          #       ABI -- keep it as a tuple otherwise
          c.translate(env, m, env[env.lookupField(typ, 0)].typ, bu)
          c.translate(env, m, env[env.lookupField(typ, 1)].typ, bu)
        else:
          c.translate(env, m, typ, bu)

    if desc.callConv(env) in {ccClosure, ccTailcall}:
      # TODO: this is wrong. There should be no concept of "closure" this far
      #       in the compilation process
      # TODO: same goes for .tailcall routines, which also only need a pointer
      #       when portable tail-call elimination is enabled
      bu.subTree cnkPtrTy:
        bu.add node(cnkVoidTy)

    if desc.hasVarargs(env):
      bu.subTree cnkVarargs: discard

proc fieldName(c; env; m; f: StructField, prefix: string, index: int): uint32 =
  ## Computes and returns the packed name for `f`. For fields without explicit
  ## names, a name is created by concatenating `prefix` with `pos`.
  let name =
    if not f.isNamed:
      # use a name derived from the index for anonymous fields
      prefix & $index
    elif f.isNoMangle:
      env.name(f)
    else:
      mangleField(c.graph.cache.getIdent(env.name(f)))
  m.pack(name)

proc embedTaggedUnion(c; env; m; desc: TypeHeader, tag: FieldId, bu) =
  let prefix = "_" & env.name(env[tag]) & "_"
  # an anonymous union is used
  bu.subTree cnkField:
    bu.subTree cnkUnionTy:
      bu.add boolNode(false, m) # not packed
      var i = 0
      for _, it in env.fields(desc):
        bu.subTree cnkField:
          if isEmbedded(it):
            c.translate(env, m, env.headerFor(it.typ, Lowered), bu)
          else:
            c.translate(env, m, it.typ, bu)
          bu.add intNode(0, m)
          bu.add intNode(0, m)
          bu.add intNode(0, m)
          bu.add node(cnkString, fieldName(c, env, m, it, prefix, i))
        inc i
    bu.add intNode(0, m)
    bu.add intNode(0, m)
    bu.add intNode(0, m)
    bu.add strNode("", m)

proc translate(c; env; m; desc: TypeHeader, bu) =
  case desc.kind
  of tkBool:
    bu.add node(cnkBoolTy)
  of tkChar:
    bu.add node(cnkCharTy)
  of tkInt:
    bu.subTree cnkIntTy:
      bu.add intNode(desc.size(env), m)
  of tkUInt:
    bu.subTree cnkUIntTy:
      bu.add intNode(desc.size(env), m)
  of tkFloat:
    bu.subTree cnkFloatTy:
      bu.add intNode(desc.size(env), m)
  of tkPointer:
    bu.subTree cnkPtrTy:
      bu.add node(cnkVoidTy)
  of tkPtr, tkRef, tkVar, tkLent:
    let elem = env.canonical(desc.elem)
    if env.headerFor(elem, Lowered).kind == tkUncheckedArray:
      bu.subTree cnkPtrToArrayTy:
        c.translate(env, m, env.headerFor(elem, Lowered).elem, bu)
    else:
      bu.subTree cnkPtrTy:
        c.translate(env, m, elem, bu)
  of tkArray:
    bu.subTree cnkArrayTy:
      bu.add intNode(desc.arrayLen(env), m)
      c.translate(env, m, desc.elem(), bu)
  of tkProc:
    c.translateProcType(env, m, desc, bu)
  of tkStruct, tkUnion:
    bu.subTree (if desc.kind == tkStruct: cnkStructTy else: cnkUnionTy):
      if desc.kind == tkStruct:
        bu.add boolNode(desc.isPacked(env), m)
      else:
        bu.add boolNode(false, m)

      if desc.kind == tkStruct and desc.base(env) != VoidType:
        # the inherited-from type is added as the first field
        bu.subTree cnkField:
          c.translate(env, m, desc.base(env), bu)
          bu.add intNode(0, m)
          bu.add intNode(0, m)
          bu.add intNode(0, m)
          # FIXME: the field name is able to collide with user-defined names
          bu.add strNode("Sup", m)
      elif desc.numFields == 0:
        # happens for structs in tagged unions. All CGIR structs are required
        # to have at least one field, so add one
        # TODO: prevent MIR struct types from having no fields
        bu.subTree cnkField:
          c.translate(env, m, UInt8Type, bu)
          bu.add intNode(0, m)
          bu.add intNode(0, m)
          bu.add intNode(0, m)
          bu.add strNode("_pad", m)

      var idx = 0
      for f, recf in env.fields(desc):
        let fdesc = env.headerFor(recf.typ, Canonical)
        let name = fieldName(c, env, m, recf, "Field", idx)
        let attribs = if recf.isNoAlias: {CgLocAttrib.NoAlias} else: {}
        inc idx

        case fdesc.kind
        of tkUncheckedArray:
          bu.subTree cnkFlexField:
            c.translate(env, m, fdesc.elem, bu)
            bu.add intNode(recf.align, m)
            bu.add node(cnkString, name)
        of tkVoid:
          # TODO: this must not happen. Change `sem` to drop all void fields
          #       early on
          discard "ignore"
        elif isEmbedded(recf):
          embedTaggedUnion(c, env, m, fdesc, env.lookupDiscr(desc, f), bu)
        else:
          bu.subTree cnkField:
            c.translate(env, m, recf.typ, bu)
            bu.add intNode(recf.align, m)
            bu.add setNode(attribs, m)
            bu.add intNode(recf.bitsize, m)
            bu.add node(cnkString, name)
  of tkCstring:
    # the only compatible strings reaching here are C strings. A C string is a
    # pointer to an unbounded C character array
    bu.subTree cnkPtrToArrayTy:
      bu.subTree cnkOpaqueTy:
        bu.add strNode("char", m)
        bu.add strNode("", m)
  of tkVoid, tkClosure, tkSet, tkOpenArray, tkSeq,
     tkString, tkUncheckedArray, tkImported:
    unreachable(desc.kind)

proc typeToCgir*(c; env; m; typ: TypeId): StringId =
  ## Translates the given MIR type to a CGIR type, adding it - if not already
  ## present - to the CGIR module, and returning its name.
  let typ = env.canonical(typ)
  c.map.withValue typ, val:
    # already translated
    result = val[]
  do:
    # add a mapping first, so that recursive types work
    result = m.put(computeTypeName(c.graph, env, typ))
    c.map[typ] = result
    var bu = Builder()
    let desc = env.headerFor(typ, Lowered)
    if desc.kind == tkImported:
      let
        s = env[typ].sym
        header =
          if exfHeader in s.extFlags:
            m.pack(c.graph.getLib(s.annex).path.strVal)
          else:
            m.pack("")

      bu.subTree cnkOpaqueTy:
        bu.add node(cnkString, uint32 m.put(s.extname))
        bu.add node(cnkString, header)
    else:
      c.translate(env, m, desc, bu)

    m.types[result] = m.tast.append(bu.finish())

proc initContext*(g: ModuleGraph): Context =
  ## Creates a type translation context.
  Context(graph: g)
