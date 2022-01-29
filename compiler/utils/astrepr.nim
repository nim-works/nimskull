## `treeRepr()` implementation for the `PNode` tree structures

import
  ast/[
    ast,
    reports,
    renderer,
    lineinfos,
    errorhandling
  ],
  front/[
    options,
    msgs
  ],
  experimental/colortext,
  std/[
    tables,
    strutils,
    strformat,
  ]

import std/options as std_options

type
  TReprFlag* = enum
    trfPositionIndexed
    trfReportInfo
    trfPackedFields
    trfSkipAuxError
    trfSymDefined
    trfIndexVisisted

    trfShowSymFlags
    trfShowSymLineInfo
    trfShowSymTypes
    trfShowSymMagic
    trfShowSymKind
    trfShowSymOwner
    trfShowSymOptions
    trfShowSymPosition
    trfShowSymOffset
    trfShowSymBitsize
    trfShowSymAlignment

    trfShowTypeCallConv
    trfShowTypeFlags
    trfShowTypeSize
    trfShowTypeOwner
    trfShowTypeBaseId

    trfShowNodeLineInfo
    trfShowNodeFlags
    trfShowNodeIds
    trfShowNodeValue
    trfShowNodeComments
    trfShowNodeErrors
    trfShowNodeTypes


export colortext.`$`

const treeReprAllFields* = { trfShowSymFlags .. trfShowNodeTypes }

const defaultTreeReprFlags* = {
  trfPositionIndexed,
  trfReportInfo,
  trfSkipAuxError,
  trfIndexVisisted
} + treeReprAllFields - {
  trfShowNodeLineInfo,
  trfShowSymLineInfo,
  trfShowSymOptions,
}

const treeReprCompact* = defaultTreeReprFlags + {trfPackedFields}

const IntTypes = {
  tyInt, tyInt8, tyInt16,
  tyInt32, tyInt64, tyFloat, tyFloat32, tyFloat64, tyUInt, tyUInt8,
  tyUInt16, tyUInt32, tyUInt64
}

const CompressedBuiltinTypes = {tyBool, tyChar, tyPointer, tyString} + IntTypes

proc textRepr(conf: ConfigRef, typ: PType): ColText =
  result.add ($typ.kind)[2..^1] + fgMagenta
  if not isNil(typ.sym):
    result &= " sk:" & ($typ.sym.kind)[2..^1] + fgCyan

  if not isNil(typ.n):
    let t = $typ.n
    if '\n' notin t:
      result &= " " & t + fgRed


template genFields(res: ColText, indent: int, flags: set[TReprFlag]): untyped =
  proc hfield(name: string, text: ColText = default ColText) =
    res.add " "
    res.add name
    res.add ":"
    res.add text

  proc vfield(name: string, text: ColText = default ColText) =
    res.newline()
    res.addIndent(indent, 1)
    res.add alignLeft(name & ":", 7)
    res.add text

  proc field(name: string, text: ColText = default ColText) =
    if trfPackedFields in flags:
      hfield(name, text)
    else:
      vfield(name, text)

proc format[I](s: set[I], sub: int): string =
  var first = true
  result = "{"
  for item in s:
    if not first: result.add ", "
    result.add substr($item, sub)
    first = false

  result.add "}"

const setStyle = termFg(2, 1, 3)

func format(i: SomeInteger): ColText = $i + fgCyan

proc symFields(
    conf: ConfigRef,
    sym: PSym,
    flags: set[TReprFlag] = defaultTreeReprFlags,
    indent: int = 0
  ): ColText =

  coloredResult(1)
  var res = addr result
  genFields(res[], indent, flags)

  if trfShowSymFlags in flags and sym.flags.len > 0:
    field("flags", format(sym.flags, 2) + setStyle)

  if trfShowSymMagic in flags and sym.magic != mNone:
    field("magic", substr($sym.magic, 1) + setStyle)

  if trfShowSymOptions in flags and 0 < sym.options.len:
    field("opts", format(sym.options, 3) + setStyle)

  if trfShowSymOffset in flags and 0 <= sym.offset:
    field("offset", sym.offset.format())

  if trfShowSymPosition in flags and sym.position != 0:
    field("pos", sym.offset.format())

  if trfShowSymTypes in flags and not sym.typ.isNil():
    field("typ", conf.textRepr(sym.typ))

  if not isNil(conf) and trfShowSymLineInfo in flags and sym.info != unknownLineInfo:
    field("info", conf.toMsgFilename(sym.info.fileIndex) + fgBlue)
    add "(", $sym.info.line + fgCyan
    add ".", $sym.info.col + fgCyan
    add ")"

  if trfShowSymOwner in flags and not sym.owner.isNil():
    field("owner")
    let own = sym.owner
    hfield("kind", $own.kind + fgGreen)
    hfield("name", $own.name.s + fgCyan)

  if sym.kind in {skLet, skVar, skField, skForVar}:
    if trfShowSymBitsize in flags and sym.bitsize != 0:
      field("bitsize", sym.bitsize.format())

    if trfShowSymAlignment in flags and sym.alignment != 0:
      field("alignment", sym.bitsize.format())



proc treeRepr*(
    conf: ConfigRef,
    sym: PSym,
    flags: set[TReprFlag] = defaultTreeReprFlags,
    indent: int = 0
  ): ColText =

  coloredResult(1)
  if sym.isNil():
    addi indent, "<nil>" + fgRed

  else:
    addi indent, substr($sym.kind, 2) + fgBlue
    add symFields(conf, sym, flags, indent + 2)

proc typFields(
    conf: ConfigRef,
    typ: PType,
    flags: set[TReprFlag],
    indent: int
  ): ColText =

  let res = addr result
  genFields(res[], indent, flags)

  if trfShowTypeCallConv in flags and typ.kind == tyProc:
    field("callConv", $typ.callConv + fgCyan)

  if trfShowTypeFlags in flags and typ.flags.len > 0:
    field("flags", format(typ.flags, 2) + fgCyan)

  if trfShowTypeSize in flags and typ.size != -1:
    field("size", typ.size.format())

  if trfShowTypeOwner in flags and not typ.owner.isNil():
    field("owner", typ.owner.name.s + fgCyan)

proc treeRepr*(
    conf: ConfigRef,
    typ: PType,
    flags: set[TReprFlag] = defaultTreeReprFlags,
    indent: int = 0
  ): ColText =

  coloredResult(1)

  var res = addr result
  genFields(res[], indent, flags)

  proc aux(typ: PType, level: int) =
    let indent = level * 2 + indent
    if typ.isNil():
      addi indent, "<nil>" + fgRed

    else:
      addi indent, substr($typ.kind, 2) + fgBlue
      if trfShowTypeBaseId in flags:
        hfield("mId", typ.itemId.module.format())
        hfield("iId", typ.itemId.item.format())

      add typFields(conf, typ, flags, indent + 2)

      for sub in typ.sons:
        aux(sub, level + 1)

  aux(typ, 0)



proc treeRepr*(
    conf: ConfigRef,
    pnode: PNode,
    flags: set[TReprFlag] = defaultTreeReprFlags,
    maxDepth: int          = 120,
    maxLen: int            = 30,
    maxPath: int           = 1,
    indentIncrease: int = 0
  ): ColText =
  ## .. include:: tree_repr_doc.rst


  coloredResult(1)


  var visited: Table[int, int]
  var res = addr result
  var nodecount = 0
  proc aux(n: PNode, idx: seq[int]) =
    var indent = indentIncrease
    genFields(res[], indent, flags)

    addIndent(indentIncrease)
    if trfPositionIndexed in flags:
      indent += 2
      if 0 < idx.len:
        var first = true
        for idxIdx, pos in idx:
          if idx.len - maxPath <= idxIdx:
            indent += tern(first, 0, 1)

            case pos:
              of 0 .. 9: indent += 1
              of 10 .. 99: indent += 2
              else: indent += 3

            if not first:
              add "." + termFg(15)

            first = false
            add $pos + termFg(15)

          else:
            indent += 2
            add "  "

        if 0 < maxPath:
          add " "
          inc indent

    else:
      addIndent(idx.len * 2)

    if isNil(n):
      add "<nil>" + fgRed
      return

    elif not (n.kind == nkEmpty and n.safeLen == 0) and
         # empty nodes can be reused. Only check for visitation if it is
         # not an empty (completely empty) node
         cast[int](n) in visited:

      if trfIndexVisisted in flags:
        add "<visited "
        add substr($n.kind, 2) + fgCyan
        add " at "
        add $visited[cast[int](n)] + styleDim
        add ">"

      else:
        add "<visited>"

      return

    elif idx.len > maxDepth:
      add " ..."
      return

    visited[cast[int](n)] = nodecount
    add substr($n.kind, 2) + fgCyan

    if trfIndexVisisted in flags:
      add " "
      add $nodecount + styleDim

    inc nodecount

    when defined(useNodeids):
      if trfShowNodeIds in flags:
        hfield("nid")
        add $n.id

    proc addComment(sep: bool = true) =
      if trfShowNodeComments in flags and n.comment.len > 0:
        var nl = false
        for line in split(n.comment.strip(leading = false), '\n'):
          if nl: add "\n"
          nl = true

          addi indent, "  # " + termFg(4, 2, 1)
          add line + termFg(4, 2, 1)

        add "\n"

      elif sep:
        add " "

    proc addFlags() =
      if trfShowNodeFlags in flags and n.flags.len > 0:
        field("nflags", format(n.flags, 2) + setStyle)

      if trfShowNodeTypes in flags and not n.typ.isNil():
        if (
          n.typ.kind notin CompressedBuiltinTypes or
          (n.kind in nkIntKinds and n.typ.kind notin IntTypes)
        ):
          if n.kind == nkSym and not isNil(n.sym):
            if n.sym.typ == n.typ:
              # Type will be printed in `symFlags`
              discard

            else:
              field("typ", conf.textRepr(n.typ))
              add " != sym.typ" + fgRed

          else:
            field("typ", conf.textRepr(n.typ))

    if not conf.isNil() and trfShowNodeLineInfo in flags and
       n.info != unknownLineInfo:
      field("info", conf.toMsgFilename(n.info.fileIndex) + fgBlue)
      add "("
      add $n.info.line + fgCyan
      add "."
      add $n.info.col + fgCyan
      add ")"

    case n.kind:
      of nkStrKinds:
        add " "
        add "\"" & n.strVal + fgYellow & "\""
        addFlags()
        addComment()

      of nkCharLit .. nkUInt64Lit:
        add " "
        add $n.intVal + fgBlue
        addFlags()
        addComment()

      of nkFloatLit .. nkFloat128Lit:
        add " "
        add $n.floatVal + fgMagenta
        addFlags()
        addComment()

      of nkIdent:
        add " "
        add n.ident.s + fgGreen
        addFlags()
        addComment()

      of nkSym:
        add " "
        add n.sym.name.s + fgCyan
        hfield "sk", substr($n.sym.kind, 2) + fgBlue

        if not n.sym.owner.isNil() and
           n.sym.owner.kind in {skModule, skType}:
          add &"({n.sym.owner.name.s}.{n.sym.name.s})" + termFg(15)

        add symFields(conf, n.sym, flags, indent)
        addFlags()
        addComment()

      of nkCommentStmt:
        addFlags()
        add "\n"
        addComment()

      of nkError:
        if isNil(conf):
          field("err", substr($n.errorKind(), 4) + termFg(5, 2, 0))

        else:
          let report = conf.getReport(n).semReport
          field("err", substr($report.kind, 4) + termFg(5, 2, 0))
          hfield("errid", $n.reportId.int + fgRed)

        let (file, line, col) = n.compilerInstInfo()
        field("info", formatPath(conf, file) + fgBlue)
        add "(", $line + fgCyan
        add ".", $col + fgCyan
        add ")"

      else:
        discard


    if n.kind notin {nkNone .. nkNilLit, nkCommentStmt}:
      addFlags()
      if n.len > 0:
        add "\n"

      addComment(false)

      for newIdx, subn in n:
        if trfSkipAuxError in flags and n.kind == nkError and newIdx in {1, 2}:
          continue

        aux(subn, idx & newIdx)


        if idx.len + 1 > maxDepth:
          break

        if newIdx > maxLen:
          break

        if newIdx < n.len - 1:
          add "\n"

  aux(pnode, @[])

proc debugAst*(it: PNode) {.exportc.} =
  ## Print out tree representation of the AST node.
  ##
  ## .. note:: there is no `ConfigRef` argument, and because of that some
  ##     information cannot be fully retrieved from the AST (such as full
  ##     paths of the FileIndex entries).
  ##
  ## .. tip:: This proc is annotated with `{.exportc.}` which means it's
  ##     mangled name exactly the same - `debugAst` and you can call it
  ##     from the `gdb` debugging session.
  echo treeRepr(nil, it)

proc debugType*(it: PType) {.exportc.} =
  ## Print out tree represntation of the type. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  echo treeRepr(nil, it)

proc debugSym*(it: PSym) {.exportc.} =
  ## Print out tree represntation of the symbol. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  echo treeRepr(nil, it)

proc debug*(it: PNode) =
  ## Convenience overload of `debugAst`
  debugAst(it)

proc debug*(it: PType) =
  ## Convenience overload of `debugType`
  debugType(it)

proc debug*(it: PSym) =
  ## Convenience overload of `debugSym`
  debugSym(it)

proc debug*(conf: ConfigRef, it: PNode) =
  ## Print tree representation of the AST
  echo treeRepr(conf, it)

proc debug*(conf: ConfigRef, it: PType) =
  ## Print tree representation of the type
  echo treeRepr(conf, it)

proc debug*(conf: ConfigRef, it: PSym) =
  ## Print tree reprsentation of the symbol
  echo treeRepr(conf, it)
