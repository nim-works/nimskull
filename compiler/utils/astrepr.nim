## `treeRepr()` implementation for the `PNode` tree structures. This module
## provides helper procedures that can be used for debugging purposes -
## dumping compiler IR in the textual form.
##
## For each IR type (``PType``, ``PSym``, ``PNode``) there are several
## procs defined:
#
## - `treeRepr` - main implementation to generate the `ColText` chunk for the
##   object.
## - `debug` - convenience ovelroads that print generated text immediately.
##   There are two version of the `debug` - one that accept `ConfigRef` object,
##   and one that can work without it. Both call `treeRepr` internally, but
##   with `ConfigRef` present more information can be printed.
## - `debugAst`, `debugSym`, `debugType` - procs added for use in `gdb` debugging.
##   They have `{.exportc.}` annotations, so can be used directly by name there
##
## `treeRepr` and all other procedures in the module accept `TReprConf`
## object (default value is provided via `implicitTReprConf` global, for
## gdb procs it is used implicitly) that controls which fields specifically
## will be printed.

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
    strformat
  ]

import std/options as std_options

type
  TReprFlag* = enum
    trfReportInfo ## Show information about embedded semantic report errors
    trfPackedFields ## Put fields horizontally instead of arranging them
                    ## out veritcally. Should be used only when a small
                    ## number of total fields is enabled, otherwise output
                    ## will be unreadable.
    trfSkipAuxError ## Skip 'data store' fields of the `nkError` node -
                    ## elements `[1..2]` are used to store additional
                    ## metadata and are rarely needed for printing.
    trfIndexVisisted ## Enumerate each visited node when printing the
                     ## `PNode` tree
    trfShowDefaultedFields ## Show fields with defaulted values

    trfShowSymFlags ## Show symbol `.flags` field
    trfShowSymLineInfo ## Line information
    trfShowSymTypes ## Flat render of the symbol type
    trfShowSymMagic ## used symbol magic
    trfShowSymKind ## `.kind`
    trfShowSymOwner ## Full chain of the symbol owners
    trfShowSymOptions ## `.options`
    trfShowSymPosition ## `.position`
    trfShowSymOffset ## `.offset`
    trfShowSymBitsize ## `.bitsize`
    trfShowSymAlignment ## `.alignment`

    trfShowTypeCallConv ## Calling convention
    trfShowTypeFlags ## `.flags`
    trfShowTypeSym ## `.sym`
    trfShowTypeAlloc ## `.align` and `.size` fields. They represent a
                     ## single logical chunk of information, and pinter
                     ## together
    trfShowTypeOwner ## Full chain of the type owner
    trfShowTypeBaseId ## `module` and `item` from `itemId` field

    trfShowNodeLineInfo ## Node location information
    trfShowNodeFlags ## `.flags`
    trfShowNodeIds ## `.id` field, available when compiler is built with `-d:nodeIds`
    trfShowNodeComments ## `.comment` field
    trfShowNodeErrors ## Embedded `nkError` reports
    trfShowNodeTypes ## Flat render of the node type

  TReprConf* = object
    ## Main debug configuration object
    flags*: set[TReprFlag]  ## Set of the formatting options for the
    ## procedure. Several built-in constants are defined in this module
    ## that might be suitable for different debugging or AST exploration
    ## tasks.
    maxDepth*: int ## Ignore all nodes that are placed deeper than that.
    ## Useful to see a high-level overview for large nodes, such as full
    ## proc bodies
    maxLen*: int ## on each level, print upto that number of subnodes.
    ## Useful to cut out parts on large `case` trees, toplevel nodes for
    ## modules, statement lists and such.
    maxPath*: int ## maximum path length for subnodes



export colortext.`$`

const treeReprAllFields* = { trfShowSymFlags .. trfShowNodeTypes } ## Set
                           ## of flags to print all fields in all tree
                           ## reprs

const style = (
  kind: fgBlue,
  nilIt: fgRed,
  ident: fgCyan,
  setIt: termFg(2, 1, 3),
  number: fgCyan,
  strLit: fgYellow,
  floatLit: fgMagenta,
  dim: termFg(15),
  errKind: termFg(5, 2, 0),
  err: fgRed,
  comment: termFg(4, 2, 1),
  owner: termFg(1, 3, 0)
)

const defaultTReprConf* = TReprConf(
  maxDepth: 120,
  maxLen: 30,
  maxPath: 1,
  flags: {
    trfReportInfo,
    trfSkipAuxError,
    trfIndexVisisted
  } + treeReprAllFields - {
    trfShowNodeLineInfo,
    trfShowSymLineInfo,
    trfShowSymOptions,
    trfShowTypeBaseId,
    trfShowTypeAlloc
  }
) ## Default based configuration for the tree repr printing functions

const compactTReprConf* =
  block:
    var base = defaultTReprConf
    base.flags.incl trfPackedFields
    base
  ## Compacted tree repr configuration

var implicitTReprConf*: TReprConf = defaultTReprConf ## global
  ## configuration object that is implicitly used by `debugAst` and
  ## `debugType`. Can be used in order to configure behaviour of the
  ## debugging functions that could later be called from `gdb` environment
  ## (`debugAst`, `debugType`, `debugSym`), or sem execution tracer


const IntTypes = {
  tyInt, tyInt8, tyInt16,
  tyInt32, tyInt64, tyFloat, tyFloat32, tyFloat64, tyUInt, tyUInt8,
  tyUInt16, tyUInt32, tyUInt64
}

const CompressedBuiltinTypes = {tyBool, tyChar, tyPointer, tyString} + IntTypes

func contains(rconf: TReprConf, flag: TReprFlag): bool = flag in rconf.flags

proc textRepr(conf: ConfigRef, typ: PType): ColText =
  result.add ($typ.kind)[2..^1] + style.kind
  if not isNil(typ.sym):
    result &= " sk:" & ($typ.sym.kind)[2..^1] + style.kind

  if not isNil(typ.n):
    let t = $typ.n
    if '\n' notin t:
      result &= " " & t + fgRed


template genFields(res: ColText, indent: int, rconf: TReprConf): untyped =
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
    if trfPackedFields in rconf:
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

func format(i: SomeInteger): ColText = $i + style.number

proc ownerChain(sym: PSym): string =
  var own: seq[PSym] = @[sym]
  var sym = sym
  while not sym.owner.isNil():
    sym = sym.owner
    own.add sym

  for idx in countdown(own.high, 0):
    if idx < own.high:
      result.add "."

    result.add own[idx].name.s
    result.add "("
    result.add substr($own[idx].kind, 2)
    result.add ")"

func defaulted(r: TReprConf): bool = trfShowDefaultedFields in r

proc symFields(
    conf: ConfigRef,
    sym: PSym,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText =

  coloredResult(1)
  var res = addr result
  genFields(res[], indent, rconf)

  if trfShowSymFlags in rconf and
     (sym.flags.len > 0 or rconf.defaulted()):
    field("flags", format(sym.flags, 2) + style.setIt)

  if trfShowSymMagic in rconf and
     (sym.magic != mNone or rconf.defaulted()):
    field("magic", substr($sym.magic, 1) + style.setIt)

  if trfShowSymOptions in rconf and
     (0 < sym.options.len or rconf.defaulted()):
    field("opts", format(sym.options, 3) + style.setIt)

  if trfShowSymOffset in rconf and
     (0 <= sym.offset or rconf.defaulted()):
    field("offset", $sym.offset + style.number)

  if trfShowSymPosition in rconf and
     (sym.position != 0 or rconf.defaulted()):
    field("pos", $sym.offset + style.number)

  if trfShowSymTypes in rconf and not sym.typ.isNil():
    field("typ", conf.textRepr(sym.typ))

  if not isNil(conf) and
     trfShowSymLineInfo in rconf and
     (sym.info != unknownLineInfo or rconf.defaulted()):
    field("info", conf.toMsgFilename(sym.info.fileIndex) + style.ident)
    add "(", $sym.info.line + style.number
    add ".", $sym.info.col + style.number
    add ")"

  if trfShowSymOwner in rconf and not sym.owner.isNil():
    field("owner")
    add sym.owner.ownerChain() + style.owner

  if sym.kind in {skLet, skVar, skField, skForVar}:
    if trfShowSymBitsize in rconf and
       (sym.bitsize != 0 or rconf.defaulted()):
      field("bitsize", $sym.bitsize + style.number)

    if trfShowSymAlignment in rconf and
       (sym.alignment != 0 or rconf.defaulted()):
      field("alignment", sym.bitsize.format())



proc treeRepr*(
    conf: ConfigRef,
    sym: PSym,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText =

  coloredResult(1)
  if sym.isNil():
    addi indent, "<nil>" + style.nilIt

  else:
    addi indent, substr($sym.kind, 2) + style.kind
    add symFields(conf, sym, rconf, indent + 2)

proc typFields(
    conf: ConfigRef,
    typ: PType,
    rconf: TReprConf,
    indent: int
  ): ColText =

  let res = addr result
  genFields(res[], indent, rconf)

  if trfShowTypeCallConv in rconf and typ.kind == tyProc:
    field("callConv", $typ.callConv + style.ident)

  if trfShowTypeFlags in rconf and
     (typ.flags.len > 0 or trfShowDefaultedFields in rconf):
    field("flags", format(typ.flags, 2) + style.ident)

  if trfShowTypeAlloc in rconf:
    field("size", typ.size.format())
    if typ.size == -1:
      res[].add " (unknown)" + style.number

    hfield("align", $typ.align + style.number)

  if trfShowTypeOwner in rconf and not typ.owner.isNil():
    field("owner", typ.owner.ownerChain() + style.owner)


proc treeRepr*(
    conf: ConfigRef,
    pnode: PNode,
    rconf: TReprConf = defaultTReprConf,
    indent: int = 0
  ): ColText

proc treeRepr*(
    conf: ConfigRef,
    typ: PType,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText =

  coloredResult(1)

  var res = addr result
  genFields(res[], indent, rconf)

  proc aux(typ: PType, level: int) =
    let indent = level * 2 + indent
    if typ.isNil():
      addi indent, "<nil>" + style.nilIt

    else:
      addi indent, substr($typ.kind, 2) + style.kind
      if trfShowTypeBaseId in rconf:
        hfield("itemId")
        hfield("module",typ.itemId.module.format())
        hfield("item", typ.itemId.item.format())

      add typFields(conf, typ, rconf, indent + 2)

      if 0 < len(typ.sons):
        add "\n"
        addi indent + 2, "sons:"
        for sub in typ.sons:
          add "\n"
          aux(sub, level + 2)

      if not typ.n.isNil():
        add "\n"
        addi indent + 2, "n:\n"
        add treeRepr(conf, typ.n, rconf, indent = indent + 4)

      if not typ.sym.isNil():
        add "\n"
        addi indent + 2, "sym:\n"
        add treeRepr(conf, typ.sym, rconf, indent = indent + 4)

  aux(typ, 0)



proc treeRepr*(
    conf: ConfigRef,
    pnode: PNode,
    rconf: TReprConf = defaultTReprConf,
    indent: int = 0
  ): ColText =
  ## .. include:: tree_repr_doc.rst

  coloredResult(1)


  let indentIncrease = indent
  var
    visited: Table[int, int]
    res = addr result
    nodecount = 0

  proc aux(n: PNode, idx: seq[int]) =
    var indent = indentIncrease
    genFields(res[], indent, rconf)
    addIndent(indentIncrease)
    indent += 2
    if 0 < idx.len:
      var first = true
      for idxIdx, pos in idx:
        if idx.len - rconf.maxPath <= idxIdx:
          indent += tern(first, 0, 1)

          case pos:
            of 0 .. 9: indent += 1
            of 10 .. 99: indent += 2
            else: indent += 3

          if not first:
            add "." + style.dim

          first = false
          add $pos + style.dim

        else:
          indent += 2
          add "  "

      if 0 < rconf.maxPath:
        add " "
        inc indent

    if isNil(n):
      add "<nil>" + style.nilIt
      return

    elif not (n.kind == nkEmpty and n.safeLen == 0) and
         # empty nodes can be reused. Only check for visitation if it is
         # not an empty (completely empty) node
         cast[int](n) in visited:

      if trfIndexVisisted in rconf:
        add "<visited "
        add substr($n.kind, 2) + style.number
        add " at "
        add $visited[cast[int](n)] + style.dim
        add ">"

      else:
        add "<visited>"

      return

    elif idx.len > rconf.maxDepth:
      add " ..."
      return

    visited[cast[int](n)] = nodecount
    add substr($n.kind, 2) + style.kind

    if trfIndexVisisted in rconf:
      add " "
      add $nodecount + style.dim

    inc nodecount

    when defined(useNodeids):
      if trfShowNodeIds in rconf:
        hfield("nid")
        add $n.id

    let hasComment = trfShowNodeComments in rconf and n.comment.len > 0

    proc addComment(sep: bool = true) =
      if hasComment:
        var nl = false
        for line in split(n.comment.strip(leading = false), '\n'):
          if nl: add "\n"
          nl = true

          addi indent, "  # " + style.comment
          add line + style.comment

        add "\n"

      elif sep:
        add " "

    proc addFlags() =
      if trfShowNodeFlags in rconf and n.flags.len > 0:
        field("nflags", format(n.flags, 2) + style.setIt)

      if trfShowNodeTypes in rconf and not n.typ.isNil():
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
              add " != sym.typ" + style.err

          else:
            field("typ", conf.textRepr(n.typ))

    if not conf.isNil() and trfShowNodeLineInfo in rconf and
       n.info != unknownLineInfo:
      field("info", conf.toMsgFilename(n.info.fileIndex) + style.ident)
      add "("
      add $n.info.line + style.number
      add "."
      add $n.info.col + style.number
      add ")"

    case n.kind:
      of nkStrKinds:
        add " "
        add "\"" & n.strVal + style.strLit & "\""
        addFlags()
        if hasComment: add "\n"
        addComment()

      of nkCharLit .. nkUInt64Lit:
        add " "
        add $n.intVal + style.number
        addFlags()
        if hasComment: add "\n"
        addComment()

      of nkFloatLit .. nkFloat128Lit:
        add " "
        add $n.floatVal + style.floatLit
        addFlags()
        if hasComment: add "\n"
        addComment()

      of nkIdent:
        add " "
        add n.ident.s + style.ident
        addFlags()
        if hasComment: add "\n"
        addComment()

      of nkSym:
        add " "
        add n.sym.name.s + style.ident
        hfield "sk", substr($n.sym.kind, 2) + style.kind

        add symFields(conf, n.sym, rconf, indent)
        addFlags()
        addComment()

      of nkCommentStmt:
        addFlags()
        add "\n"
        if hasComment: add "\n"
        addComment()

      of nkError:
        if isNil(conf):
          field("err", substr($n.errorKind(), 4) + style.errKind)

        else:
          let report = conf.getReport(n).semReport
          field("err", substr($report.kind, 4) + style.errKind)
          hfield("errid", $n.reportId.int + style.err)

        let (file, line, col) = n.compilerInstInfo()
        field("info", formatPath(conf, file) + style.ident)
        add "(", $line + style.number
        add ".", $col + style.number
        add ")"

      else:
        discard


    if n.kind notin {nkNone .. nkNilLit, nkCommentStmt}:
      addFlags()
      if n.len > 0:
        add "\n"

      if hasComment:
        addComment(false)
        add "\n"

      for newIdx, subn in n:
        if trfSkipAuxError in rconf and n.kind == nkError and newIdx in {1, 2}:
          continue

        aux(subn, idx & newIdx)


        if idx.len + 1 > rconf.maxDepth:
          break

        if newIdx > rconf.maxLen:
          break

        if newIdx < n.len - 1:
          add "\n"

  aux(pnode, @[])

{.pragma: dbg, deprecated: "Debug proc, should not be used in the final build".}

proc debugAst*(it: PNode) {.exportc, dbg.} =
  ## Print out tree representation of the AST node.
  ##
  ## .. note:: there is no `ConfigRef` argument, and because of that some
  ##     information cannot be fully retrieved from the AST (such as full
  ##     paths of the FileIndex entries).
  ##
  ## .. tip:: This proc is annotated with `{.exportc.}` which means it's
  ##     mangled name exactly the same - `debugAst` and you can call it
  ##     from the `gdb` debugging session.
  echo treeRepr(nil, it, implicitTReprConf)

proc debugType*(it: PType) {.exportc, dbg.} =
  ## Print out tree represntation of the type. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  echo treeRepr(nil, it, implicitTReprConf)

proc debugSym*(it: PSym) {.exportc, dbg.} =
  ## Print out tree represntation of the symbol. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  echo treeRepr(nil, it, implicitTReprConf)

proc debug*(it: PNode, tconf: TReprConf = implicitTReprConf) {.dbg.} =
  ## Convenience overload of `debugAst`
  echo treeRepr(nil, it, tconf)

proc debug*(it: PType, tconf: TReprConf = implicitTReprConf) {.dbg.} =
  ## Convenience overload of `debugType`
  echo treeRepr(nil, it, tconf)

proc debug*(it: PSym, tconf: TReprConf = implicitTReprConf) {.dbg.} =
  ## Convenience overload of `debugSym`
  echo treeRepr(nil, it, tconf)

proc debug*(
    conf: ConfigRef, it: PNode, tconf: TReprConf = implicitTReprConf) {.dbg.} =
  ## Print tree representation of the AST
  echo treeRepr(conf, it, tconf)

proc debug*(
    conf: ConfigRef, it: PType, tconf: TReprConf = implicitTReprConf) {.dbg.} =
  ## Print tree representation of the type
  echo treeRepr(conf, it, tconf)

proc debug*(
    conf: ConfigRef, it: PSym, tconf: TReprConf = implicitTReprConf) {.dbg.} =
  ## Print tree reprsentation of the symbol
  echo treeRepr(conf, it, tconf)
