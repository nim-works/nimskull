##[

`treeRepr()` implementation for the `PNode` tree structures. This module
provides helper procedures that can be used for debugging purposes -
dumping compiler IR in the textual form.

For each IR type (``PType``, ``PSym``, ``PNode``) there are several procs
defined:

- `treeRepr` - main implementation to generate the `ColText` chunk for the
  object.

- `debug` - convenience overloads that print generated text immediately.
  There are two version of the `debug` - one that accept `ConfigRef`
  object, and one that can work without it. Both call `treeRepr`
  internally, but with `ConfigRef` present more information can be printed.

- `debugAst`, `debugSym`, `debugType` - procs added for use in `gdb`
  debugging. They have `{.exportc.}` annotations, so can be used directly
  by name there

`treeRepr` and all other procedures in the module accept `TReprConf` object
(default value is provided via `implicitTReprConf` global, for gdb procs it
is used implicitly) that controls which fields specifically will be
printed.

Configuration object also includes callbacks that would allow you to
provide additional information for the printed targets. For example, you
want to perform some analysis, and don't have `ConfigRef` object passed
around (because your code *only* collects some data from the nodes). But
then you need to figure out the actual location of where node comes from.
In that case you simply assign to `implicitTReprConf.extraNodeInfo`, and
next `debug` you call will provide needed information.

.. code::nim

  implicitTReprConf.extraNodeInfo = proc(node: PNode): ColText =
    result.add "node location " & (config $ node.info)

You can change implicit trepr configuration in the necessary modules or in
`compiler/nim.nim`, right after creation of the config reference.

If you need to figure out what specific flag controls which field (without
going into the source code) add `trfDescFlag` to the configuration.

]##


import
  compiler/ast/[
    ast,
    reports,
    renderer,
    lineinfos,
    errorhandling
  ],
  compiler/front/[
    options,
    msgs
  ],
  experimental/colortext,
  std/[
    tables,
    strutils,
    strformat,
    math
  ]

import std/options as std_options

export colortext

type
  TReprFlag* = enum
    ## Configuration options for treeRepr debugging
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
    trfShowKindTypes ## Show `T:`, `S:`, `N:` prefixes for the enum kinds
    ## when printed in the tree. This is useful to distinguish between
    ## various structure types when printing node that contains symbols
    ## which also have types.
    trfShowDefaultedFields ## Show fields with defaulted values
    trfShowNilFields ## Show fields with `nil` values

    trfShowSymFlags ## Show symbol `.flags` field
    trfShowSymLineInfo ## Line information
    trfShowSymName
    trfShowSymTypes ## Flat render of the symbol type
    trfShowSymMagic ## used symbol magic
    trfShowSymKind ## `.kind`
    trfShowSymOwner ## Full chain of the symbol owners
    trfShowSymOptions ## `.options`
    trfShowSymPosition ## `.position`
    trfShowSymOffset ## `.offset`
    trfShowSymBitsize ## `.bitsize`
    trfShowSymAlignment ## `.alignment`
    trfShowSymAst ## `.ast` from the symbol
    trfShowFullSymTypes ## Full render of the symbol type on one level.
    ## Multiple levels are not printed
    trfShowSymId ## `module` and `item` from `itemId` field

    trfShowTypeCallConv ## Calling convention
    trfShowTypeFlags ## `.flags`
    trfShowTypeSym ## `.sym`
    trfShowTypeAst ## `.n` of the `PType` object
    trfShowTypeAlloc ## `.align` and `.size` fields. They represent a
    ## single logical chunk of information, and pinter together
    trfShowTypeOwner ## Full chain of the type owner
    trfShowTypeId ## `module` and `item` from `itemId` field

    trfShowNodeLineInfo ## Node location information
    trfShowNodeFlags ## `.flags`
    trfShowNodeIds ## `.id` field, available when compiler is built with `-d:nodeIds`
    trfShowNodeComments ## `.comment` field
    trfShowNodeErrors ## Embedded `nkError` reports
    trfShowNodeTypes
    trfShowFullNodeTypes ## Show first level of the node types expansion.
    ## NOTE: only shows the first level to avoid infinte recursion
    trfDescFlag ## For each formatted field, show name of the flag that
                ## controls it

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

    extraNodeInfo*: proc(node: PNode): ColText ## Additional information
    ## for node, symbol or type printing. By default these procs are not
    ## implemented and ignored. If the procedure is not nil it is called at
    ## the start of each entry (before mandatory fields and nested
    ## elements) and results are indented to match the surroundings.

    extraSymInfo*: proc(sym: PSym): ColText ## Extra info for symbol
    extraTypInfo*: proc(typ: PType): ColText ## Extra info for type


const treeReprAllFields* = {trfShowSymFlags .. trfShowNodeTypes} ## Set of
  ## flags to print all fields in all tree reprs

const style = (
  kind: fgBlue,
  nilIt: fgRed,
  ident: fgCyan,
  setIt: termFg(5, 1, 5),
  number: fgCyan,
  strLit: fgYellow,
  floatLit: fgMagenta,
  dim: termFg(15),
  errKind: termFg(5, 2, 0),
  err: fgRed,
  comment: termFg(4, 2, 1),
  owner: termFg(1, 3, 0),
  identLit: fgGreen
)

let defaultTReprConf* = TReprConf(
  maxDepth: 120,
  maxLen: 30,
  maxPath: 1,
  flags: {
    trfReportInfo,
    trfSkipAuxError,
    trfShowKindTypes,
  } + treeReprAllFields - {
    trfPackedFields,
    trfShowNodeLineInfo,
    trfShowSymLineInfo,
    trfShowSymOptions,
    trfShowSymPosition,
    trfShowTypeId,
    trfShowTypeAlloc,
    trfShowSymAst,
    trfShowSymId,
    trfShowSymAst,
    trfShowTypeAst
  }
) ## Default based configuration for the tree repr printing functions

let onlyStructureTReprConf* =
  block:
    var base = defaultTReprConf
    base.flags = {trfShowNodeComments}
    base.maxPath = 0
    base

func incl*(conf: var TReprConf, flag: TReprFlag | set[TReprFlag]) =
  conf.flags.incl flag

func excl*(conf: var TReprConf, flag: TReprFlag | set[TReprFlag]) =
  conf.flags.excl flag

func `+`*(conf: TReprConf, flag: TReprFlag | set[TReprFlag]): TReprConf =
  result = conf
  result.incl flag

func `-`*(conf: TReprConf, flag: TReprFlag | set[TReprFlag]): TReprConf =
  result = conf
  result.excl flag

let compactTReprConf* = defaultTReprConf + trfPackedFields ## Compacted tree repr configuration

let verboseTReprConf* =
  block:
    var base = defaultTReprConf + {
      low(TReprFlag) .. high(TReprFlag) } - { trfPackedFields, trfDescFlag }
    base.maxPath = 40
    base
  ## Show absolutely everything

var
  compilerTraceReprConf*: TReprConf =
    block:
      var base = defaultTReprConf
      base.maxDepth = 3
      base.maxLen = 5
      base.flags = {
        trfPackedFields,
        trfSkipAuxError,
        trfShowKindTypes,
        trfShowSymName,
        trfShowSymTypes,
        trfShowSymKind,
        trfShowNodeErrors,
        trfShowNodeIds,
        trfShowNodeLineInfo
      }
      base
    ## default tree repr config for compiler tracing, meant to be compact as
    ## there is a lot of tracing spam.
  implicitTReprConf*: TReprConf = defaultTReprConf
    ## global configuration object that is implicitly used by `debugAst` and
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
func packed(conf: TReprConf): bool = trfPackedFields in conf

func maybeIndent(conf: TReprConf, ind: int): int =
  if conf.packed(): 0 else: ind

proc textRepr(conf: ConfigRef, typ: PType): ColText =
  result.add ($typ.kind)[2..^1] + style.kind
  if not isNil(typ.sym):
    result &= " sk:" & ($typ.sym.kind)[2..^1] + style.kind

  if not isNil(typ.n):
    let t = $typ.n
    if '\n' notin t:
      result &= " " & t + fgRed

proc nameField(res: var ColText, cond: TReprFlag, rconf: TReprConf) =
  if trfDescFlag in rconf and cond != trfDescFlag:
    res.addf(" ($#) ", $cond)


template genFields(res: ColText, indent: int, rconf: TReprConf): untyped =
  proc hfield(name: string, cond: TReprFlag, text: ColText = default ColText) =
    if cond in rconf:
      res.add " "
      res.add name
      res.add ":"
      nameField(res, cond, rconf)
      res.add text


  proc vfield(name: string, cond: TReprFlag, text: ColText = default ColText) =
    if cond in rconf:
      res.newline()
      res.addIndent(indent, 1)
      res.add alignLeft(name & ":", 9)
      nameField(res, cond, rconf)
      res.add text

  proc field(name: string, cond: TReprFlag, text: ColText = default ColText) =
    if rconf.packed():
      hfield(name, cond, text)
    else:
      vfield(name, cond, text)

func formatKind[I](rconf: TReprConf, kind: I, sub: int = 2): string =
  if trfShowKindTypes in rconf:
    when kind is TNodeKind: result = "N:" & substr($kind, 2)
    elif kind is TSymKind: result = "S:" & substr($kind, 2)
    elif kind is TTypeKind: result = "T:" & substr($kind, 2)
    elif kind is ReportKind: result = "RK:" & substr($kind, 4)
    else: result = $kind
  else:
    result = substr($kind, sub)

proc format[I](s: set[I], sub: int): string =
  var first = true
  result = "{"
  for item in s:
    if not first: result.add ", "
    result.add substr($item, sub)
    first = false

  result.add "}"

func format(i: SomeInteger): ColText = $i + style.number

proc ownerChain(rconf: TReprConf, sym: PSym): string =
  var chain: seq[PSym] = @[sym]
  var sym = sym
  while not sym.owner.isNil():
    sym = sym.owner
    chain.add sym

  for idx in countdown(chain.high, 0):
    if idx < chain.high:
      result.add "."

    result.add "'"
    result.add chain[idx].name.s
    result.add "'("
    result.add rconf.formatKind(chain[idx], 2)
    result.add ")"

func defaulted(r: TReprConf): bool = trfShowDefaultedFields in r

proc infoField(
    conf: ConfigRef, info: TLineInfo, rconf: TReprConf,
    indent: int, name: string,
    flag: TReprFlag
  ): ColText =
  var res = addr result
  genFields(res[], indent, rconf)
  if info == unknownLineInfo:
    field(name, flag, "unknown" + style.err)

  elif isNil(conf):
    field(name, flag)
    hfield("fileIndex", flag, $info.fileIndex.int + style.number)
    hfield("line", flag, $info.line + style.number)
    hfield("col", flag, $info.col + style.number)

  else:
    field(name, flag, "$#($#, $#)" % [
      conf.toMsgFilename(info.fileIndex) + style.ident,
      $info.line + style.number,
      $info.col + style.number
    ])


proc treeRepr*(
    conf: ConfigRef,
    pnode: PNode,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText

proc treeRepr*(
    conf: ConfigRef,
    sym: PSym,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText

proc treeRepr*(
    conf: ConfigRef,
    typ: PType,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText

proc symFields(
    conf: ConfigRef,
    sym: PSym,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0,
    inNode: bool = false
  ): ColText =

  coloredResult(1)
  var res = addr result
  genFields(res[], indent, rconf)

  if not rconf.extraSymInfo.isNil():
    let text = rconf.extraSymInfo(sym)
    if 0 < len(text):
      result.add text.indent(indent)

  if trfShowSymName in rconf:
    field("name.s", trfShowSymName, sym.name.s + style.ident)
    hfield("sym.id", trfShowSymName, $sym.id + style.number)
    hfield("name.id", trfShowSymName, $sym.name.id + style.number)

  if sym.flags.len > 0 or rconf.defaulted():
    field("flags", trfShowSymFlags, format(sym.flags, 2) + style.setIt)

  if sym.magic != mNone or rconf.defaulted():
    field("magic", trfShowSymMagic,
          rconf.formatKind(sym.magic, 1) + style.setIt)

  if trfShowSymId in rconf:
    field("itemId", trfShowSymId)
    hfield("module", trfShowSymId, sym.itemId.module.format())
    hfield("item", trfShowSymId, sym.itemId.item.format())

  if 0 < sym.options.len or rconf.defaulted():
    field("opts", trfShowSymOptions, format(sym.options, 3) + style.setIt)

  if 0 <= sym.offset or rconf.defaulted():
    field("offset", trfShowSymOffset, $sym.offset + style.number)

  if sym.position != 0 or rconf.defaulted():
    field("position", trfShowSymPosition, $sym.position + style.number)

  if trfShowSymLineInfo in rconf:
    addi rconf.maybeIndent(indent), infoField(
      conf, sym.info, rconf, indent, tern(
        inNode, "symInfo", "info"), trfShowSymLineInfo)

  if not sym.owner.isNil() or trfShowNilFields in rconf:
    field("owner", trfShowSymOwner, rconf.ownerChain(sym.owner) + style.owner)

  if sym.kind in {skLet, skVar, skField, skForVar}:
    if sym.bitsize != 0 or rconf.defaulted():
      field("bitsize", trfShowSymBitsize, $sym.bitsize + style.number)

    if sym.alignment != 0 or rconf.defaulted():
      field("alignment", trfShowSymAlignment, sym.bitsize.format())

  if trfShowFullSymTypes in rconf:
    var tmp = rconf
    # Exclude one level of the symbol types in order to avoid infinte
    # recursion
    tmp.excl trfShowFullSymTypes
    field("typ", trfShowFullSymTypes)
    add "\n"
    add conf.treeRepr(sym.typ, tmp, indent = indent + 2)

  elif not sym.typ.isNil() or trfShowNilFields in rconf:
    field("typ", trfShowSymTypes, conf.textRepr(sym.typ))

  if trfShowSymAst in rconf:
    var rconf = compactTReprConf
    rconf.excl trfShowSymAst
    field("ast", trfShowSymAst)
    result.add "\n"
    result.add treeRepr(
      conf, sym.ast, rconf, indent = indent + 2)



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
    addi indent, rconf.formatKind(sym.kind) + style.kind
    if trfShowSymName notin rconf:
      # If the name is not show in verbose manner as a field, print it
      # beforehand as a regular string.
      add " \""
      add sym.name.s + style.identLit
      add "\""

    add symFields(conf, sym, rconf, indent + 2)

proc typFields(
    conf: ConfigRef,
    typ: PType,
    rconf: TReprConf,
    indent: int
  ): ColText =

  let res = addr result
  genFields(res[], indent, rconf)

  if not rconf.extraTypInfo.isNil():
    let text = rconf.extraTypInfo(typ)
    if 0 < len(text):
      result.add text.indent(indent)


  if typ.kind == tyProc:
    field("callConv", trfShowTypeCallConv, $typ.callConv + style.ident)

  if typ.flags.len > 0 or trfShowDefaultedFields in rconf:
    field("flags", trfShowTypeFlags, format(typ.flags, 2) + style.ident)

  if trfShowTypeAlloc in rconf:
    field("size", trfShowTypeAlloc, typ.size.format())
    if typ.size == -1:
      res[].add " (unknown)" + style.number

    hfield("align", trfShowTypeAlloc, $typ.align + style.number)

  if typ.owner != nil:
    field("owner", trfShowTypeOwner, rconf.ownerChain(typ.owner) + style.owner)


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
      addi indent, rconf.formatKind(typ.kind) + style.kind
      field("itemId", trfShowTypeId)
      hfield("module", trfShowTypeId, typ.itemId.module.format())
      hfield("item", trfShowTypeId, typ.itemId.item.format())

      add typFields(conf, typ, rconf, indent + 2)

      if 0 < len(typ.sons):
        add "\n"
        addi indent + 2, "sons:"
        for sub in typ.sons:
          add "\n"
          aux(sub, level + 2)

      if (not typ.n.isNil() or trfShowNilFields in rconf) and
         (trfShowTypeAst in rconf):
        add "\n"
        addi indent + 2, "n:\n"
        add treeRepr(conf, typ.n, rconf, indent = indent + 4)

      if (not typ.sym.isNil() or trfShowNilFields in rconf) and
         (trfShowTypeSym in rconf):
        add "\n"
        addi indent + 2, "sym:\n"
        add treeRepr(conf, typ.sym, rconf, indent = indent + 4)

  aux(typ, 0)



proc cyclicTreeAux(n: PNode, visited: var seq[PNode], count: var int): bool =
  if n.isNil(): return
  for v in visited:
    if v == n:
      return true

  inc count
  if n.kind notin {nkEmpty..nkNilLit}:
    visited.add(n)
    for nSon in n.sons:
      if cyclicTreeAux(nSon, visited, count):
        return true

    discard visited.pop()

proc cyclicTree(n: PNode): tuple[cyclic: bool, count: int] =
  var visited: seq[PNode] = @[]
  result.cyclic = cyclicTreeAux(n, visited, result.count)

proc treeRepr*(
    conf: ConfigRef,
    pnode: PNode,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText =
  ## .. include:: tree_repr_doc.rst

  coloredResult(1)


  let indentIncrease = indent
  var
    visited: Table[int, int]
    res = addr result
    nodecount = 0

  let (cyclic, maxNum) = cyclicTree(pnode)
  let numerate = trfIndexVisisted in rconf or cyclic

  let numWidth = log10(maxNum.float).int + 1
  let pad = tern(numerate, 3 + numWidth, 0)

  proc aux(n: PNode, idx: seq[int]) =
    if numerate:
      add "["
      add ($nodecount + style.dim) |>> numWidth
      add "] "

    inc nodecount

    var indent = indentIncrease
    genFields(res[], indent, rconf)
    addIndent(indentIncrease)
    indent += 2 + pad
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

      if numerate:
        add "<visited "
        add rconf.formatKind(n.kind) + style.number
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
    add rconf.formatKind(n.kind) + style.kind

    if trfShowNodeIds in rconf:
      hfield("nid", trfShowNodeIds)
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
      if not rconf.extraNodeInfo.isNil():
        let text = rconf.extraNodeInfo(n)
        if 0 < len(text):
          add text.indent(indent)

      if trfShowNodeLineInfo in rconf:
        addi(
          rconf.maybeIndent(indent), infoField(
          conf, n.info, rconf, indent, "info", trfShowNodeLineInfo))

      if n.flags.len > 0:
        field("nflags", trfShowNodeFlags, format(n.flags, 2) + style.setIt)

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
              field("typ", trfShowNodeTypes, " != sym.typ" + style.err)
              add "\n"
              add conf.treeRepr(n.typ, rconf, indent = indent + 2)

          else:
            field("typ", trfShowNodeTypes)
            add "\n"
            add conf.treeRepr(n.typ, rconf, indent = indent + 2)

    proc postLiteral() =
      addFlags()
      if hasComment: add "\n"
      addComment()

    case n.kind:
      of nkStrKinds:
        add " "
        add "\"" & n.strVal + style.strLit & "\""
        postLiteral()

      of nkCharLit .. nkUInt64Lit:
        add " "
        add $n.intVal + style.number
        postLiteral()

      of nkFloatLit .. nkFloat128Lit:
        add " "
        add $n.floatVal + style.floatLit
        postLiteral()

      of nkIdent:
        add " \""
        add n.ident.s + style.identLit
        add "\""
        hfield("ident.id", trfDescFlag, $n.ident.id + style.number)
        postLiteral()

      of nkSym:
        add " \""
        add n.sym.name.s + style.identLit
        add "\""
        hfield("sk", trfDescFlag, rconf.formatKind(n.sym.kind) + style.kind)

        add symFields(conf, n.sym, rconf, indent, inNode = true)
        addFlags()
        addComment()

      of nkCommentStmt:
        addFlags()
        add "\n"
        if hasComment: add "\n"
        addComment()

      of nkError:
        if isNil(conf):
          field(
            "err", trfDescFlag,
            rconf.formatKind(n.errorKind()) + style.errKind)

        else:
          let report = conf.getReport(n).semReport
          field(
            "err", trfDescFlag,
            rconf.formatKind(report.kind) + style.errKind)
          hfield("errid", trfDescFlag, $n.reportId.int + style.err)

        let (file, line, col) = n.compilerInstInfo()
        field("info", trfDescFlag, formatPath(conf, file) + style.ident)
        add "(", $line + style.number
        add ",", $col + style.number
        add ")"

      of nkType:
        postLiteral()


      else:
        discard


    if n.kind notin {nkNone .. nkNilLit, nkCommentStmt}:
      addFlags()
      if n.len > 0:
        add "\n"

      if hasComment:
        addComment(false)
        if len(n) == 0:
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

{.pragma:
  dbg,
  deprecated: "DEBUG proc, should not be used in the final build!",
  noSideEffect
  .}

var implicitDebugConfRef: ConfigRef

template hack(body: untyped): untyped =
  {.cast(noSideEffect).}:
    body

proc setImplicitDebugConfRef*(conf: ConfigRef) {.dbg.} =
  ## Set implicit debug config reference.
  hack:
    implicitDebugConfRef = conf

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
  hack:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

proc debugType*(it: PType) {.exportc, dbg.} =
  ## Print out tree represntation of the type. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  hack:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

proc debugSym*(it: PSym) {.exportc, dbg.} =
  ## Print out tree represntation of the symbol. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  hack:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

proc debug*(it: PNode | PSym | PNode, tconf: TReprConf) {.dbg.} =
  ## Convenience overload of `debugAst`
  hack:
    echo treeRepr(implicitDebugConfRef, it, tconf)

proc debug*(
    conf: ConfigRef, it: PNode | PType | PSym, tconf: TReprConf) {.dbg.} =
  ## Print tree representation of the AST
  hack:
    echo treeRepr(conf, it, tconf)

proc debug*(it: PNode | PSym | PType) {.dbg.} =
  ## Convenience overload of `debugAst`
  hack:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

proc debug*(conf: ConfigRef, it: PNode | PType | PSym) {.dbg.} =
  ## Print tree representation of the AST
  hack:
    echo treeRepr(conf, it, implicitTReprConf)

proc inLines*(node: PNode, lrange: Slice[int]): bool {.dbg.} =
  hack:
    lrange.a <= node.info.line.int and node.info.line.int <= lrange.b

proc inFile*(
    conf: ConfigRef,
    node: PNode | PSym,
    file: string,
    lrange: Slice[int] = low(int) .. high(int)
  ): bool {.dbg.} =
  ## Check if file name of the `info` has `filename` in it, and that node
  ## is located in the specified line range. For debugging purposes as it
  ## is pretty slow.
  hack:
    return file in toFilename(conf, node.info) and
           node.info.line.int in lrange

proc inFile*(
    node: PNode | PSym,
    file: string,
    lrange: Slice[int] = low(int) .. high(int)
  ): bool {.dbg.} =
  ## Check if file name of the `info` has `filename` in it. For debugging
  ## purposes as it is pretty slow. Should be used like this:
  ##
  ## ..code-block::nim
  ##
  ##   if inFile(conf, node.info, "filename"):
  ##     debug node # For example
  ##
  ## This proc requries implicit debug config to be set, since information
  ## about nodde file names is not available otherwise.
  ##
  ## .. warning:: Requires implicit debug configuration to be set
  ##
  ## .. note:: It checks whether `file` is a substring of the full path, so
  ##           you can only write the a part of the name, like `"osproc"` or
  ##           `"strutils"`
  hack:
    return file in toFilename(implicitDebugConfRef, node.info) and
           node.info.line.int in lrange

func inDebug*(): bool =
  ## Check whether current implicit compiler configuration is in the
  ## 'debug' range.
  ##
  ## .. warning:: Requires implicit debug configuration to be set
  {.cast(noSideEffect).}:
    assert not isNil(implicitDebugConfRef)
    return implicitDebugConfRef.isDefined("nimCompilerDebug")
