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
    ast_types,
    ast_parsed_types,
    ast_query,
    ast,
    reports,
    lineinfos
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    idioms
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
    trfReportInfo ## Show information about embedded errors
    trfPackedFields ## Put fields horizontally instead of arranging them
                    ## out veritcally. Should be used only when a small
                    ## number of total fields is enabled, otherwise output
                    ## will be unreadable.
    trfSkipAuxError ## Skip 'data store' fields of the `nkError` node -
    ## elements `[1..2]` are used to store additional metadata and are
    ## rarely needed for printing.
    trfIndexVisisted ## Enumerate each visited node when printing the
    ## `PNode` tree
    trfShowKindTypes ## Show `T:`, `S:`, `N:` prefixes for the enum kinds
    ## when printed in the tree. This is useful to distinguish between
    ## various structure types when printing node that contains symbols
    ## which also have types.
    trfShowDefaultedFields ## Show fields with defaulted values
    trfShowNilFields ## Show fields with `nil` values
    trfShowDebugLocation ## Show location of the `debug` calls in the
    ## source code - useful if you have multiple debugging places and need
    ## to diferentiate between them.

    trfShowFullSymTypes ## Full render of the symbol type on one level.
    ## Multiple levels are not printed
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
    trfShowFullSymChoice ## Show all alternative symbols for the
    ## `nkOpenSymChoice` and `nkClosedSymChoice` node kinds
    trfShowSymAst ## `.ast` from the symbol
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
    trfShowNodeIds ## `.id` field
    trfShowNodeComments ## `.comment` field
    trfShowNodeErrors ## Embedded `nkError` reports
    trfShowNodeTypes
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
    # trfSkipAuxError,
    trfShowKindTypes,
  } + treeReprAllFields - {
    trfPackedFields,

    trfShowNodeLineInfo,
    # trfShowNodeTypes,

    trfShowSymLineInfo,
    trfShowSymOptions,
    trfShowSymPosition,
    trfShowSymOwner,
    trfShowFullSymChoice,
    trfShowSymId,
    trfShowSymAst,
    trfShowSymTypes,
    trfShowFullSymTypes,
    trfShowSymFlags,

    trfShowTypeOwner,
    trfShowTypeId,
    trfShowTypeSym,
    trfShowTypeAlloc,
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
  implicitCompilerTraceReprConf*: TReprConf =
    block:
      var base = defaultTReprConf
      base.maxDepth = 4
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

template tern(predicate: bool, tBranch: untyped, fBranch: untyped): untyped =
  ## Shorthand for inline if/else. Allows use of conditions in strformat,
  ## simplifies use in expressions. Less picky with formatting
  {.line: instantiationInfo(fullPaths = true).}:
    block:
      if predicate: tBranch else: fBranch

func contains(rconf: TReprConf, flag: TReprFlag): bool = flag in rconf.flags
func packed(conf: TReprConf): bool = trfPackedFields in conf

func maybeIndent(conf: TReprConf, ind: int): int =
  if conf.packed(): 0 else: ind

proc textRepr(conf: ConfigRef, typ: PType): ColText =
  result.add ($typ.kind)[2..^1] + style.kind
  if not isNil(typ.sym):
    result &= " sk:" & ($typ.sym.kind)[2..^1] + style.kind

proc nameField(res: var ColText, cond: TReprFlag, rconf: TReprConf) =
  if trfDescFlag in rconf and cond != trfDescFlag:
    res.addf(" ($#) ", $cond)


template genFields(res: ColText, indent: int, rconf: TReprConf): untyped =
  proc hfield(name: string, cond: TReprFlag, text = default ColText,
              rc = rconf) =
    if cond in rc:
      res.add " "
      res.add name
      res.add ":"
      nameField(res, cond, rc)
      res.add text

  proc vfield(name: string, cond: TReprFlag, text = default ColText,
              rc = rconf) =
    if cond in rc:
      res.newline()
      res.addIndent(indent, 1)
      res.add alignLeft(name & ":", 9)
      nameField(res, cond, rc)
      res.add text

  proc field(name: string, cond: TReprFlag, text = default ColText,
             rc = rconf) =
    if rc.packed():
      hfield(name, cond, text, rc)
    else:
      vfield(name, cond, text, rc)

func formatKind[I](rconf: TReprConf, kind: I, sub: int = 2): string =
  if trfShowKindTypes in rconf:
    when kind is TNodeKind: result = "N:" & substr($kind, 2)
    elif kind is ParsedNodeKind: result = "N:" & substr($kind, 3)
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
  while not sym.isNil() and not sym.owner.isNil():
    sym = sym.owner
    chain.add sym

  for idx in countdown(chain.high, 0):
    if idx < chain.high:
      result.add "."

    let s = chain[idx]
    if s.isNil():
      result.add "<nil>"
    else:
      result.add "'"
      result.add s.name.s
      result.add "'("
      result.add rconf.formatKind(s.kind, 2)
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
    id: PIdent,
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
    if trfShowSymAst notin rconf:
      if sym.ast != nil:
        hfield("sym.ast.id", trfShowSymName, $sym.ast.id + style.number)
      elif trfShowNilFields in rconf:
        hfield("sym.ast.id", trfShowSymName, "<nil>" + style.nilIt)
      else:
        discard

  if trfShowSymKind in rconf:
    field("kind", trfShowSymName, rconf.formatKind(sym.kind) + style.kind)

  if sym.flags.len > 0 or rconf.defaulted():
    field("flags", trfShowSymFlags, format(sym.flags, 2) + style.setIt)

  if sym.magic != mNone or rconf.defaulted():
    hfield("magic", trfShowSymMagic,
           rconf.formatKind(sym.magic, 1) + style.setIt)

  if trfShowSymId in rconf:
    field("itemId", trfShowSymId)
    hfield("module", trfShowSymId, sym.itemId.module.format())
    hfield("item", trfShowSymId, sym.itemId.item.format())

  if sym.options.len > 0 or rconf.defaulted():
    field("opts", trfShowSymOptions, format(sym.options, 3) + style.setIt)

  if sym.offset >= 0 or rconf.defaulted():
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

  let fieldCondition =
    if trfShowFullSymTypes in rconf:
      trfShowFullSymTypes
    elif inNode:
      trfShowNodeTypes
    else:
      trfShowSymTypes
  if sym.typ.isNil:
    if trfShowNilFields in rconf:
      field("typ", fieldCondition, conf.textRepr(sym.typ))
  else:
    if fieldCondition == trfShowFullSymTypes or
        (inNode and trfShowNodeTypes in rconf):
      var tmp = rconf
      # Exclude one level of the symbol types in order to avoid infinite
      # recursion
      tmp.excl trfShowFullSymTypes
      field("typ", fieldCondition, rc = tmp)
      add "\n"
      add conf.treeRepr(sym.typ, tmp, indent = indent + 2)
    elif not inNode:
      field("typ", fieldCondition, conf.textRepr(sym.typ))

  if trfShowSymAst in rconf:
    var rconf = rconf
    rconf.excl trfShowSymAst
    field("ast", trfShowSymAst)
    result.add "\n"
    result.add treeRepr(
      conf, sym.ast, rconf, indent = indent + 2)


proc treeRepr*(
    conf: ConfigRef,
    id: PIdent,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText =

  coloredResult(1)
  let res = addr result
  genFields(res[], indent, rconf)
  if id.isNil():
    addi indent, "<nil>" + style.nilIt
  else:
    addi indent, ""
    if trfShowSymName in rconf:
      add " \""
      add id.s + style.identLit
      add "\""

    if trfShowSymId in rconf:
      hfield("id", trfShowSymId, $id.id + style.ident)


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

  if rconf.extraTypInfo != nil:
    let text = rconf.extraTypInfo(typ)
    if 0 < len(text):
      result.add text.indent(indent)

  if typ.flags.len > 0 or trfShowDefaultedFields in rconf:
    hfield("flags", trfShowTypeFlags, format(typ.flags, 2) + style.ident)

  if typ.kind == tyProc:
    field("callConv", trfShowTypeCallConv, $typ.callConv + style.ident)

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
      if trfShowTypeSym notin rconf and typ.sym != nil:
        # If the name is not show in verbose manner as a field, print it
        # beforehand as a regular string.
        add " \""
        add typ.sym.name.s + style.identLit
        add "\""
      field("itemId", trfShowTypeId)
      hfield("module", trfShowTypeId, typ.itemId.module.format())
      hfield("item", trfShowTypeId, typ.itemId.item.format())

      add typFields(conf, typ, rconf, indent + 2)

      if len(typ.sons) > 0:
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

proc cyclicTreeAux(n: ParsedNode, visited: var seq[ParsedNode], count: var int): bool =
  if n.isNil(): return
  for v in visited:
    if v == n:
      return true

  inc count
  case n.kind
  of {pnkEmpty..pnkCustomLit}:
    discard
  of pnkParsedKindsWithSons:
    visited.add(n)

    for nSon in n.sons:
      if cyclicTreeAux(nSon, visited, count):
        return true

    discard visited.pop()
  else:
    discard

proc cyclicTree(n: ParsedNode): tuple[cyclic: bool, count: int] =
  var visited: seq[ParsedNode] = @[]
  result.cyclic = cyclicTreeAux(n, visited, result.count)

proc cyclicTreeAux(n: PNode, visited: var seq[PNode], count: var int): bool =
  if n.isNil(): return
  for v in visited:
    if v == n:
      return true

  inc count
  case n.kind
  of {nkEmpty..nkNilLit}:
    discard
  else:
    visited.add(n)

    let sons =
      case n.kind
      of nkError:
        @[n.diag.wrongNode]
      else:
        @[]

    for nSon in sons:
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
      if rconf.extraNodeInfo != nil:
        let text = rconf.extraNodeInfo(n)
        if 0 < len(text):
          add text.indent(indent)

      if trfShowNodeLineInfo in rconf:
        addi(
          rconf.maybeIndent(indent), infoField(
          conf, n.info, rconf, indent, "info", trfShowNodeLineInfo))

      if n.flags.len > 0:
        hfield("nflags", trfShowNodeFlags, format(n.flags, 2) + style.setIt)

      if trfShowNodeTypes in rconf and not n.typ.isNil():
        if (
          n.typ.kind notin CompressedBuiltinTypes or
          (n.kind in nkIntKinds and n.typ.kind notin IntTypes)
        ):
          var tmp = rconf
          # Exclude one level of the symbol types in order to avoid infinite
          # recursion
          tmp.excl trfShowFullSymTypes
          tmp.excl trfShowTypeSym
          if n.kind == nkSym and n.sym != nil:
            if n.sym.typ == n.typ:
              # Type will be printed in `symFlags`
              discard
            else:
              field("typ", trfShowNodeTypes, " != sym.typ" + style.err, tmp)
              add "\n"
              add conf.treeRepr(n.typ, tmp, indent = indent + 2)
          else:
            field("typ", trfShowNodeTypes, rc = tmp)
            add "\n"
            add conf.treeRepr(n.typ, tmp, indent = indent + 2)

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
        hfield("ident.id", trfShowNodeIds, $n.ident.id + style.number)
        postLiteral()

      of nkSym:
        add " \""
        add n.sym.name.s + style.identLit
        add "\""
        hfield("sk", trfShowSymKind, $n.sym.kind + style.kind)

        var rconf = rconf
        rconf.excl trfShowSymName
        add symFields(conf, n.sym, rconf, indent, inNode = true)
        addFlags()
        addComment()

      of nkCommentStmt:
        addFlags()
        add "\n"
        if hasComment: add "\n"
        addComment()

      of nkError:
        let
          diag = n.diag
          reportKind = diag.astDiagToLegacyReportKind
          i = diag.instLoc
          file = i.filename

        field("err", trfReportInfo, rconf.formatKind(reportKind) + style.errKind)
        hfield("diagId", trfReportInfo, $diag.diagId.int + style.err)
        field("info", trfReportInfo, formatPath(conf, file) + style.ident)
        add "(", $i.line.int + style.number
        add ",", $i.column.int + style.number
        add ")"

      of nkType:
        postLiteral()

      else:
        discard

    if n.kind notin {nkNone .. nkNilLit, nkCommentStmt}:
      addFlags()
      if (n.kind == nkError and trfSkipAuxError notin rconf) or
          (n.kind != nkError and n.len > 0):
        add "\n"

      if hasComment:
        addComment(false)
        if n.len == 0:
          add "\n"

      let
        sons =
          when n is PNode:
            case n.kind
            of nkError:
              @[n.diag.wrongNode]
            else:
              n.sons
          else:
            n.sons

      for newIdx, subn in sons:
        if trfSkipAuxError in rconf and n.kind == nkError:
          continue

        if trfShowFullSymChoice notin rconf and
           n.kind in {nkClosedSymChoice, nkOpenSymChoice} and
           0 < newIdx:
          break

        if n.kind in {nkOpenSymChoice} and 0 < newIdx:
          assert false, $rconf.flags

        aux(subn, idx & newIdx)

        if idx.len + 1 > rconf.maxDepth:
          break

        if newIdx > rconf.maxLen:
          break

        if newIdx < sons.len - 1:
          add "\n"

  aux(pnode, @[])

proc treeRepr*(
    conf: ConfigRef,
    pnode: ParsedNode,
    rconf: TReprConf = implicitTReprConf,
    indent: int = 0
  ): ColText =
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

  proc aux(n: ParsedNode, idx: seq[int]) =
    if numerate:
      add "["
      add ($nodecount + style.dim) |>> numWidth
      add "] "

    inc nodecount

    var indent = indentIncrease
    genFields(res[], indent, rconf)
    addIndent(indentIncrease)
    indent += 2 + pad
    if idx.len > 0:
      var first = true
      for idxIdx, pos in idx:
        if idx.len - rconf.maxPath <= idxIdx:
          indent += tern(first, 0, 1)

          case pos
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

      if rconf.maxPath > 0:
        add " "
        inc indent

    if isNil(n):
      add "<nil>" + style.nilIt
      return
    elif not (n.kind == pnkEmpty and n.safeLen == 0) and
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
    add "($#, $#)" % [$n.info.line + style.number, $n.info.col + style.number]

    let hasComment = trfShowNodeComments in rconf and n.comment.len > 0

    proc addComment() =
      if hasComment:
        var nl = false
        for line in split(n.comment.strip(leading = false), '\n'):
          if nl: add "\n"
          nl = true

          addi indent, "# " + style.comment
          add line + style.comment

    proc postLiteral() =
      if hasComment: add "\n"
      addComment()

    case n.kind
    of pnkStrLit .. pnkTripleStrLit:
      add " "
      add "\"" & n.lit.literal + style.strLit & "\""
      postLiteral()

    of pnkCharLit .. pnkUInt64Lit:
      add " "
      add $n.lit.iNumber + style.number
      postLiteral()

    of pnkFloatLit .. pnkFloat128Lit:
      add " "
      add $n.lit.fNumber + style.floatLit
      postLiteral()

    of pnkIdent:
      add " \""
      add n.startToken.ident.s + style.identLit
      add "\""
      hfield("ident.id", trfShowNodeIds, $n.startToken.ident.id + style.number)
      postLiteral()

    of pnkCustomLit:
      if hasComment:
        add "\n"
        addComment()

      if idx.len < rconf.maxDepth:
        let (num, ident) = splitCustomLit(n)

        for newIdx, subn in [num, ident].pairs:
          if newIdx + 1 > rconf.maxLen:
            break

          visited.del(cast[int](subn)) # addresses get fixated between calls

          add "\n"
          aux(subn, idx & newIdx)

    of pnkEmpty, pnkNilLit:
      discard

    of pnkError:
      unreachable("IMPLEMENT ME")

    of pnkCommentStmt:
      if hasComment: add "\n"
      addComment()

    of pnkAccQuoted:
      if hasComment:
        add "\n"
        addComment()

      if idx.len < rconf.maxDepth:
        for newIdx, identInfo in n.idents.pairs:
          if newIdx + 1 > rconf.maxLen:
            break

          add "\n"
          addi indent, " \""
          add identInfo.ident.s + style.identLit
          add "\""
          add "($#, $#)" % [$identInfo.line + style.number, $identInfo.col + style.number]
          hfield("ident.id", trfShowNodeIds, $identInfo.ident.id + style.number)

    of pnkParsedKindsWithSons - {pnkCommentStmt}:
      if hasComment:
        add "\n"
        addComment()

      if idx.len < rconf.maxDepth:
        for newIdx, subn in n.sons.pairs:
          if newIdx + 1 > rconf.maxLen:
            break

          add "\n"
          aux(subn, idx & newIdx)

  aux(pnode, @[])

{.pragma:
  dbg,
  deprecated: "DEBUG proc, should not be used in the final build!",
  noSideEffect
  .}

var implicitDebugConfRef: ConfigRef

template ignoreSideEffectTracking(body: untyped): untyped =
  {.cast(noSideEffect).}:
    body

proc setImplicitDebugConfRef*(conf: ConfigRef) {.dbg.} =
  ## Set implicit debug config reference.
  ignoreSideEffectTracking:
    implicitDebugConfRef = conf

proc debugParsedAst*(it: ParsedNode) {.exportc, dbg.} =
  ## Print out tree representation of the parsed AST node
  ignoreSideEffectTracking:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

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
  ignoreSideEffectTracking:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

proc debugType*(it: PType) {.exportc, dbg.} =
  ## Print out tree represntation of the type. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  ignoreSideEffectTracking:
    echo treeRepr(implicitDebugConfRef, it, implicitTReprConf)

proc debugSym*(it: PSym) {.exportc, dbg.} =
  ## Print out tree represntation of the symbol. Can also be used in gdb
  ## debugging session due to `.exportc.` annotation
  ignoreSideEffectTracking:
    var conf = implicitTReprConf
    conf.flags = conf.flags + {trfShowSymAst, trfShowSymFlags, trfShowSymTypes}
    echo treeRepr(implicitDebugConfRef, it, conf)

type
  Debugable = PNode | PSym | PType | PIdent | ParsedNode ## Types that can
    ## be debugged using `treeRepr`

proc debugAux(
    conf: ConfigRef,
    it: Debugable,
    tconf: TReprConf,
    location: InstantiationInfo
  ) {.dbg.} =
  ignoreSideEffectTracking:
    if trfShowDebugLocation in tconf:
      echo "debug at ", location

    echo treeRepr(conf, it, tconf)

template debug*(it: Debugable, tconf: TReprConf) =
  ## Convenience overload of `debugAst`
  debugAux(implicitDebugConfRef, it, tconf, instLoc())

template debug*(conf: ConfigRef, it: Debugable, tconf: TReprConf) =
  ## Print tree representation of the AST using provided configuration.
  debugAux(conf, it, tconf, instLoc())

template debug*(it: PNode) =
  ## Print tree representation of a PNode for compiler debugging
  # TODO: customize the TReprConf here
  debugAux(implicitDebugConfRef, it, implicitTReprConf, instLoc())

template debug*(it: PSym) =
  ## Print tree representation of a PSym for compiler debugging
  # TODO: create a PSym specific implicitTReprConf
  var conf = implicitTReprConf
  conf.flags = conf.flags +
                {
                  trfShowSymAst, trfShowSymId,

                  trfShowTypeFlags
                } -
                {
                  trfShowSymTypes, trfShowFullSymTypes, trfShowSymLineInfo,
                  trfShowSymFlags, trfShowSymMagic,

                  # trfShowNodeTypes,
                  trfShowNodeLineInfo, trfShowNodeFlags}
  debugAux(implicitDebugConfRef, it, conf, instLoc())

template debug*(it: PType) =
  ## Print tree representation of a PType for compiler debugging
  # TODO: customize the TReprConf here
  debugAux(implicitDebugConfRef, it, implicitTReprConf, instLoc())

template debug*(it: PIdent) =
  ## Print tree representation of a PIdent for compiler debugging
  # TODO: customize the TReprConf here
  debugAux(implicitDebugConfRef, it, implicitTReprConf, instLoc())

template debug*(it: ParsedNode) =
  ## Print tree representation of a ParsedNode for compiler debugging
  # TODO: customize the TReprConf here
  debugAux(implicitDebugConfRef, it, implicitTReprConf, instLoc())

template debug*(conf: ConfigRef, it: Debugable) =
  ## Print tree representation of the AST
  debugAux(conf, it, implicitTReprConf, instLoc())

proc inLines*(node: PNode, lrange: Slice[int]): bool {.dbg.} =
  ignoreSideEffectTracking:
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
  ignoreSideEffectTracking:
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
  ignoreSideEffectTracking:
    return file in toFilename(implicitDebugConfRef, node.info) and
           node.info.line.int in lrange

func inDebug*(conf: ConfigRef): bool =
  ## Check whether 'nim compiler debug' is defined right now.
  return conf.isDefined("nimCompilerDebug")

func inDebug*(): bool =
  ## Check whether current implicit compiler configuration is in the
  ## 'debug' range.
  ##
  ## .. warning:: Requires implicit debug configuration to be set
  {.cast(noSideEffect).}:
    assert not isNil(implicitDebugConfRef)
    return implicitDebugConfRef.inDebug()
