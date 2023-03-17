## Implementation of the structured CLI message generator. Using
## `--msgFormat=sexp` will make compiler switch to the report hook
## implemented in this module.

import
  experimental/[
    sexp,
    colortext,
    sexp_diff
  ],
  compiler/ast/[
    lineinfos,
    ast,
    reports
  ],
  compiler/front/[
    options,
    msgs
  ],
  std/[
    strutils
  ]

import std/options as std_options

var writeConf: ConfigRef


proc addFields[T](s: var SexpNode, r: T, ignore: seq[string] = @[])

proc sexpItems*[T](s: T): SexpNode =
  result = newSList()
  for item in items(s):
    result.add sexp(item)

proc sexp*[T: object | tuple](obj: T): SexpNode =
  result = newSList()
  addFields(result, obj)

proc sexp*[T: object | tuple](obj: ref T): SexpNode = sexp(obj[])
proc sexp*[E: enum](e: E): SexpNode = newSSymbol($e)
proc sexp*[T](s: seq[T]): SexpNode = sexpItems(s)
proc sexp*[R, T](s: array[R, T]): SexpNode = sexpItems(s)
proc sexp*[I](s: set[I]): SexpNode = sexpItems(s)
proc sexp*(s: cstring): SexpNode = sexp($s)
proc sexp*(n: NodeId): SexpNode = sexp(n.int)

proc sexp*(v: SomeInteger): SexpNode = newSInt(BiggestInt(v))
proc sexp*(id: FileIndex): SexpNode =
  sexp(writeConf.toMsgFilename(id))


iterator sexpFields[T: object | tuple](obj: T, ignore: seq[string] = @[]): SexpNode =
  for name, value in fieldPairs(obj):
    var pass = true
    when value is ref or value is ptr:
      if isNil(value):
        pass = false

    when value is seq or value is string:
      if len(value) == 0:
        pass = false

    when value is TLineInfo:
      if pass and value == unknownLineInfo:
        pass = false

    when value is ReportLineInfo:
      if pass and not value.isValid():
        pass = false

    if pass and name in ignore:
      pass = false

    if pass:
      yield newSKeyword(name, sexp(value))


proc add*(self: var SexpNode, str: string, expr: SexpNode) =
  self.add newSSymbol(":" & str)
  self.add expr

proc sexp*[T](o: Option[T]): SexpNode =
  if o.isNone: newSNil() else: sexp(o.get())

proc addFields[T](s: var SexpNode, r: T, ignore: seq[string] = @[]) =
  for item in sexpFields(r, ignore):
    s.add item

proc sexp*(i: ReportLineInfo): SexpNode =
  convertSexp([
    writeConf.formatPath(i.file).sexp(),
    sexp(i.line),
    sexp(i.col)
  ])

proc sexp*(i: TLineInfo): SexpNode =
  convertSexp([sexp(i.fileIndex), sexp(i.line), sexp(i.col)])

proc sexp*(e: StackTraceEntry): SexpNode =
  result = newSList()
  result.addFields(e, @["filename"])
  result.add newSKeyword(
    "filename", writeConf.formatPath($e.filename).sexp())


proc sexp*(typ: PType): SexpNode =
  if typ.isNil: return newSNil()
  result = newSList()
  result.add newSSymbol(($typ.kind)[2 ..^ 1])
  if typ.sons.len > 0:
    result.add("sons", sexp(typ.sons))

proc sexp*(node: PNode): SexpNode =
  if node.isNil: return newSNil()

  result = newSList()
  result.add newSSymbol(($node.kind)[2 ..^ 1])
  case node.kind:
    of nkNone, nkEmpty:           discard
    of nkCharLit..nkUInt64Lit:    result.add sexp(node.intVal)
    of nkFloatLit..nkFloat128Lit: result.add sexp(node.floatVal)
    of nkStrLit..nkTripleStrLit:  result.add sexp(node.strVal)
    of nkSym:                     result.add newSSymbol(node.sym.name.s)
    of nkIdent:                   result.add newSSymbol(node.ident.s)
    of nkError:                   result.add sexp(node.diag.wrongNode)
    of nkWithSons:
      for node in node.sons:
        result.add sexp(node)

proc sexp*(t: PSym): SexpNode =
  convertSexp([
    substr($t.kind, 2).newSSymbol(),
    name = sexp(t.name.s),
    info = sexp(t.info)
  ])


proc reportHook*(conf: ConfigRef, r: Report): TErrorHandling =
  writeConf = conf
  let wkind = conf.writabilityKind(r)

  if wkind == writeDisabled:
    return
  else:
    var s = newSList()
    s.add newSSymbol(multiReplace($r.kind, {
      "rsem": "Sem",
      "rpar": "Par",
      "rlex": "Lex",
      "rvm": "VM",
      "rint": "Int",
      "rext": "Ext",
      "rdbg": "Dbg",
      "rback": "Bck",
    }))
    s.add newSSymbol(":severity")
    s.add sexp(conf.severity(r))

    let f = @["kind"]

    case r.category:
      of repLexer:    s.addFields(r.lexReport, f)
      of repParser:   s.addFields(r.parserReport, f)
      of repCmd:      s.addFields(r.cmdReport, f)
      of repVM:       s.addFields(r.vmReport, f)
      of repSem:
        if r.kind == rsemProcessingStmt:
          s.addFields(r.semReport, f & "node")
        else:
          s.addFields(r.semReport, f)
      of repDbgTrace: s.addFields(r.dbgTraceReport, f)
      of repDebug:    s.addFields(r.debugReport, f)
      of repInternal: s.addFields(r.internalReport, f)
      of repBackend:  s.addFields(r.backendReport, f)
      of repExternal: s.addFields(r.externalReport, f)

    if wkind == writeForceEnabled:
      echo s.toLine().toString(conf.useColor)
    else:
      conf.writeln(s.toLine().toString(conf.useColor))
