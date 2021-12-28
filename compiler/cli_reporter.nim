import reports
import std/[strformat, strutils, options, parseutils]

import nimsuggest/sexp


## Implementation of the default command-line error hook. All the
## pretty-printed messages are constructed in this module.

when false:
  import hmisc/hasts/json_serde
  using writer: var JsonSerializer

  jsonSerdeFor(FileIndex, loadJsonDistinct, writeJsonDistinct)

  proc writeJson(writer; node: PNode) = writer.writeJson("[TODO write PNode]")
  proc writeJson(writer; node: PSym) = writer.writeJson("[TODO write PSym]")
  proc writeJson(writer; node: PType) = writer.writeJson("[TODO write PType]")

proc report(r: SemReport)      = echo r
proc report(r: ParserReport)   = echo r
proc report(r: LexerReport)    = echo r
proc report(r: InternalReport) = echo r
proc report(r: ExternalReport) = echo r
proc report(r: DebugReport)    = echo r
proc report(r: BackendReport)  = echo r
proc report(r: CmdReport)      = echo r


proc addFields[T](s: var SexpNode, r: T, ignore: seq[string] = @[])

proc sexp[T: object | tuple](obj: T): SexpNode =
  result = newSList()
  addFields(result, obj)

proc sexp[T: object | tuple](obj: ref T): SexpNode =
  result = newSList()
  addFields(result, obj[])

proc sexp*[E: enum](e: E): SexpNode = newSSymbol($e)

proc sexpItems*[T](s: T): SexpNode =
  result = newSList()
  for item in items(s):
    result.add sexp(item)


proc sexp*[T](s: seq[T]): SexpNode = sexpItems(s)
proc sexp*[R, T](s: array[R, T]): SexpNode = sexpItems(s)
proc sexp*[I](s: set[I]): SexpNode = sexpItems(s)
proc sexp*(s: cstring): SexpNode = sexp($s)

proc sexp*(v: SomeInteger): SexpNode = newSInt(BiggestInt(v))
proc sexp*(id: FileIndex): SexpNode = newSInt(int(id))

iterator sexpFields[T](obj: T, ignore: seq[string] = @[]): tuple[key, val: SexpNode] =
  for name, value in fieldPairs(obj):
    if name notin ignore:
      yield (newSSymbol(":" & name), sexp(value))

proc sexp*[T](o: Option[T]): SexpNode =
  if o.isNone: newSNil() else: sexp(o.get())

proc addFields[T](s: var SexpNode, r: T, ignore: seq[string] = @[]) =
  for key, val in sexpFields(r, ignore):
    s.add key
    s.add val

proc sexp*(node: PNode): SexpNode =
  if node.isNil: return newSNil()

  result = newSList()
  result.add newSSymbol(($node.kind)[2 ..^ 1])
  case node.kind:
    of nkCharLit..nkUInt64Lit:    result.add sexp(node.strVal)
    of nkFloatLit..nkFloat128Lit: result.add sexp(node.floatVal)
    of nkStrLit..nkTripleStrLit:  result.add sexp(node.intVal)
    of nkSym:                     result.add newSSymbol(node.sym.name.s)
    of nkIdent:                   result.add newSSymbol(node.ident.s)
    else:
      for node in node.sons:
        result.add sexp(node)

proc sexp*(t: PType): SexpNode = newSSymbol("<type>")
proc sexp*(t: PSym): SexpNode = newSSymbol("<sym>")

proc reportHook*(r: Report) =
  let k = $r.kind
  var s = newSList()
  s.add newSSymbol($r.category & "-" & k)
  case r.category:
    of repLexer:    s.addFields(r.lexReport)
    of repParser:   s.addFields(r.parserReport)
    of repCmd:      s.addFields(r.cmdReport)
    of repSem:
      if r.kind == rsemProcessingStmt:
        s.addFields(r.semReport, @["expression"])

      else:
        s.addFields(r.semReport)

    of repDebug:    s.addFields(r.debugReport)
    of repInternal: s.addFields(r.internalReport)
    of repBackend:  s.addFields(r.backendReport)
    of repExternal: s.addFields(r.externalReport)

  echo s
