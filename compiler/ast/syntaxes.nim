#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Implements the dispatcher for the different parsers.

import
  std/strutils,
  compiler/ast/[
    llstream,
    ast,
    ast_parsed_types,
    idents,
    lexer,
    parser,
    filters,
    filter_tmpl,
    renderer,
    lineinfos,
  ],
  compiler/front/[
    options,
    msgs,
  ],
  compiler/utils/[
    pathutils,
  ]

# TODO: reporting lexer and parser errors tangles it up with filters, break
#       this up into it's own diag/event/telemetry
from compiler/ast/reports_lexer import LexerReport
from compiler/ast/reports_parser import ParserReport

# TODO: replace with internalError/Assert, at the very least, its own
#       diag/event/telemetry is better
from compiler/ast/reports_internal import InternalReport
from compiler/ast/report_enums import ReportKind

# TODO: see about getting rid of all these parser exports
export Parser, parseAll, parseTopLevelStmt, closeParser

type
  FilterKind = enum
    filtNone = "none"
    filtTemplate = "stdtmpl"
    filtReplace = "replace"
    filtStrip = "strip"

proc utf8Bom(s: string): int =
  if s.len >= 3 and s[0] == '\xEF' and s[1] == '\xBB' and s[2] == '\xBF':
    3
  else:
    0

proc containsShebang(s: string, i: int): bool =
  if i+1 < s.len and s[i] == '#' and s[i+1] == '!':
    var j = i + 2
    while j < s.len and s[j] in Whitespace: inc(j)
    result = s[j] == '/'

proc parsePipe(filename: AbsoluteFile, inputStream: PLLStream; cache: IdentCache;
               config: ConfigRef): PNode =
  result = newNode(nkEmpty)
  var s = llStreamOpen(filename, fmRead)
  if s != nil:
    var line = newStringOfCap(80)
    discard llStreamReadLine(s, line)
    var i = utf8Bom(line)
    var linenumber = 1
    if containsShebang(line, i):
      discard llStreamReadLine(s, line)
      i = 0
      inc linenumber
    if i+1 < line.len and line[i] == '#' and line[i+1] == '?':
      inc(i, 2)
      while i < line.len and line[i] in Whitespace: inc(i)
      var p: Parser
      openParser(p, filename, llStreamOpen(substr(line, i)), cache, config)
      result = toPNode(parseAll(p))
      closeParser(p)
    llStreamClose(s)

proc getFilter(ident: PIdent): FilterKind =
  for i in FilterKind:
    if cmpIgnoreStyle(ident.s, $i) == 0:
      return i

proc getCallee(conf: ConfigRef; n: PNode): PIdent =
  if n.kind in nkCallKinds and n[0].kind == nkIdent:
    result = n[0].ident
  elif n.kind == nkIdent:
    result = n.ident
  else:
    conf.localReport(n.info, ParserReport(kind: rparInvalidFilter, node: n))

proc applyFilter(p: var Parser, n: PNode, filename: AbsoluteFile,
                 stdin: PLLStream): PLLStream =
  var f = getFilter(getCallee(p.lex.config, n))
  result = case f
           of filtNone:
             stdin
           of filtTemplate:
             filterTmpl(p.lex.config, stdin, filename, n)
           of filtStrip:
             filterStrip(p.lex.config, stdin, filename, n)
           of filtReplace:
             filterReplace(p.lex.config, stdin, filename, n)
  if f != filtNone:
    assert p.lex.config != nil
    if p.lex.config.hasHint(rlexSourceCodeFilterOutput):
      p.lex.config.localReport LexerReport(
        kind: rlexSourceCodeFilterOutput, msg: result.s)

proc evalPipe(p: var Parser, n: PNode, filename: AbsoluteFile,
              start: PLLStream): PLLStream =
  assert p.lex.config != nil
  result = start
  if n.kind == nkEmpty: return
  if n.kind == nkInfix and n[0].kind == nkIdent and n[0].ident.s == "|":
    for i in 1..2:
      if n[i].kind == nkInfix:
        result = evalPipe(p, n[i], filename, result)
      else:
        result = applyFilter(p, n[i], filename, result)
  elif n.kind == nkStmtList:
    result = evalPipe(p, n[0], filename, result)
  else:
    result = applyFilter(p, n, filename, result)

proc openParser*(p: var Parser, fileIdx: FileIndex, inputstream: PLLStream;
                  cache: IdentCache; config: ConfigRef) =
  assert config != nil
  let filename = toFullPathConsiderDirty(config, fileIdx)
  var pipe = parsePipe(filename, inputstream, cache, config)
  p.lex.config = config
  let s = if pipe != nil: evalPipe(p, pipe, filename, inputstream)
          else: inputstream
  parser.openParser(p, fileIdx, s, cache, config)

proc setupParser*(p: var Parser; fileIdx: FileIndex; cache: IdentCache;
                   config: ConfigRef): bool =
  let filename = toFullPathConsiderDirty(config, fileIdx)
  var f: File
  if not open(f, filename.string):
    config.localReport InternalReport(
      kind: rintCannotOpenFile, file: filename.string)
    return false
  openParser(p, fileIdx, llStreamOpen(f), cache, config)
  result = true

proc parseFile*(fileIdx: FileIndex, cache: IdentCache, config: ConfigRef): ParsedNode =
  var p: Parser
  if setupParser(p, fileIdx, cache, config):
    result = parseAll(p)
    closeParser(p)