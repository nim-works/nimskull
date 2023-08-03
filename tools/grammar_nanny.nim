## Simple tool to check for obvious mistakes in Nim's
## grammar.txt file.

import std / [strutils, sets]

import
  compiler/ast/[llstream, lexer, idents, lineinfos, reports],
  compiler/utils/[pathutils],
  compiler/front/[options, msgs, cli_reporter]

# import ".." / compiler / [
#   llstream, lexer, options, msgs, idents,
#   lineinfos, pathutils, reports]

proc checkGrammarFileImpl(cache: IdentCache, config: ConfigRef) =
  var f = AbsoluteFile"doc/grammar.txt"
  let data = readFile(f.string).multiReplace({"IND{=}": "SAME_IND", "'": "\""})
  var stream = llStreamOpen(data)
  var declaredSyms = initHashSet[string]()
  var usedSyms = initHashSet[string]()
  usedSyms.incl "module" # 'module' is the start rule.
  if stream != nil:
    declaredSyms.incl "section" # special case for 'section(RULE)' in the grammar
    var
      L: Lexer
      tok: Token
    initToken(tok)
    openLexer(L, f, stream, cache, config)
    # load the first token:
    rawGetTok(L, tok)
    var word = ""
    while tok.tokType != tkEof:
      #printTok(config, tok)
      if isKeyword(tok.tokType) or tok.tokType == tkSymbol:
        word = tok.ident.s
        rawGetTok(L, tok)
        if tok.tokType == tkEquals:
          declaredSyms.incl word
          rawGetTok(L, tok)
        elif not allCharsInSet(word, {'A'..'Z', '0'..'9', '_'}):
          usedSyms.incl word
      else:
        rawGetTok(L, tok)
    for u in declaredSyms:
      if u notin usedSyms:
        echo "Unused non-terminal: ", u

    for u in usedSyms:
      if u notin declaredSyms:
        echo "Undeclared non-terminal: ", u

    closeLexer(L)
  else:
    config.localReport InternalReport(kind: rintCannotOpenFile, file: f.string)

proc checkGrammarFile*() =
  let conf = newConfigRef(reportHook)
  conf.astDiagToLegacyReport = cli_reporter.legacyReportBridge
  checkGrammarFileImpl(newIdentCache(), conf)
