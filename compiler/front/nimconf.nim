#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Nim Configuration File Language
## ===============================
##
## The Nim compiler uses a simple configuration file language to customize its behavior. This document describes the syntax and features of this language.
##
## File Format
## -----------
##
## Configuration files typically use the `<module>.nim.cfg` or `nim.cfg`. They are plain text files with a custom syntax for setting compiler options and defining conditional compilation.
##
## Basic Syntax
## ------------
##
## Each line in a configuration file can be one of the following:
##
## 1. An option setting
## 2. A directive
## 3. A comment (starts with #)
##
## Option Settings
## ---------------
##
## Options are set using the following syntax:
##
## .. code-block::
##   option_name[:=] "value"
##   -{0,2}option_name *[:=] *"value"
##
## or
##
## .. code-block::
##
## The `:=` separator is optional. For boolean options, the value can be omitted.
##
## Examples:
##
## .. code-block::
##  threads
##
##
## Directives
## ----------
##
## Directives start with the `@` symbol and provide additional control over the configuration process.
##
## Conditional Compilation
## ^^^^^^^^^^^^^^^^^^^^^^^
##
## - `@if <condition>:`
## - `@elif <condition>:`
## - `@else:`
## - `@end`
##
## These directives allow for conditional compilation based on defined symbols or expressions.
##
## Output
## ^^^^^^
##
## - `@write "message"`
##
## Writes a message during configuration processing.
##
## Environment Variables
## ^^^^^^^^^^^^^^^^^^^^^
##
## - `@putenv "key" "value"`
## - `@prependenv "key" "value"`
## - `@appendenv "key" "value"`
##
## These directives manipulate environment variables during configuration.
##
## File Inclusion
## ^^^^^^^^^^^^^^
##
## - `@include "filename"`
##
## Includes another configuration file.
##
## Expressions
## -----------
##
## Expressions can be used in conditional compilation directives. They support:
##
## - Boolean operators: `and`, `or`, `not`
## - Parentheses for grouping
## - Defined symbol checking
##
## Example:
##
## @if defined(windows) and not defined(mingw):
##   # Windows-specific non-MinGW configuration
## @end
##
##
## Variable Expansion
## ------------------
##
## The `%=` operator can be used for variable expansion. It expands variables using the current configuration and environment variables. #TODO: which takes precedence?
##
## Example:
##
## nimblePath %= "$home/.nimble/pkgs/"
##
## This will expand `$home` to the user's home directory.
##
## String Concatenation
## --------------------
##
## Strings can be concatenated using the `&` operator.
##
## Comments
## --------
##
## Comments start with the `#` character and continue to the end of the line.
##
## Example:
##
## This is a comment
##
## threads = on # Enable threading
##

# TODO: document that any nim cli option can be used in a config file; link to docs/advopts.rst
# TODO: document config variables; they must include at least one '.' in the name and be at least 10 characters long; can be referenced in config files using `$name`, or `${name}`, syntax

## This module handles the reading of config file(s).
##
## ..note:: Even though this module is very effectful in its processing of a
##          config file and updating a `ConfigRef`, it must not assume that
##          it's handling a 'canonical' `ConfigRef` for the current program and
##          is not at liberty to output error messages and the like. That must
##          be handled by the caller.

import
  std/[
    os,
    strutils,
    strtabs,
  ],
  compiler/front/[
    options,
    optionsprocessor,
  ],
  compiler/ast/[
    lexer,
    idents,
    wordrecg,
    llstream,
    lineinfos
  ],
  compiler/utils/[
    pathutils,
  ]
from compiler/front/msgs import toFullPath

type
  ConfigFileEventKind* = enum
    ## events/errors arising from parsing and processing a compiler config file
    # xxx: this is modelled too closely after legacy reports

    # fatal errors begin
    cekInternalError
    # fatal errors end

    # parsing errors begin
    # lexer generated
    cekLexerErrorDiag        ## lexer error being forwarded

    # expected token
    cekParseExpectedX        ## expected some token
    cekParseExpectedCloseX   ## expected closing ')', ']', etc
    cekParseExpectedIdent    ## expected an identifier
    # parsing errors end

    # users errors begin
    # invalid input
    cekInvalidDirective

    # flag parsing/processing error
    cekFlagError             ## error from flag processing
    # user errors end

    # warning begin
    cekLexerWarningDiag      ## warning from the lexer
    # warning end

    # user output start
    cekWriteConfig           ## write out the config
    # user output end

    # progress begin
    cekProgressConfStart = "Conf"
    cekProgressPathAdded = "Path"
    # progress end

  ConfigFileEvent* = object
    case kind*: ConfigFileEventKind:
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
         cekInvalidDirective, cekWriteConfig, cekProgressPathAdded:
        location*: TLineInfo         ## diagnostic location
      of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag:
        lexerDiag*: LexerDiag
      of cekFlagError:
        flagResult*: ProcSwitchResult
        flagInfo*: TLineInfo
      of cekProgressConfStart:
        discard
    instLoc*: InstantiationInfo ## instantiation in this module's source
    msg*: string

  NimConfEvtWriter* = proc(config: ConfigRef,
                           evt: ConfigFileEvent,
                           writeFrom: InstantiationInfo): void

  NimConfParser = object
    lexer: Lexer
    condStack: seq[bool]
    config: ConfigRef
    cfgEvtWriter: NimConfEvtWriter ## do not call, use `callEvtWriter`
    stopOnError: bool              ## whether to continue if an error occurs
    stopProcessing: bool           ## set if `stopOnError` and an error
                                   ## occurred, or if there is a fatal error
    identCache: IdentCache

  CancelConfigProcessing = object of CatchableError
    ## internal error used to halt processing

# ---------------- configuration file parser -----------------------------
# we use Nim's lexer here to save space and work

proc callEvtWriter(N: var NimConfParser, e: ConfigFileEvent,
                   loc: InstantiationInfo) =
  let stopProcessing =
    case e.kind
    of cekInternalError:
      true
    of cekLexerErrorDiag..cekFlagError:
      N.stopOnError
    of cekLexerWarningDiag..cekProgressPathAdded:
      false

  N.cfgEvtWriter(N.config, e, loc)

  if stopProcessing:
    N.stopProcessing = stopProcessing
    raise (ref CancelConfigProcessing)()

proc handleError(N: var NimConfParser,
                 ev: range[cekParseExpectedX..cekInvalidDirective],
                 errMsg: string,
                 instLoc = instLoc(-1)) =
  let e = ConfigFileEvent(kind: ev,
                          location: N.lexer.getLineInfo,
                          instLoc: instLoc,
                          msg: errMsg)
  N.callEvtWriter(e, instLoc)

proc handleExpectedX(N: var NimConfParser, missing: string,
                    instLoc = instLoc(-1)) =
  let e = ConfigFileEvent(kind: cekParseExpectedX,
                          location: N.lexer.getLineInfo,
                          instLoc: instLoc,
                          msg: missing)
  N.callEvtWriter(e, instLoc)

proc handleWriteConf(N: var NimConfParser, cfg: string,
                     instLoc = instLoc(-1)) =
  let e = ConfigFileEvent(kind: cekWriteConfig,
                          instLoc: instLoc,
                          msg: cfg)
  N.callEvtWriter(e, instLoc)

proc handleRead(N: var NimConfParser,
                filename: string,
                instLoc = instLoc(-1)) =
  let e = ConfigFileEvent(kind: cekProgressConfStart,
                          instLoc: instLoc,
                          msg: filename)
  N.callEvtWriter(e, instLoc)

proc ppGetTok(N: var NimConfParser, tok: var Token) =
  var firstLine = true
    ## used to force at least one attempt

  # simple filter
  while firstLine or tok.tokType in {tkComment}:
    firstLine = false # first attempt started, now only comment skipping

    rawGetTok(N.lexer, tok)

    if tok.tokType == tkError:
      let e = ConfigFileEvent(kind: cekInternalError,
                              lexerDiag: tok.error,
                              instLoc: tok.error.instLoc)
      N.callEvtWriter(e, instLoc(-1))

    for d in N.lexer.errorsHintsAndWarnings():
      let e =
        case d.kind
        of LexDiagsError:
          ConfigFileEvent(kind: cekLexerErrorDiag, lexerDiag: d,
                          instLoc: d.instLoc)
        of LexDiagsWarning:
          ConfigFileEvent(kind: cekLexerWarningDiag, lexerDiag: d,
                          instLoc: d.instLoc)
        of LexDiagsHint:
          continue # we don't generate these
      N.callEvtWriter(e, instLoc(-1))

proc parseExpr(N: var NimConfParser, tok: var Token): bool
proc parseAtom(N: var NimConfParser, tok: var Token): bool =
  if tok.tokType == tkParLe:
    ppGetTok(N, tok)
    result = parseExpr(N, tok)
    if tok.tokType == tkParRi:
      ppGetTok(N, tok)
    else:
      handleError(N, cekParseExpectedCloseX, ")")
  elif tok.tokType == tkNot:
    ppGetTok(N, tok)
    result = not parseAtom(N, tok)
  else:
    result = isDefined(N.config, tok.ident.s)
    ppGetTok(N, tok)

proc parseAndExpr(N: var NimConfParser, tok: var Token): bool =
  result = parseAtom(N, tok)
  while tok.tokType == tkAnd:
    ppGetTok(N, tok)          # skip "and"
    var b = parseAtom(N, tok)
    result = result and b

proc parseExpr(N: var NimConfParser, tok: var Token): bool =
  result = parseAndExpr(N, tok)
  while tok.tokType == tkOr:
    ppGetTok(N, tok)          # skip "or"
    var b = parseAndExpr(N, tok)
    result = result or b

proc evalppIf(N: var NimConfParser, tok: var Token): bool =
  ppGetTok(N, tok)            # skip 'if' or 'elif'
  result = parseExpr(N, tok)
  if tok.tokType == tkColon:
    ppGetTok(N, tok)
  else:
    handleExpectedX(N, ":")

proc doEnd(N: var NimConfParser, tok: var Token) =
  if high(N.condStack) < 0:
    handleExpectedX(N, "@if")
  ppGetTok(N, tok)            # skip 'end'
  setLen(N.condStack, high(N.condStack))

type
  TJumpDest = enum
    jdEndif, jdElseEndif

proc jumpToDirective(N: var NimConfParser, tok: var Token, dest: TJumpDest)
proc doElse(N: var NimConfParser, tok: var Token) =
  if high(N.condStack) < 0:
    handleExpectedX(N, "@if")

  ppGetTok(N, tok)

  if tok.tokType == tkColon:
    ppGetTok(N, tok)

  if N.condStack[high(N.condStack)]:
    jumpToDirective(N, tok, jdEndif)

proc doElif(N: var NimConfParser, tok: var Token) =
  if high(N.condStack) < 0:
    handleExpectedX(N, "@if")

  var res = evalppIf(N, tok)
  if N.condStack[high(N.condStack)] or not res:
    jumpToDirective(N, tok, jdElseEndif)
  else:
    N.condStack[high(N.condStack)] = true

proc jumpToDirective(N: var NimConfParser, tok: var Token, dest: TJumpDest) =
  var nestedIfs = 0
  while true:
    if tok.ident != nil and tok.ident.s == "@":
      ppGetTok(N, tok)
      case whichKeyword(tok.ident)
      of wIf:
        inc(nestedIfs)
      of wElse:
        if dest == jdElseEndif and nestedIfs == 0:
          doElse(N, tok)
          break
      of wElif:
        if dest == jdElseEndif and nestedIfs == 0:
          doElif(N, tok)
          break
      of wEnd:
        if nestedIfs == 0:
          doEnd(N, tok)
          break
        if nestedIfs > 0: dec(nestedIfs)
      else:
        discard
      ppGetTok(N, tok)
    elif tok.tokType == tkEof:
      handleExpectedX(N, "@end")
    else:
      ppGetTok(N, tok)

proc readConfigFile(N: var NimConfParser, filename: AbsoluteFile): bool

proc parseDirective(N: var NimConfParser, tok: var Token) =
  ppGetTok(N, tok)            # skip @
  case whichKeyword(tok.ident)
  of wIf:
    setLen(N.condStack, N.condStack.len + 1)
    let res = evalppIf(N, tok)
    N.condStack[high(N.condStack)] = res
    if not res: jumpToDirective(N, tok, jdElseEndif)
  of wElif: doElif(N, tok)
  of wElse: doElse(N, tok)
  of wEnd: doEnd(N, tok)
  of wWrite:
    ppGetTok(N, tok)
    N.handleWriteConf:
      strtabs.`%`($tok, N.config.configVars, {useEnvironment, useKey})
    ppGetTok(N, tok)
  else:
    case tok.ident.s.normalize
    of "putenv":
      ppGetTok(N, tok)
      var key = $tok
      ppGetTok(N, tok)
      os.putEnv(key, $tok)
      ppGetTok(N, tok)
    of "prependenv":
      ppGetTok(N, tok)
      var key = $tok
      ppGetTok(N, tok)
      os.putEnv(key, $tok & os.getEnv(key))
      ppGetTok(N, tok)
    of "appendenv":
      ppGetTok(N, tok)
      var key = $tok
      ppGetTok(N, tok)
      os.putEnv(key, os.getEnv(key) & $tok)
    of "include":
      ppGetTok(N, tok)
      let configFile = toAbsolute(expandTilde($tok), N.config.toFullPath(N.lexer.fileIdx).parentDir.AbsoluteDir)
      let currentLexer = N.lexer # `readConfigFile` clobbers it, so save the lexer so we can restore it
      block:
        if readConfigFile(N, configFile):
          N.config.configFiles.add(configFile)
        N.lexer = currentLexer # restore the lexer
      ppGetTok(N, tok)
    else:
      handleError(N, cekInvalidDirective, $tok)

proc confTok(N: var NimConfParser, tok: var Token) =
  ppGetTok(N, tok)
  while tok.ident != nil and tok.ident.s == "@":
    parseDirective(N, tok)    # else: give the token to the parser

proc checkSymbol(N: var NimConfParser, tok: Token) =
  if tok.tokType notin {tkSymbol..tkInt64Lit, tkStrLit..tkTripleStrLit}:
    handleError(N, cekParseExpectedIdent, $tok)

proc parseAssignment(N: var NimConfParser, tok: var Token) =
  if tok.ident != nil:
    if tok.ident.s == "-" or tok.ident.s == "--":
      confTok(N, tok)           # skip unnecessary prefix
  var info = getLineInfo(N.lexer, tok) # save for later in case of an error
  checkSymbol(N, tok)
  var s = $tok
  confTok(N, tok)             # skip symbol
  var val = ""
  while tok.tokType == tkDot:
    s.add('.')
    confTok(N, tok)
    checkSymbol(N, tok)
    s.add($tok)
    confTok(N, tok)
  if tok.tokType == tkBracketLe:
    # BUGFIX: val, not s!
    confTok(N, tok)
    checkSymbol(N, tok)
    val.add('[')
    val.add($tok)
    confTok(N, tok)
    if tok.tokType == tkBracketRi:
      confTok(N, tok)
    else:
      handleError(N, cekParseExpectedCloseX, "]")
    val.add(']')
  let percent = tok.ident != nil and tok.ident.s == "%="
  if tok.tokType in {tkColon, tkEquals} or percent:
    if val.len > 0: val.add(':')
    confTok(N, tok)           # skip ':' or '=' or '%'
    checkSymbol(N, tok)
    val.add($tok)
    confTok(N, tok)           # skip symbol
    if tok.tokType in {tkColon, tkEquals}:
      val.add($tok) # add the :
      confTok(N, tok)           # skip symbol
      checkSymbol(N, tok)
      val.add($tok) # add the token after it
      confTok(N, tok)           # skip symbol
    while tok.ident != nil and tok.ident.s == "&":
      confTok(N, tok)
      checkSymbol(N, tok)
      val.add($tok)
      confTok(N, tok)
  let
    v =
      if percent:
        strtabs.`%`(val, N.config.configVars, {useEnvironment, useEmpty})
      else:
        val
    r = processSwitch(s, v, passPP, N.config)
  case r.kind
  of procSwitchSuccess: discard # ignore
  of procSwitchResultErrorKinds:
    let evt = ConfigFileEvent(kind: cekFlagError, flagResult: r,
                              flagInfo: info)
    N.callEvtWriter(evt, instLoc())

  case r.switch
  of cmdSwitchNimblepath:
    if r.processedNimblePath.didProcess:
      for res in r.processedNimblePath.nimblePathResult.addedPaths:
        N.callEvtWriter(ConfigFileEvent(kind: cekProgressPathAdded,
                                        msg: res.string,
                                        location: info,
                                        instLoc: instLoc()),
                        instLoc())
  else:
    discard

proc readConfigFile(N: var NimConfParser, filename: AbsoluteFile): bool =
  ## assumes `cfgEvtWriter` has already been set, do not export
  if filename in N.config.configFiles:
    # Avoid circular inclusion
    return false

  var
    tok: Token
    stream: PLLStream

  stream = llStreamOpen(filename, fmRead)
  if stream != nil:
    initToken(tok)
    openLexer(N.lexer, filename, stream, N.identCache, N.config)

    # save the existing source of command parameters and use the config file
    let oldCmdLineSrcIdx = N.config.commandLineSrcIdx
    N.config.commandLineSrcIdx = N.lexer.fileIdx

    try:
      tok.tokType = tkEof       # to avoid a pointless warning
      confTok(N, tok)           # read in the first token

      while tok.tokType != tkEof:
        parseAssignment(N, tok)

      if N.condStack.len > 0:
        handleError(N, cekParseExpectedX, "@end")

      result = true
    except CancelConfigProcessing:
      discard
    finally:
      # restore to the previous source of command parameters
      N.config.commandLineSrcIdx = oldCmdLineSrcIdx

      closeLexer(N.lexer)

proc getUserConfigPath*(filename: RelativeFile): AbsoluteFile =
  result = getConfigDir().AbsoluteDir / RelativeDir"nim" / filename

proc getSystemConfigPath*(conf: ConfigRef; filename: RelativeFile): AbsoluteFile =
  # try standard configuration file (installation did not distribute files
  # the UNIX way)
  let p = getPrefixDir(conf)
  result = p / RelativeDir"config" / filename
  when defined(unix):
    if not fileExists(result): result = p / RelativeDir"etc/nim" / filename
    if not fileExists(result): result = AbsoluteDir"/etc/nim" / filename

iterator configFiles(N: NimConfParser, cfg: RelativeFile): AbsoluteFile =
  # xxx: maybe this should open lexers instead
  if optSkipSystemConfigFile notin N.config.globalOptions:
    yield getSystemConfigPath(N.config, cfg)

  if optSkipUserConfigFile notin N.config.globalOptions:
    yield getUserConfigPath(cfg)

  let pd = if N.config.projectPath.isEmpty: AbsoluteDir(getCurrentDir())
           else:                            N.config.projectPath

  if optSkipParentConfigFiles notin N.config.globalOptions:
    for dir in parentDirs(pd.string, fromRoot=true, inclusive=false):
      yield AbsoluteDir(dir) / cfg

  if optSkipProjConfigFile notin N.config.globalOptions:
    yield pd / cfg

    if N.config.projectName.len != 0:
      # project wide config file:
      yield changeFileExt(N.config.projectFull, "nim.cfg")

proc loadConfigs(N: var NimConfParser, cfg: RelativeFile): bool =
  setDefaultLibpath(N.config)

  for cfgFile in configFiles(N, cfg):
    if readConfigFile(N, cfgFile):
      N.config.configFiles.add(cfgFile)

  for filename in N.config.configFiles:
    # delayed to here so that `hintConf` is honored
    N.handleRead(filename.string)

  result = not N.stopProcessing # an unset stopProcessing means no errors

proc loadConfigs*(
    cfg: RelativeFile; cache: IdentCache;
    conf: ConfigRef, evtHandler: NimConfEvtWriter,
    stopOnError: bool = true
  ): bool {.inline.} =
  var parser = NimConfParser(config: conf, cfgEvtWriter: evtHandler,
                             stopOnError: stopOnError, identCache: cache)
  parser.loadConfigs(cfg)

proc readConfigFile*(
  filename: AbsoluteFile, cache: IdentCache,
  conf: ConfigRef, evtHandler: NimConfEvtWriter
): bool {.inline.} =
  # created and exported for `nimph`
  var parser = NimConfParser(config: conf, cfgEvtWriter: evtHandler,
                             stopOnError: true, identCache: cache)
  parser.readConfigFile(filename)
