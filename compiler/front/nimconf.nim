#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module handles the reading of the config file.
## 
## Future Direction:
## Rewrite this module to work in a more continuation/iterator style where,
## upon an error return data and control immediately to the caller which can
## then decide to act upon it, and if desired resume execution. This will
## likely require setting up a context upon calling loadConfig. Then running
## until an error is encountered or completion. If an error is encountered, the
## routine where the error was encountered must leave the context in a
## resumable state, which means no closing the lexer, etc and returning to the
## caller a description of what happened. The caller can then do something, and
## if it chooses to, resume execution by calling something like
## `continueLoadingConfig`.

import
  std/[
    os,
    strutils,
    strtabs,
  ],
  compiler/front/[
    options,
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

type
  ConfigFileEventKind* = enum
    ## events/errors arising from parsing and processing a compiler config file
    # TODO: this is modelled too closely after legacy reports

    # fatal errors begin
    cekInternalError
    # fatal errors end

    # unclassified event begin
    cekFlagAssignment
    # unclassified event end

    # users errors begin

    # lexer generated
    cekLexerErrorDiag        ## lexer error being forwarded

    # expected token
    cekParseExpectedX        ## expected some token
    cekParseExpectedCloseX   ## expected closing ')', ']', etc
    cekParseExpectedIdent    ## expected an identifier

    # invalid input
    cekInvalidDirective
    # user errors end

    # warning begin
    cekLexerWarningDiag      ## warning from the lexer
    # warning end

    # hint begin
    cekLexerHintDiag         ## hint from the lexer
    # hint end

    # user output start
    cekWriteConfig           ## write out the config
    # user output end

    # debug start
    cekDebugTrace            ## config debug trace
    cekDebugReadStart        ## start config file read
    cekDebugReadStop         ## stop config file read
    # debug end

    # progress begin
    cekProgressConfStart
    # progress end

  ConfigFileEvent* = object
    case kind*: ConfigFileEventKind:
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
         cekInvalidDirective, cekWriteConfig, cekDebugTrace:
        location*: TLineInfo         ## diagnostic location
      of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag,
         cekLexerHintDiag:
        lexerDiag*: LexerDiag
      of cekFlagAssignment:
        flagResult*: ProcSwitchResult
        flagInfo*: TLineInfo
      of cekDebugReadStart, cekDebugReadStop, cekProgressConfStart:
        discard
    instLoc*: InstantiationInfo ## instantiation in lexer's source
    msg*: string

  CfgScriptEvt* = object
    scriptEvt*: ScriptEvt
    scriptPath*: AbsoluteFile

  NimConfEvtHandler* = proc(config: ConfigRef,
                            evt: ConfigFileEvent,
                            reportFrom: InstantiationInfo): void

  ConfScriptEvtReceiver* = proc(evt: CfgScriptEvt)

  NimConfParser = object
    ## Used for both parsing a config file (*.cfg) and processing a script file
    ## (*.nims)
    # xxx: rename to match dual purpose
    lexer: Lexer
    condStack: seq[bool]
    config: ConfigRef
    cfgEvtHandler: NimConfEvtHandler
    scriptEvtReceiver*: ConfScriptEvtReceiver

# ---------------- configuration file parser -----------------------------
# we use Nim's lexer here to save space and work

proc handleError(N: NimConfParser,
                 ev: range[cekParseExpectedX..cekInvalidDirective],
                 errMsg: string, 
                 instLoc = instLoc(-1)) =
    let e = ConfigFileEvent(kind: ev,
                            location: N.lexer.getLineInfo,
                            instLoc: instLoc,
                            msg: errMsg)
    N.cfgEvtHandler(N.lexer.config, e, instLoc)

proc handleExpectedX(N: NimConfParser, missing: string, instLoc = instLoc(-1)) =
    let e = ConfigFileEvent(kind: cekParseExpectedX, 
                            location: N.lexer.getLineInfo, 
                            instLoc: instLoc, 
                            msg: missing)
    N.cfgEvtHandler(N.lexer.config, e, instLoc)

proc handleWriteConf(N: NimConfParser, cfg: string, instLoc = instLoc(-1)) =
    let e = ConfigFileEvent(kind: cekWriteConfig,
                            instLoc: instLoc,
                            msg: cfg)
    N.cfgEvtHandler(N.lexer.config, e, instLoc)

proc handleTrace(N: NimConfParser, trace: string, instLoc = instLoc(-1)) =
    let e = ConfigFileEvent(kind: cekDebugTrace,
                            location: N.lexer.getLineInfo,
                            instLoc: instLoc,
                            msg: trace)
    N.cfgEvtHandler(N.lexer.config, e, instLoc)

proc handleRead(N: NimConfParser,
                evt: range[cekDebugReadStart..cekProgressConfStart],
                filename: string,
                instLoc = instLoc(-1)) =
    let e = ConfigFileEvent(kind: evt,
                            instLoc: instLoc,
                            msg: filename)
    N.cfgEvtHandler(N.config, e, instLoc)

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
      N.cfgEvtHandler(N.lexer.config, e, instLoc(-1))

    for d in N.lexer.errorsHintsAndWarnings():
      {.cast(uncheckedAssign).}:
        let e = ConfigFileEvent(kind: (case d.kind
                                      of LexDiagsError:   cekLexerErrorDiag
                                      of LexDiagsWarning: cekLexerWarningDiag
                                      of LexDiagsHint:    cekLexerHintDiag
                                      ),
                                lexerDiag: d,
                                instLoc: d.instLoc)
      N.cfgEvtHandler(N.lexer.config, e, instLoc(-1))

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
      ppGetTok(N, tok)

    of "trace":
      ppGetTok(N, tok)
      N.handleTrace($tok)
      ppGetTok(N, tok)

    else:
      handleError(N, cekInvalidDirective, $tok)

proc confTok(N: var NimConfParser, tok: var Token) =
  ppGetTok(N, tok)
  while tok.ident != nil and tok.ident.s == "@":
    parseDirective(N, tok)    # else: give the token to the parser

proc checkSymbol(N: NimConfParser, tok: Token) =
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
    evt = ConfigFileEvent(kind: cekFlagAssignment, flagResult: r,
                          flagInfo: info)
  N.cfgEvtHandler(N.config, evt, instLoc())

proc readConfigFile(N: var NimConfParser, filename: AbsoluteFile,
                    cache: IdentCache): bool =
  ## assumes `cfgEvtHandler` has already been set, do not export
  var
    tok: Token
    stream: PLLStream

  stream = llStreamOpen(filename, fmRead)
  if stream != nil:
    N.handleRead(cekDebugReadStart, filename.string)

    initToken(tok)
    openLexer(N.lexer, filename, stream, cache, N.config)

    # save the existing source of command parameters and use the config file
    let oldCmdLineSrcIdx = N.config.commandLineSrcIdx
    N.config.commandLineSrcIdx = N.lexer.fileIdx

    tok.tokType = tkEof       # to avoid a pointless warning
    confTok(N, tok)           # read in the first token

    while tok.tokType != tkEof:
      parseAssignment(N, tok)

    # restore to the previous source of command parameters
    N.config.commandLineSrcIdx = oldCmdLineSrcIdx

    if N.condStack.len > 0:
      handleError(N, cekParseExpectedX, "@end")

    closeLexer(N.lexer)

    N.handleRead(cekDebugReadStop, filename.string)

    return true

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

proc loadConfigs(
    N: var NimConfParser, cfg: RelativeFile, cache: IdentCache
  ) =
  setDefaultLibpath(N.config)

  proc readConfigFile(N: var NimConfParser, configPath: AbsoluteFile) =
    if readConfigFile(N, configPath, cache):
      N.config.configFiles.add(configPath)

  if optSkipSystemConfigFile notin N.config.globalOptions:
    N.readConfigFile(getSystemConfigPath(N.config, cfg))

  if optSkipUserConfigFile notin N.config.globalOptions:
    N.readConfigFile(getUserConfigPath(cfg))

  let pd = if N.config.projectPath.isEmpty:
             AbsoluteDir(getCurrentDir())
           else:
             N.config.projectPath

  if optSkipParentConfigFiles notin N.config.globalOptions:
    for dir in parentDirs(pd.string, fromRoot=true, inclusive=false):
      N.readConfigFile(AbsoluteDir(dir) / cfg)

  if optSkipProjConfigFile notin N.config.globalOptions:
    N.readConfigFile(pd / cfg)

    if N.config.projectName.len != 0:
      # new project wide config file:
      var projectConfig = changeFileExt(N.config.projectFull, "nimcfg")
      if not fileExists(projectConfig):
        projectConfig = changeFileExt(N.config.projectFull, "nim.cfg")
      N.readConfigFile(projectConfig)

  for filename in N.config.configFiles:
    # delayed to here so that `hintConf` is honored
    N.handleRead(cekProgressConfStart, filename.string)

proc loadConfigs*(
    cfg: RelativeFile; cache: IdentCache;
    conf: ConfigRef, cfgEvtHandler: NimConfEvtHandler,
    cfgScriptEvtReceiver: ConfScriptEvtReceiver
  ) {.inline.} =
  var parser = NimConfParser(config: conf, cfgEvtHandler: cfgEvtHandler,
                             scriptEvtReceiver: cfgScriptEvtReceiver)
  parser.loadConfigs(cfg, cache)

  
