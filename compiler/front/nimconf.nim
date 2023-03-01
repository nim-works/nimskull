#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# This module handles the reading of the config file.

import
  std/[
    os,
    strutils,
    strtabs,
  ],
  compiler/front/[
    commands,
    options,
    scriptconfig
  ],
  compiler/ast/[
    lexer,
    idents,
    wordrecg,
    llstream,
    lineinfos,
    ast
  ],
  compiler/utils/[
    pathutils,
  ]

type
  ConfigEventKind* = enum
    ## events/errors arising from parsing and processing a compiler config file

    # fatal errors begin
    cekInternalError
    # fatal errors end

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
    case kind*: ConfigEventKind:
      of cekParseExpectedX, cekParseExpectedCloseX, cekParseExpectedIdent,
         cekInvalidDirective, cekWriteConfig, cekDebugTrace:
        location*: TLineInfo         ## diagnostic location
      of cekInternalError, cekLexerErrorDiag, cekLexerWarningDiag,
         cekLexerHintDiag:
        lexerDiag*: LexerDiag
      of cekDebugReadStart, cekDebugReadStop, cekProgressConfStart:
        discard
    instLoc*: InstantiationInfo ## instantiation in lexer's source
    msg*: string
  
  NimConfEvtHandler* = proc(config: ConfigRef,
                            evt: ConfigFileEvent,
                            reportFrom: InstantiationInfo,
                            eh: TErrorHandling = doNothing): void
  NimConfParser = object
    lexer: Lexer
    condStack: seq[bool]
    config: ConfigRef
    cfgEvtHandler: NimConfEvtHandler


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
      N.cfgEvtHandler(N.lexer.config, e, instLoc(-1), doAbort)
    
    for d in N.lexer.errorsHintsAndWarnings():
      {.cast(uncheckedAssign).}:
        let e = ConfigFileEvent(kind: (case d.kind
                                      of LexDiagsError:   cekLexerErrorDiag
                                      of LexDiagsWarning: cekLexerWarningDiag
                                      of LexDiagsHint:    cekLexerHintDiag
                                      ),
                                lexerDiag: d,
                                instLoc: d.instLoc)
      N.cfgEvtHandler(N.lexer.config, e, instLoc(-1), doNothing)

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
  if percent:
    processSwitch(s, strtabs.`%`(val, N.config.configVars,
                                {useEnvironment, useEmpty}), passPP, info,
                                N.config)
  else:
    processSwitch(s, val, passPP, info, N.config)

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
    tok.tokType = tkEof       # to avoid a pointless warning
    confTok(N, tok)           # read in the first token

    while tok.tokType != tkEof:
      parseAssignment(N, tok)

    if N.condStack.len > 0:
      handleError(N, cekParseExpectedX, "@end")

    closeLexer(N.lexer)

    N.handleRead(cekDebugReadStop, filename.string)

    return true

proc readConfigFile*(filename: AbsoluteFile, cache: IdentCache,
                     config: ConfigRef, evtHandler: NimConfEvtHandler
): bool {.inline.} =
  # set the event handler so we can report
  var parser = NimConfParser(config: config, cfgEvtHandler: evtHandler)
  readConfigFile(parser, filename, cache)

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
    N: var NimConfParser, cfg: RelativeFile, cache: IdentCache,
    idgen: IdGenerator
  ) =
  setDefaultLibpath(N.config)

  proc readConfigFile(N: var NimConfParser, path: AbsoluteFile) =
    let configPath = path
    if readConfigFile(N, configPath, cache):
      N.config.configFiles.add(configPath)

  proc runNimScriptIfExists(N: var NimConfParser, path: AbsoluteFile,
                            isMain = false) =
    let p = path # eval once
    var s: PLLStream
    if isMain and optWasNimscript in N.config.globalOptions:
      if N.config.projectIsStdin:
        s = stdin.llStreamOpen
      elif N.config.projectIsCmd:
        s = llStreamOpen(N.config.cmdInput)

    if s == nil and fileExists(p):
      s = llStreamOpen(p, fmRead)

    if s != nil:
      N.config.configFiles.add(p)
      runNimScript(cache, p, idgen, freshDefines = false, N.config, s)

  if optSkipSystemConfigFile notin N.config.globalOptions:
    N.readConfigFile(getSystemConfigPath(N.config, cfg))

    if cfg == DefaultConfig:
      N.runNimScriptIfExists(getSystemConfigPath(N.config, DefaultConfigNims))

  if optSkipUserConfigFile notin N.config.globalOptions:
    N.readConfigFile(getUserConfigPath(cfg))

    if cfg == DefaultConfig:
      N.runNimScriptIfExists(getUserConfigPath(DefaultConfigNims))

  let pd = if not N.config.projectPath.isEmpty:
             N.config.projectPath
           else:
             AbsoluteDir(getCurrentDir())

  if optSkipParentConfigFiles notin N.config.globalOptions:
    for dir in parentDirs(pd.string, fromRoot=true, inclusive=false):
      N.readConfigFile(AbsoluteDir(dir) / cfg)
      if cfg == DefaultConfig:
        N.runNimScriptIfExists(AbsoluteDir(dir) / DefaultConfigNims)

  if optSkipProjConfigFile notin N.config.globalOptions:
    N.readConfigFile(pd / cfg)
    if cfg == DefaultConfig:
      N.runNimScriptIfExists(pd / DefaultConfigNims)

    if N.config.projectName.len != 0:
      # new project wide config file:
      var projectConfig = changeFileExt(N.config.projectFull, "nimcfg")
      if not fileExists(projectConfig):
        projectConfig = changeFileExt(N.config.projectFull, "nim.cfg")
      N.readConfigFile(projectConfig)

  let
    scriptFile = N.config.projectFull.changeFileExt("nims")
    scriptIsProj = scriptFile == N.config.projectFull
  
  template showHintConf =
    for filename in N.config.configFiles:
      # delayed to here so that `hintConf` is honored
      N.handleRead(cekProgressConfStart, filename.string)

  if N.config.cmd == cmdNimscript:
    showHintConf()
    N.config.configFiles.setLen 0
  
  if N.config.cmd != cmdIdeTools:
    if N.config.cmd == cmdNimscript:
      N.runNimScriptIfExists(N.config.projectFull, isMain = true)
    else:
      N.runNimScriptIfExists(scriptFile, isMain = true)
  else:
    if not scriptIsProj:
      N.runNimScriptIfExists(scriptFile, isMain = true)
    else:
      # 'nimsuggest foo.nims' means to just auto-complete the NimScript file
      discard
  
  showHintConf()

proc loadConfigs*(
    cfg: RelativeFile; cache: IdentCache;
    conf: ConfigRef; idgen: IdGenerator;
    evtHandler: NimConfEvtHandler
  ) {.inline.} =
  var parser = NimConfParser(config: conf, cfgEvtHandler: evtHandler)
  parser.loadConfigs(cfg, cache, idgen)

  
