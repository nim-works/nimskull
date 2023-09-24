import std/[algorithm, hashes, os, osproc, sets,
            streams, strformat, strutils, tables, logging]
import nimlsppkg/[baseprotocol, logger, suggestlib, utfmapping, utils]
include nimlsppkg/[messages, messageenums]

const
  # This is used to explicitly set the default source path
  explicitSourcePath {.strdefine.} = getCurrentCompilerExe().parentDir.parentDir
  VerionMisMatch = "Current Nim version $1 does not match the NimLSP is built against $2"

var
  nimpath = explicitSourcePath
  gotShutdown = false
  initialized = false
  projectFiles = initTable[string, tuple[nimsuggest: NimSuggest, openFiles: OrderedSet[string]]]()
  openFiles = initTable[string, tuple[projectFile: string, fingerTable: seq[seq[tuple[u16pos, offset: int]]]]]()

proc debugSuggests(suggests: seq[Suggest]) =
  for sug in suggests:
    debug logFormat(sug)
  flushLog()

template location(req: untyped): string =
  let lineCol = "(" & $(req.rawLine() + 1) & ":" & $openFiles.col(req) & ")"
  req.filePath & lineCol

template uriAndStash(req: untyped): string =
  "uri: " &  req.fileuri & "stash: " & req.filestash

template fileuri(p: untyped): string =
  p["textDocument"]["uri"].getStr

template filePath(p: untyped): string =
  p.fileuri[7..^1]

template filestash(p: untyped): string =
  storage / (hash(p.fileuri).toHex & ".nim" )

template rawLine(p: untyped): int =
  p["position"]["line"].getInt

template rawChar(p: untyped): int =
  p["position"]["character"].getInt

template col(openFiles: typeof openFiles; p: untyped): int =
  openFiles[p.fileuri].fingerTable[p.rawLine].utf16to8(p.rawChar)

template textDocumentRequest(message, kind: typed; name, body: untyped): untyped =
  if message.hasKey("params"):
    let p = message["params"]
    var name = kind(p)
    if p.isValid(kind, allowExtra = false):
      body
    else:
      debugLog("Unable to parse data as ", kind)

template textDocumentNotification(message, kind: typed; name, body: untyped): untyped =
  if message.hasKey("params"):
    var p = message["params"]
    var name = kind(p)
    if p.isValid(kind, allowExtra = false):
      if "languageId" notin name["textDocument"] or 
       name["textDocument"]["languageId"].getStr == "nim":
        body
      else:
        debugLog("Unable to parse data as ", kind)

proc respond(outs: Stream, req: JsonNode, data: JsonNode) =
  let id = parseId(req["id"])
  let resp = create(ResponseMessage, "2.0", id, some(data), none(ResponseError))
  outs.sendJson resp.JsonNode

proc request(outs: Stream, id: string, meth: string, data: JsonNode) =
  let resp = create(RequestMessage, "2.0", id, meth, some(data))
  outs.sendJson resp.JsonNode

proc error(outs: Stream, req: JsonNode, code: ErrorCode, msg: string, data: JsonNode) =
  let err = some(create(ResponseError, ord(code), msg, data))
  let id = parseId(req{"id"})
  let resp = create(ResponseMessage, "2.0", id, none(JsonNode), err)
  outs.sendJson resp.JsonNode

proc notify(outs: Stream, notification: string, data: JsonNode) =
  let resp = create(NotificationMessage, "2.0", notification, some(data))
  outs.sendJson resp.JsonNode

proc publishDiagnostics(outs: Stream, uri:string, diagnostics: seq[Diagnostic]) =
  let data = create(PublishDiagnosticsParams, uri, diagnostics)
  notify(outs, "textDocument/publishDiagnostics", data.JsonNode)

proc workDoneProgressCreate(outs: Stream, id: string, token: string) =
  # https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#serverInitiatedProgress
  let data = create(WorkDoneProgressCreateParams, token)
  outs.request(id, "window/workDoneProgress/create", data.JsonNode)

proc progressBegin(outs: Stream, title: string, cancellable = none(bool),
   message = none(string), percentage = none(int)) =
  let data = create(WorkDoneProgressBegin, "begin", title, cancellable, message, percentage)
  outs.notify("$/progress", data.JsonNode)

proc progressReport(outs: Stream, cancellable = none(bool),
   message = none(string), percentage = none(int)) =
  let data = create(WorkDoneProgressReport, "report", cancellable, message, percentage)
  outs.notify("$/progress", data.JsonNode)

proc progressEnd(outs: Stream, message = none(string)) =
  let data = create(WorkDoneProgressEnd, "end", message)
  outs.notify("$/progress", data.JsonNode)

proc createRange(line, col, length: int): Range =
  create(Range,
    create(Position, line, col),
    create(Position, line, col + length)
  )

template getNimsuggest(fileuri: string): Nimsuggest =
  projectFiles[openFiles[fileuri].projectFile].nimsuggest

proc checkVersion(outs: Stream) =
  # xxx: usable when the server deployment seperately
  let
    nimoutputTuple =
      execCmdEx("nim --version", options = {poEvalCommand, poUsePath})
  if nimoutputTuple.exitcode == 0:
    let
      nimoutput = nimoutputTuple.output
      versionStart = "Nim Compiler Version ".len
      version = nimoutput[versionStart..<nimoutput.find(" ", versionStart)]
      #hashStart = nimoutput.find("git hash") + 10
      #hash = nimoutput[hashStart..nimoutput.find("\n", hashStart)]
    if version != NimVersion:
      let 
        text = VerionMisMatch % [version, NimVersion]
        msg = create(ShowMessageParams, MessageType.Warning.int, message = text)
      outs.notify("window/showMessage", msg.JsonNode)

proc createMarkupContent(label: string; content: string): MarkupContent =
  let label = "```nim\n" & label & "\n```\n"
  result = create(MarkupContent, "markdown", label & rstToMarkdown(content))

proc createMarkupContent(sug: Suggest): MarkupContent =
  var label = sug.qualifiedPath.join(".")
  if sug.forth != "":
    label &= ": "
    label &= sug.forth
  createMarkupContent(label, sug.doc)

proc createDiagnostic(sug: Suggest): Diagnostic =
  let
    message = sug.doc
    endcolumn = sug.column + message.rfind('\'') - message.find('\'') - 1
  result = create(Diagnostic,
    create(Range,
      create(Position, sug.line-1, sug.column),
      create(Position, sug.line-1, max(sug.column, endcolumn))
    ),
    some(case sug.forth:
      of "Error": DiagnosticSeverity.Error.int
      of "Hint": DiagnosticSeverity.Hint.int
      of "Warning": DiagnosticSeverity.Warning.int
      else: DiagnosticSeverity.Error.int),
    none(int),
    some("nimsuggest chk"),
    message,
    none(seq[DiagnosticRelatedInformation])
  )

proc main(ins: Stream, outs: Stream) =
  # checkVersion(outs) xxx: enable when deployment seperately
  var message: JsonNode
  var frame: string
  while true:
    try:
      debugLog "Trying to read message"
      frame = ins.readFrame
      message = frame.parseJson
      if isValid(message, RequestMessage):
        debugLog "Got valid Request message of type ", message["method"].getStr
        if not initialized and message["method"].getStr != "initialize":
          const msg = "Unable to accept requests before being initialized"
          outs.error(message, ServerNotInitialized, msg, newJNull())
          continue
        case message["method"].getStr:
          of "shutdown":
            let resp = newJNull()
            outs.respond(message, resp)
            gotShutdown = true
          of "initialize":
            # xxx handle InitializeParams
            initialized = true
            let resp = create(InitializeResult, create(ServerCapabilities,
              textDocumentSync = some(create(TextDocumentSyncOptions,
                openClose = some(true),
                change = some(TextDocumentSyncKind.Full.int),
                willSave = some(false),
                willSaveWaitUntil = some(false),
                save = some(create(SaveOptions, some(true)))
              )), # ?: TextDocumentSyncOptions or int or float
              hoverProvider = some(true), # ?: bool
              completionProvider = some(create(CompletionOptions,
                resolveProvider = some(false),
                triggerCharacters = some(@["."])
              )), # ?: CompletionOptions
              signatureHelpProvider = some(create(SignatureHelpOptions,
                triggerCharacters = some(@["(", ","])
              )), # ?: SignatureHelpOptions
              definitionProvider = some(true), #?: bool
              typeDefinitionProvider = none(bool), #?: bool or TextDocumentAndStaticRegistrationOptions
              implementationProvider = none(bool), #?: bool or TextDocumentAndStaticRegistrationOptions
              referencesProvider = some(true), #?: bool
              documentHighlightProvider = none(bool), #?: bool
              documentSymbolProvider = some(true), #?: bool
              workspaceSymbolProvider = none(bool), #?: bool
              codeActionProvider = none(bool), #?: bool
              codeLensProvider = none(CodeLensOptions), #?: CodeLensOptions
              documentFormattingProvider = none(bool), #?: bool
              documentRangeFormattingProvider = none(bool), #?: bool
              documentOnTypeFormattingProvider = none(DocumentOnTypeFormattingOptions), #?: DocumentOnTypeFormattingOptions
              renameProvider = some(true), #?: bool
              documentLinkProvider = none(DocumentLinkOptions), #?: DocumentLinkOptions
              colorProvider = none(bool), #?: bool or ColorProviderOptions or TextDocumentAndStaticRegistrationOptions
              executeCommandProvider = none(ExecuteCommandOptions), #?: ExecuteCommandOptions
              workspace = none(WorkspaceCapability), #?: WorkspaceCapability
              experimental = none(JsonNode) #?: any
            )).JsonNode
            outs.respond(message,resp)
          of "textDocument/completion":
            textDocumentRequest(message, CompletionParams, req):
              debugLog location(req)
              let suggestions = getNimsuggest(req.fileuri).sug(req.filePath, req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: " & $suggestions.len
              debugSuggests(suggestions[0 ..< min(suggestions.len, 10)])
              var
                completionItems = newJarray()
                seenLabels: CountTable[string]
                addedSuggestions: HashSet[string]
              for suggestion in suggestions:
                seenLabels.inc suggestion.collapseByIdentifier
              for i in 0..suggestions.high:
                let
                  suggestion = suggestions[i]
                  collapsed = suggestion.collapseByIdentifier
                if not addedSuggestions.contains collapsed:
                  addedSuggestions.incl collapsed
                  let
                    seenTimes = seenLabels[collapsed]
                    detail =
                      if seenTimes == 1: some(nimSymDetails(suggestion))
                      else: some(&"[{seenTimes} overloads]")
                  completionItems.add create(CompletionItem,
                    label = suggestion.qualifiedPath[^1].strip(chars = {'`'}),
                    kind = some(nimSymToLSPKind(suggestion).int),
                    tags = some(suggestion.flags.mapIt(it.int)),
                    detail = detail,
                    documentation = some(createMarkupContent(suggestion)),
                    deprecated = none(bool),
                    preselect = none(bool),
                    sortText = some(fmt"{i:04}"),
                    filterText = none(string),
                    insertText = none(string),
                    insertTextFormat = none(int),
                    textEdit = none(TextEdit),
                    additionalTextEdits = none(seq[TextEdit]),
                    commitCharacters = none(seq[string]),
                    command = none(Command),
                    data = none(JsonNode)
                  ).JsonNode
              outs.respond(message, completionItems)
          of "textDocument/hover":
            textDocumentRequest(message, TextDocumentPositionParams, req):
              debugLog location(req)
              let suggestions = getNimsuggest(req.fileuri).def(req.filePath, req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: " & $suggestions.len
              debugSuggests(suggestions[0 ..< min(suggestions.len, 10)])
              var resp: JsonNode
              if suggestions.len == 0:
                resp = newJNull()
              else:
                let
                  length = suggestions[0].qualifiedPath[^1].len
                  rangeopt = some(createRange(req.rawLine, req.rawChar, length))
                  markupContent = createMarkupContent(suggestions[0])
                resp = create(Hover, markupContent, rangeopt).JsonNode
              outs.respond(message, resp)
          of "textDocument/references":
            textDocumentRequest(message, ReferenceParams, req):
              debugLog location(req)
              let suggestions = getNimsuggest(req.fileuri).use(req.filePath, req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: " & $suggestions.len
              debugSuggests(suggestions[0 ..< min(suggestions.len, 10)])
              var response = newJarray()
              for sug in suggestions:
                if sug.section == ideUse or req["context"]["includeDeclaration"].getBool:
                  response.add create(Location,
                    "file://" & pathToUri(sug.filepath),
                    createRange(sug.line-1, sug.column, sug.qualifiedPath[^1].len)
                  ).JsonNode
              if response.len == 0:
                outs.respond(message, newJNull())
              else:
                outs.respond(message, response)
          of "textDocument/rename":
            textDocumentRequest(message, RenameParams, req):
              debugLog location(req)
              let suggestions = getNimsuggest(req.fileuri).use(req.filePath, req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: " & $suggestions.len
              debugSuggests(suggestions[0..<min(suggestions.len, 10)])
              var resp: JsonNode
              if suggestions.len == 0:
                resp = newJNull()
              else:
                var textEdits = newJObject()
                for sug in suggestions:
                  let uri = "file://" & pathToUri(sug.filepath)
                  if uri notin textEdits:
                    textEdits[uri] = newJArray()
                  textEdits[uri].add create(TextEdit,
                    createRange(sug.line-1, sug.column, sug.qualifiedPath[^1].len),
                    req["newName"].getStr
                  ).JsonNode
                resp = create(WorkspaceEdit,
                  some(textEdits),
                  none(seq[TextDocumentEdit])
                ).JsonNode
              outs.respond(message, resp)
          of "textDocument/definition":
            textDocumentRequest(message, TextDocumentPositionParams, req):
              debugLog location(req)
              let declarations = getNimsuggest(req.fileuri).def(req.filePath, req.filestash,
                req.rawLine + 1,
                openFiles.col(req)
              )
              debugLog "Found suggestions: " & $declarations.len
              debugSuggests(declarations[0..<min(declarations.len, 10)])
              var resp: JsonNode
              if declarations.len == 0:
                resp = newJNull()
              else:
                resp = newJarray()
                for decl in declarations:
                  resp.add create(Location,
                    "file://" & pathToUri(decl.filepath),
                    createRange(decl.line-1,  decl.column, decl.qualifiedPath[^1].len)
                  ).JsonNode
              outs.respond(message, resp)
          of "textDocument/documentSymbol":
            textDocumentRequest(message, DocumentSymbolParams, req):
              debugLog req.uriAndStash()
              let syms = getNimsuggest(req.fileuri).outline(req.filePath, req.filestash)
              debugLog "Found outlines: " & $syms.len
              debugSuggests(syms[0..<min(syms.len, 10)])
              var resp: JsonNode
              if syms.len == 0:
                resp = newJNull()
              else:
                resp = newJarray()
                for sym in syms.sortedByIt((it.line,it.column)):
                  if sym.qualifiedPath.len != 2:
                    continue
                  resp.add create(DocumentSymbol,
                    sym.qualifiedPath[^1],
                    some(symKindToString(sym.symKind)),
                    nimSymToLSPKind(sym.symKind).int,
                    some(sym.flags.mapIt(it.int)),
                    createRange(sym.line-1, sym.column, sym.tokenLen),
                    createRange(sym.line-1, sym.column, sym.tokenLen),
                    none(seq[DocumentSymbol])).JsonNode
              outs.respond(message, resp)
          of "textDocument/signatureHelp":
            textDocumentRequest(message, TextDocumentPositionParams, req):
              debugLog location(req)
              let suggestions = getNimsuggest(req.fileuri).con(req.filePath, req.filestash,
               req.rawLine + 1, req.rawChar)
              var signatures = newSeq[SignatureInformation]()
              for suggestion in suggestions:
                var label = suggestion.qualifiedPath.join(".")
                if suggestion.forth != "":
                  label &= ": "
                  label &= suggestion.forth
                signatures.add create(SignatureInformation,
                  label = label,
                  documentation = some(rstToMarkdown(suggestion.doc)),
                  # TODO: fill parameters info
                  parameters = none(seq[ParameterInformation])
                )
              let resp = create(SignatureHelp,
                signatures = signatures,
                activeSignature = some(0),
                activeParameter = some(0)
              ).JsonNode
              outs.respond(message, resp)
          else:
            let msg = "Unknown request method: " & message["method"].getStr
            debugLog msg
            outs.error(message, MethodNotFound, msg, newJObject())
        continue
      elif isValid(message, NotificationMessage):
        debugLog "Got valid Notification message of type ", message["method"].getStr
        if not initialized and message["method"].getStr != "exit":
          continue
        case message["method"].getStr:
          of "exit":
            if gotShutdown:
              quit 0
            else:
              quit 1
          of "initialized":
            discard
          of "textDocument/didOpen":
            textDocumentNotification(message, DidOpenTextDocumentParams, req):
              let
                file = open(req.filestash, fmWrite)
                projectFile = getProjectFile(uriToPath(req.fileuri))
              debugLog req.uriAndStash()
              openFiles[req.fileuri] = (projectFile: projectFile, fingerTable: @[])
              if projectFile notin projectFiles:
                debugLog "Initialising with project file: ", projectFile
                projectFiles[projectFile] = (nimsuggest: initNimsuggest(projectFile, nimpath),
                 openFiles: initOrderedSet[string]())
              projectFiles[projectFile].openFiles.incl(req.fileuri)

              for line in req["textDocument"]["text"].getStr.splitLines:
                openFiles[req.fileuri].fingerTable.add line.createUTFMapping()
                file.writeLine line
              file.close()
              let diagnostics = getNimsuggest(req.fileuri).fetchCachedReports(req.filePath)
              debugLog "Got cached diagnostics: " & $diagnostics.len
              debugSuggests(diagnostics[0..<min(diagnostics.len, 10)])
              var data = newSeq[Diagnostic]()
              for diagnostic in diagnostics:
                if diagnostic.line == 0:
                  continue
                if diagnostic.filePath != uriToPath(req.fileuri):
                  continue
                data.add createDiagnostic(diagnostic)
              outs.publishDiagnostics(req.fileuri, data)
          of "textDocument/didChange":
            textDocumentNotification(message, DidChangeTextDocumentParams, req):
              let file = open(req.filestash, fmWrite)
              debugLog req.uriAndStash()
              openFiles[req.fileuri].fingerTable = @[]
              for line in req["contentChanges"][0]["text"].getStr.splitLines:
                openFiles[req.fileuri].fingerTable.add line.createUTFMapping()
                file.writeLine line
              file.close()
              # Notify nimsuggest about a file modification.
              discard getNimsuggest(req.fileuri).mod(req.filePath, req.filestash)
          of "textDocument/didClose":
            textDocumentNotification(message, DidCloseTextDocumentParams, req):
              let projectFile = getProjectFile(uriToPath(req.fileuri))
              debugLog req.uriAndStash()
              removeFile(req.filestash)
              projectFiles[projectFile].openFiles.excl(req.fileuri)
              if projectFiles[projectFile].openFiles.len == 0:
                debugLog "Trying to stop nimsuggest"
                debugLog "Stopped nimsuggest with code: ",
                          getNimsuggest(req.fileuri).stopNimsuggest()
              openFiles.del(req.fileuri)
          of "textDocument/didSave":
            textDocumentNotification(message, DidSaveTextDocumentParams, req):
              if req["text"].isSome:
                let file = open(req.filestash, fmWrite)
                debugLog req.uriAndStash()
                openFiles[req.fileuri].fingerTable = @[]
                for line in req["text"].unsafeGet.getStr.splitLines:
                  openFiles[req.fileuri].fingerTable.add line.createUTFMapping()
                  file.writeLine line
                file.close()
              let diagnostics = getNimsuggest(req.fileuri).chk(req.filePath, req.filestash)
              debugLog "Got diagnostics: " & $diagnostics.len
              debugSuggests(diagnostics[0..<min(diagnostics.len, 10)])
              var data = newSeq[Diagnostic]()
              for diagnostic in diagnostics:
                if diagnostic.line == 0:
                  continue
                if diagnostic.filePath != uriToPath(req.fileuri):
                  continue
                data.add createDiagnostic(diagnostic)

              # Invoke chk on other open files.
              let projectFile = openFiles[req.fileuri].projectFile
              for f in projectFiles[projectFile].openFiles.items:
                if f == req.fileuri: continue
                let diagnostics = getNimsuggest(f).chk(req.filePath, req.filestash)
                debugLog "Got diagnostics: " & $diagnostics.len
                debugSuggests(diagnostics[0 ..< min(diagnostics.len, 10)])
                var data = newSeq[Diagnostic]()
                for diagnostic in diagnostics:
                  if diagnostic.line == 0:
                    continue
                  if diagnostic.filePath != uriToPath(f):
                    continue
                  data.add createDiagnostic(diagnostic)
                outs.publishDiagnostics(f, data)

              outs.publishDiagnostics(req.fileuri, data)
          else:
            let msg = "Unknown notification method: " & message["method"].getStr
            warnLog msg
            outs.error(message, MethodNotFound, msg, newJObject())
        continue
      else:
        let msg = "Invalid message: " & frame
        debugLog msg
        outs.error(message, InvalidRequest, msg, newJObject())
    except MalformedFrame as e:
      warnLog "Got Invalid message id: ", e.msg
      continue
    except UriParseError as e:
      warnLog "Got exception parsing URI: ", e.msg
      continue
    except IOError as e:
      errorLog "Got IOError: ", e.msg
      break
    except CatchableError as e:
      warnLog "Got exception: ", e.msg
      continue

when isMainModule:
  infoLog("explicitSourcePath: ", explicitSourcePath)
  for i in 1..paramCount():
    infoLog("Argument ", i, ": ", paramStr(i))
  if paramCount() == 1:
    case paramStr(1):
      of "--help":
        echo "Usage: nimlsp [OPTION | PATH]\n"
        echo "--help, shows this message"
        echo "PATH, path to the Nim source directory, defaults to \"", nimpath, "\""
        quit 0

      else: nimpath = expandFilename(paramStr(1))
  if not fileExists(nimpath / "config/nim.cfg"):
    stderr.write &"""Unable to find "config/nim.cfg" in "{nimpath
    }". Supply the Nim project folder by adding it as an argument.
  """
    quit 1
  
  var
    ins = newFileStream(stdin)
    outs = newFileStream(stdout)
  main(ins, outs)
