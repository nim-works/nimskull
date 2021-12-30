import reports, ast, types, renderer
import options as compiler_options
import std/[strutils, terminal, options]

proc msg(conf: ConfigRef, args: varargs[string, `$`]) =
  echo args.join("")

func wrap(str: string, color: ForegroundColor): string =
  result.add "\e["
  result.add $color.int
  result.add "m"
  result.add str
  result.add "\e[0m"

const
  reportTitles: array[ReportSeverity, string] = [
    "Debug: ", "Hint: ", "Warning: ", "Error: ", "Fatal: ", "Trace: "
  ]

  reportColors: array[ReportSeverity, ForegroundColor] = [
    fgDefault, fgGreen, fgYellow, fgRed, fgRed, fgCyan
  ]

proc writeContext*(conf: ConfigRef, ctx: seq[ReportContext]) =
  for ctx in items(ctx):
    case ctx.kind:
      of sckInstantiationOf:
        conf.msg(
          "template/generic instantiation of `",
          ctx.entry.name.s,
          "` from here")

      of sckInstantiationFrom:
        conf.msg("template/generic instantiation from here")

# proc presentFailedCandidates(
#     conf: ConfigRef,
#     n: PNode,
#     errors: seq[SemCallMismatch]
#   ): (TPreferedDesc, string) =

#   var prefer = preferName
#   # to avoid confusing errors like:
#   #   got (SslPtr, SocketHandle)
#   #   but expected one of:
#   #   openssl.SSL_set_fd(ssl: SslPtr, fd: SocketHandle): cint
#   # we do a pre-analysis. If all types produce the same string, we will add
#   # module information.
#   let proto = "" # describeArgs(c, n, 1, preferName)
#   for err in errors:
#     var errProto = ""
#     let n = err.target.typ.n
#     for i in 1 ..< n.len:
#       var p = n[i]
#       if p.kind == nkSym:
#         errProto.add(typeToString(p.sym.typ, preferName))
#         if i != n.len - 1:
#           errProto.add(", ")

#     if errProto == proto:
#       prefer = preferModuleInfo
#       break

#   var filterOnlyFirst = false
#   if optShowAllMismatches notin c.config.globalOptions:
#     for err in errors:
#       if err.arg > 1:
#         filterOnlyFirst = true
#         break

#   var
#     maybeWrongSpace = false
#     candidatesAll: seq[string]
#     candidates = ""
#     skipped = 0

#   for err in errors:
#     candidates.setLen 0
#     if filterOnlyFirst and err.arg == 1:
#       inc skipped
#       continue

#     if err.target in routineKinds and err.sym.ast != nil:
#       candidates.add(renderTree(err.sym.ast,
#             {renderNoBody, renderNoComments, renderNoPragmas}))
#     else:
#       candidates.add(getProcHeader(c.config, err.sym, prefer))

#     candidates.addDeclaredLocMaybe(c.config, err.sym)
#     candidates.add("\n")

#     let nArg = if err.arg < n.len: n[err.arg] else: nil

#     let nameParam = if err.targetArg != nil: err.targetArg.name.s else: ""
#     if n.len > 1:
#       candidates.add("  first type mismatch at position: " & $err.arg)
#       # candidates.add "\n  reason: " & $err.firstMismatch.kind # for debugging
#       case err.kind:
#         of kUnknownNamedParam:
#           if nArg == nil:
#             candidates.add("\n  unknown named parameter")
#           else:
#             candidates.add("\n  unknown named parameter: " & $nArg[0])

#         of kAlreadyGiven:
#           candidates.add("\n  named param already provided: " & $nArg[0])

#         of kPositionalAlreadyGiven:
#           candidates.add("\n  positional param was already given as named param")

#         of kExtraArg:
#           candidates.add("\n  extra argument given")

#         of kMissingParam:
#           candidates.add("\n  missing parameter: " & nameParam)

#         of kTypeMismatch, kVarNeeded:
#           doAssert nArg != nil
#           let wanted = err.targetArg.typ
#           doAssert err.targetArg != nil

#           candidates.add("\n  required type for " & nameParam &  ": ")
#           candidates.addTypeDeclVerboseMaybe(c.config, wanted)
#           candidates.add "\n  but expression '"

#           if err.kind == kVarNeeded:
#             candidates.add renderNotLValue(nArg)
#             candidates.add "' is immutable, not 'var'"

#           else:
#             candidates.add renderTree(nArg)
#             candidates.add "' is of type: "

#             let got = nArg.typ
#             candidates.addTypeDeclVerboseMaybe(conf, got)
#             doAssert wanted != nil

#             if got != nil:
#               if got.kind == tyProc and wanted.kind == tyProc:
#                 # These are proc mismatches so,
#                 # add the extra explict detail of the mismatch
#                 candidates.addPragmaAndCallConvMismatch(wanted, got, c.config)
#               effectProblem(wanted, got, candidates, c)

#         of kUnknown:
#           discard "do not break 'nim check'"

#       candidates.add "\n"
#       if err.arg == 1 and nArg.kind == nkTupleConstr and
#           n.kind == nkCommand:
#         maybeWrongSpace = true

#     for diag in err.diagnostics:
#       candidates.add(diag & "\n")

#     candidatesAll.add candidates

#   candidatesAll.sort # fix #13538
#   candidates = join(candidatesAll)

#   if skipped > 0:
#     candidates.add(
#       $skipped &
#         " other mismatching symbols have been " &
#         "suppressed; compile with --showAllMismatches:on to see them\n")

#   if maybeWrongSpace:
#     candidates.add("maybe misplaced space between " & renderTree(n[0]) & " and '(' \n")

#   result = (prefer, candidates)

proc toStr(r: SemReport): string = $r

proc toStr(conf: ConfigRef, loc: ReportLineInfo): string = $loc

proc report(conf: ConfigRef, r: SemReport)      =
  let sev = conf.severity(r)
  if sev == rsevError:
    conf.writeContext(r.context)

  conf.msg(
    # Optional report location
    if r.location.isSome():
      conf.toStr(r.location.get()) & " "
    else:
      "",

    # `Hint: `, `Error: ` etc.
    wrap(reportTitles[sev], reportColors[sev]),

    # Message body
    toStr(r),

    # Trailing report message - `[GcMem]`
    if sev in {rsevError, rsevWarning}:
      wrap(" [" & $r.kind & "]", fgCyan)

    else:
      ""
  )


proc report(conf: ConfigRef, r: ParserReport)   = echo r
proc report(conf: ConfigRef, r: LexerReport)    = echo r
proc report(conf: ConfigRef, r: InternalReport) = echo r
proc report(conf: ConfigRef, r: ExternalReport) = echo r
proc report(conf: ConfigRef, r: DebugReport)    = echo r
proc report(conf: ConfigRef, r: BackendReport)  = echo r
proc report(conf: ConfigRef, r: CmdReport)      = echo r

proc reportHook*(conf: ConfigRef, r: Report) =
  if not conf.isEnabled(r): return

  case r.category:
    of repLexer:    conf.report(r.lexReport)
    of repParser:   conf.report(r.parserReport)
    of repCmd:      conf.report(r.cmdReport)
    of repSem:      conf.report(r.semReport)
    of repDebug:    conf.report(r.debugReport)
    of repInternal: conf.report(r.internalReport)
    of repBackend:  conf.report(r.backendReport)
    of repExternal: conf.report(r.externalReport)
