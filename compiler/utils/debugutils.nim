##[
Utilities to help with debugging nimskull compiler.

Experimental API, subject to change.
]##

#[
## example
useful debugging flags:
--stacktrace -d:debug -d:nimDebugUtils
 nim c -o:bin/nim_temp --stacktrace -d:debug -d:nimDebugUtils compiler/nim
]#

import
  compiler/ast/[
    ast_types
  ],
  compiler/backend/[
    cgir
  ],
  compiler/front/[
    options,
    msgs
  ]

# output styling related imports
from std/os import `/`, createDir, dirExists
from std/strutils import `%`, addf, align, alignLeft, repeat, endsWith,
                         multiReplace
from experimental/colortext import `+`,
                                   ForegroundColor,
                                   ColText,
                                   addf,
                                   Style,
                                   toString

from compiler/ast/lineinfos import TLineInfo

from compiler/utils/astrepr import treeRepr,
                                   implicitCompilerTraceReprConf

when defined(nimCompilerStacktraceHints):
  from std/stackframes import setFrameMsg

type
  # xxx: compiler execution tracing is a general thing, not semantic analysis
  #      specific. The types below to be renamed with prefix `Trace` are
  #      general and should move accordingly. This will require further
  #      refactoring of all the report types prefixed with `Debug`... sigh.

  # TODO: rename to `TraceStepDirection` or something, it's not sem specific
  DebugSemStepDirection* = enum semstepEnter, semstepLeave
  
  # TODO: rename to `TraceStepKind` or something, it's not sem specific
  DebugSemStepKind* = enum
    stepNodeToNode
    stepNodeToSym
    stepIdentToSym
    stepSymNodeToNode
    stepNodeFlagsToNode
    stepNodeTypeToNode
    stepTypeTypeToType
    stepResolveOverload
    stepNodeSigMatch
    stepWrongNode
    stepError
    stepTrack

  DebugCallableCandidate* = object
    ## stripped down version of `sigmatch.TCandidate`
    state*: string
    callee*: PType
    calleeSym*: PSym
    calleeScope*: int
    call*: PNode
    error*: SemCallMismatch

  DebugSemStep* = object
    direction*: DebugSemStepDirection
    level*: int
    name*: string
    node*: PNode ## Depending on the step direction this field stores
                 ## either input or output node
    steppedFrom*: InstantiationInfo
    sym*: PSym
    case kind*: DebugSemStepKind
      of stepIdentToSym:
        ident*: PIdent

      of stepNodeTypeToNode, stepTypeTypeToType:
        typ*: PType
        typ1*: PType

      of stepNodeFlagsToNode:
        flags*: TExprFlags

      of stepNodeSigMatch, stepResolveOverload:
        filters*: TSymKinds
        candidate*: DebugCallableCandidate
        errors*: seq[SemCallMismatch]

      else:
        discard
  
  CompilerTraceKind* = enum
    compilerTraceStep
    compilerTraceLine
    compilerTraceStart
    compilerTraceEnd
    compilerTraceDefined   # TODO: rename
    compilerTraceUndefined # TODO: rename

  CompilerTrace* = object
    instLoc*: InstantiationInfo
    case kind*: CompilerTraceKind:
      of compilerTraceStep:
        semstep*: DebugSemStep

      of compilerTraceLine, compilerTraceStart:
        ctraceData*: tuple[level: int, entries: seq[StackTraceEntry]]

      of compilerTraceDefined, compilerTraceUndefined:
        srcLoc*: TLineInfo

      of compilerTraceEnd:
        discard

func add(target: var string, other: varargs[string, `$`]) =
  for item in other:
    target.add item

# Formatting code copied from `cli_reporter` to avoid a dependency on that 
# awful module, these should be pared down to what's necessary/makes sense for
# this modules
func wrap(s: string, color: ForegroundColor, style: set[Style] = {}): string =
  ## Wrap text with ANSI color formatting codes
  if s.len == 0: # don't bother with empty strings
    return s

  result.add("\e[", color.int, "m")
  for s in style:
    result.add("\e[", s.int, "m")

  result.add s
  result.add "\e[0m"

func wrap(conf: ConfigRef, str: string, color: ForegroundColor,
          style: set[Style] = {}): string {.inline.} =
  ## Optionally wrap text in ansi color formatting, if `conf` has coloring
  ## enabled
  if conf.useColor:
    wrap(str, color, style)
  else:
    str

func wrap(conf: ConfigRef, text: ColText): string =
  toString(text, conf.useColor())

func dropExt(path: string, doDrop: bool): string =
  ## Optionally drop `.nim` file extension
  if doDrop and path.endsWith(".nim"): path[0 .. ^5] else: path

proc toStr(conf: ConfigRef, loc: TLineInfo, dropExt: bool = false): string =
  ## Convert `loc`ation to printable string
  conf.wrap(
    "$1($2, $3)" % [
      conf.toMsgFilename(loc.fileIndex).dropExt(dropExt),
      $loc.line,
      $(loc.col + ColOffset)
    ],
    fgDefault,
    {styleBright})

proc toStr(conf: ConfigRef, loc: InstantiationInfo, dropExt: bool = false): string =
  ## Convert `loc`ation to printable string
  conf.wrap(
    "$1($2, $3)" % [
      conf.formatPath(loc.filename).dropExt(dropExt),
      $loc.line,
      $(loc.column + ColOffset)
    ],
    fgDefault,
    {styleBright})

func isValid(info: InstantiationInfo): bool {.inline.} =
  info.filename.len > 0 and info.filename != "???"

const
  dropTraceExt = off
  reportCaller = on

const
  traceDir = "nimCompilerDebugTraceDir"

proc generateOutput(conf: ConfigRef, r: CompilerTrace): string =
  case r.kind:
  of compilerTraceStep:
    let
      s = r.semstep
      indent = s.level * 2 + (
        2 #[ Item indentation ]# +
        5 #[ Global entry indentation ]#
      )
      enter = s.direction == semstepEnter

    proc render(node: PNode): string =
      conf.wrap(conf.treeRepr(node,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    proc render(typ: PType): string =
      conf.wrap(conf.treeRepr(typ,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    proc render(sym: PSym): string =
      conf.wrap(conf.treeRepr(sym,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    proc render(sym: PIdent): string =
      conf.wrap(conf.treeRepr(sym,
                              indent = indent + 2,
                              rconf = implicitCompilerTraceReprConf))

    result.addf("$1]", align($s.level, 2, '#'))
    result.add(
      repeat("  ", s.level),
      if s.direction == semstepEnter: "> " else: "< ",
      conf.wrap(s.name, if s.direction == semstepEnter: fgGreen else: fgRed),
      " @ ",
      conf.wrap(conf.toStr(r.instLoc, dropTraceExt), fgCyan),
      if reportCaller and s.steppedFrom.isValid():
        " from " & conf.toStr(s.steppedFrom, dropTraceExt)
      else:
        "")

    var res = addr result
    proc field(name: string, value = "\n") =
      res[].add "\n"
      res[].add repeat(" ", indent)
      res[].add name
      res[].add ":"
      res[].add value

    if conf.hack.semTraceData:
      # field("kind", $s.kind) # if you're new to reading traces, uncomment

      case s.kind:
      of stepNodeToNode:
        if enter:
          field("from node")
        else:
          field("to node")

        result.add render(s.node)

      of stepSymNodeToNode:
        if enter:
          field("from sym")
          result.add render(s.sym)
          field("from node")
          result.add render(s.node)
        else:
          field("to node")
          result.add render(s.node)

      of stepNodeTypeToNode:
        if enter:
          field("from node")
          result.add render(s.node)
          field("from type")
          result.add render(s.typ)
        else:
          field("to node")
          result.add render(s.node)

      of stepNodeFlagsToNode:
        if enter:
          field("from flags",  " " & conf.wrap($s.flags + fgCyan))
          result.add
          field("from node")
          result.add render(s.node)
        else:
          field("to node")
          result.add render(s.node)

      of stepTrack:
        discard #[ 'track' has no extra data fields ]#

      of stepError, stepWrongNode:
        field("node")
        result.add render(s.node)

      of stepNodeToSym:
        if enter:
          field("from node")
          result.add render(s.node)
        else:
          field("to sym")
          result.add render(s.sym)

      of stepIdentToSym:
        if enter:
          field("from ident")
          result.add render(s.ident)
        else:
          field("to sym")
          result.add render(s.sym)

      of stepTypeTypeToType:
        if enter:
          field("from type")
          result.add render(s.typ)
          field("from type1")
          result.add render(s.typ1)
        else:
          field("to type")
          result.add render(s.typ)
      
      of stepNodeSigMatch, stepResolveOverload:
        if enter:
          field("from node")
          result.add render(s.node)
        else:
          field("match status", s.candidate.state)

          if s.candidate.call.isNil:
            field("mismatch kind", $s.candidate.error.firstMismatch.kind)
          else:
            if s.candidate.calleeSym.isNil:
              field("callee")
              result.add render(s.candidate.callee)
            else:
              field("calleeSym")
              result.add render(s.candidate.calleeSym)
            field("call")
            result.add render(s.candidate.call)
  of compilerTraceLine:
    let ind = repeat("  ", r.ctraceData.level)
    var
      paths: seq[string]
      width = 0
    for entry in r.ctraceData.entries:
      paths.add "$1($2)" % [
        formatPath(conf, $entry.filename), $entry.line]

      width = max(paths[^1].len, width)

    for idx, entry in r.ctraceData.entries:
      result.add(
        "  ]",
        ind, " | ",
        alignLeft(paths[idx], width + 1),
        conf.wrap($entry.procname, fgGreen),
        if idx < r.ctraceData.entries.high: "\n" else: ""
      )

  of compilerTraceStart:
    result = ">>] trace start"

  of compilerTraceEnd:
    result = "<<] trace end"

  of compilerTraceDefined:
    result = ">>] debug trace defined at " & toStr(conf, r.srcLoc)

  of compilerTraceUndefined:
    result = "<<] debug trace undefined " & toStr(conf, r.srcLoc)

# xxx: global state like so is most likely a terrible idea
var
  traceFile: File
  fileIndex: int32 = 0
  counter = 0

proc rotatedTrace(conf: ConfigRef, r: CompilerTrace) =
  ## Write out debug traces into separate files in directory defined by
  ## `nimCompilerDebugTraceDir`
  # Dispatch each `{.define(nimCompilerDebug).}` section into separate file
  case r.kind
  of compilerTraceStart, compilerTraceEnd:
    # Rotated trace is constrolled by the define-undefine pair, not by
    # singular call to the nested recursion handling.
    discard
  of compilerTraceDefined:
    if not dirExists(conf.getDefined(traceDir)):
      createDir conf.getDefined(traceDir)
    counter = 0
    let loc = r.srcLoc
    let path = conf.getDefined(traceDir) / "compiler_trace_$1_$2.nim" % [
      conf.toFilename(loc).multiReplace({"/":    "_", ".nim": ""}),
      $fileIndex]
    echo "$1($2, $3): opening $4 trace" % [
      conf.toFilename(loc), $loc.line, $loc.col, path]
    traceFile = open(path, fmWrite)
    inc fileIndex
  of compilerTraceUndefined:
    let loc = r.srcLoc
    echo "$1($2, $3): closing trace, wrote $4 records" % [
      conf.toFilename(loc), $loc.line, $loc.col, $counter]
    close(traceFile)
  else:
    inc counter
    conf.excl optUseColors
    traceFile.write(conf.generateOutput(r))
    traceFile.write("\n")
    traceFile.flushFile() # Forcefully flush the file in case of abrupt
    # exit by the compiler.
    conf.incl optUseColors

proc outputTrace*(conf: ConfigRef, t: CompilerTrace) =
  ## outputs the `CompilerTrace` to stdout or the file is specified
  if conf.isDefined(traceDir):
    rotatedTrace(conf, t)
  else:
    # xxx: indentation should probably be removed as the output is already very
    #      wide and this makes it plain harder to read
    var indent {.global.}: int
    if t.kind == compilerTraceStep:
      indent = t.semstep.level
    echo conf.generateOutput(t)

proc isCompilerDebug*(conf: ConfigRef): bool {.inline.} =
  ##[
  Provides a simple way for user code to enable/disable logging in the compiler
  in a granular way. This can then be used in the compiler as follows:
  ```nim
  if conf.isCompilerDebug():
    echo n.sym.typ.len
  ```

  Example region to trace:
  ```nim
  proc main =
    echo 2
    {.define(nimCompilerDebug).}
    echo 3.5 # code section in which `isCompilerDebug` will be true
    {.undef(nimCompilerDebug).}
    echo 'x'
  ```
  ]##
  conf.isDefined("nimCompilerDebug")

proc isCompilerTraceDebug*(conf: ConfigRef): bool =
  conf.isCompilerDebug() and conf.isDefined("nimCompilerDebugCalltrace")

template addInNimDebugUtilsAux(conf: ConfigRef; prcname: string;
                                enterMsg, leaveMsg) =
  ## used by one of the dedicated templates in order to output compiler trace
  ## data, use a dedicated template (see below) for actual output. this is a
  ## helper that takes three templates, `enterMsg`, `leaveMsg`, and `getInfo`
  ## that will emit a message when entering and leaving a proc, and getting
  ## the string out of some lineinfo, respectively.
  ##
  ## The dedicate templates take specific parameters and pass in the above
  ## templates with the following signatures:
  ## * enterMsg: indent: string -> string
  ## * leaveMsg: indent: string -> string
  ##
  ## once a specialized template exists, again see below, use at the start of a
  ## proc, typically a high traffic one such as `semExpr` and then this will
  ## output partial traces through the compiler.
  ##
  ## The output is roughly:
  ## 1. begin message with starting location
  ##    a.  a full stacktrace for context
  ## 2. for each proc (nests):
  ##    a. `>prcname plus useful info...`
  ##    b. delta stack trace `| procname filepath(line, col)`
  ##    c. `<prcname plus useful change info...`
  ## 3. end message

  # xxx: as this template develops, eventually all the delta traces will be
  #      replaced with useful debugging output from here so we can inspect
  #      what the compiler is doing. eventually, even that should be superceded
  #      as that sort of transformation and observability should be first class

  when defined(nimDebugUtils): # see `debugutils`
    # do all this at the start of any proc we're debugging
    let
      isDebug = conf.isCompilerTraceDebug()
        ## see if we're in compiler debug mode and also use the fact that we
        ## know this early to see if we just entered or just left

      # determine indentitation levels for output
      indentLevel = conf.debugUtilsStack.len

    {.cast(noSideEffect).}:
      if isDebug:
        conf.debugUtilsStack.add prcname # use this to track deltas
        enterMsg(indentLevel)
        if indentLevel != 0: # print a delta stack
          # try to print only the part of the stacktrace since the last time,
          # this is done by looking for any previous calls in `debugUtilsStack`
          {.line:instantiationInfo(-2, true).}: # stops the template showing up in the StackTraceEntries
            let
              stopProc =
                if indentLevel == 1: prcname  # we're the only ones
                else: conf.debugUtilsStack[^2] # the one before us
              entries = getStackTraceEntries()
              endsWith = entries.len - 1

          # find the actual StackTraceEntry index based on the name
          var startFrom = 0
          for i in countdown(endsWith, 0):
            let e = entries[i]
            if i != endsWith and $e.procname == stopProc: # found the previous
              startFrom = i + 1
              break                                       # skip the rest

          # print the trace oldest (startFrom) to newest (endsWith)
          var trace = CompilerTrace(kind: compilerTraceLine)
          trace.ctraceData.level = indentLevel
          for i in startFrom .. endsWith:
            trace.ctraceData.entries.add entries[i]

          conf.outputTrace(trace)

    # upon leaving the proc being debugged (`defer`), let's see what changed
    defer:
      {.cast(noSideEffect).}:
        if not isDebug and conf.isCompilerTraceDebug():
          # meaning we just analysed a `{.define(nimCompilerDebug).}`
          # it started of as false, now after the proc's work (`semExpr`) this
          # `defer`red logic is seeing `true`, so we must have just started.
          var trace = CompilerTrace(kind: compilerTraceStart)
          {.line.}:
            # don't let the template show up in the StackTrace gives context
            # to the rest of the partial traces we do a full one instead
            trace.ctraceData = (indentLevel, getStackTraceEntries())

          conf.outputTrace(trace)
        elif isDebug and not conf.isCompilerTraceDebug():
          # meaning we just analysed an `{.undef(nimCompilerDebug).}`
          # it started of as true, now in the `defer` it's false
          discard conf.debugUtilsStack.pop()
          conf.outputTrace(CompilerTrace(kind: compilerTraceEnd))
        elif isDebug:
          discard conf.debugUtilsStack.pop()
          leaveMsg(indentLevel)
      discard
  else:
    discard # noop if undefined

type
  StepParams* = object
    ## Parameters necessary to construct new step of the execution tracing.
    c*: ConfigRef
    kind*: DebugSemStepKind
    indentLevel*: int
    action*: string
    info*: InstantiationInfo

proc stepParams*(
    c: ConfigRef,
    kind: DebugSemStepKind,
    indentLevel: int,
    action: string
  ): StepParams =
  StepParams(c: c, kind: kind, indentLevel: indentLevel, action: action)

const hasStacktrace = compileOption"stacktrace"

template calledFromInfo*(): InstantiationInfo =
  {.line.}:
    let e = getStackTraceEntries()[^2]
    (filename: $e.filename, line: e.line, column: 0)

template traceStepImpl*(
    params: StepParams,
    stepDirection: DebugSemStepDirection,
    body: untyped,
  ) =
  ## Construct and write debug step report using given parameters. Mutable
  ## `it: DebugSemStep` is injected and is accessible in the `body` that is
  ## passed to the template. `stepDirection` is assigned to the contructed
  ## step `.direction` field.
  block:
    let p = params
    var it {.inject.} = DebugSemStep(
      direction: stepDirection,
      level: p.indentLevel,
      name: p.action,
      kind: p.kind
    )

    if hasStacktrace:
      {.line.}:
        it.steppedFrom = calledFromInfo()

    block:
      body

    outputTrace(p.c, CompilerTrace(
      kind: compilerTraceStep,
      semstep: it,
      instLoc: params.info)
    )

template traceEnterIt*(
    loc: InstantiationInfo,
    params: StepParams,
    body: untyped,
  ): untyped =
  ## Convenience wrapper around the `traceStepImpl`. If called from user
  ## code `templateDepth` parameter must be specified as well - it controls
  ## depth of the template instantiation location that is need to be
  ## accounted for. With current implementation of the debugutils this
  ## value is set to `-5` (`instDepth` default), for your code it might be
  ## different (`-2` when called directly and `-1` for each wrapper template
  ## level).
  var tmp = params
  tmp.info = loc
  traceStepImpl(tmp, semstepEnter, body)

template traceLeaveIt*(
    loc: InstantiationInfo,
    params: StepParams,
    body: untyped,
  ): untyped =
  ## Convenience wrapper for `traceStepImpl` - for mode details see the
  ## `traceEnterIt` and `traceStepImpl` documentation.
  var tmp = params
  tmp.info = loc
  traceStepImpl(tmp, semstepLeave, body)

template frameMsg*(c: ConfigRef, n: PNode) =
  {.line.}:
    setFrameMsg "$1 $2 $3($4, $5)" % [
      $n.kind,
      $n.id,
      c.toFullPath(n.info.fileIndex),
      $n.info.line,
      $n.info.col]

template frameMsg*(c: ConfigRef, n: CgNode) =
  {.line.}:
    setFrameMsg "$1 $2($3, $4)" % [
      $n.kind,
      c.toFullPath(n.info.fileIndex),
      $n.info.line,
      $n.info.col]

const locOffset = -2

template addInNimDebugUtils*(c: ConfigRef; action: string; n, r: PNode;
                            flags: TExprFlags) =
  ## add tracing to procs that are primarily `PNode -> PNode`, with expr flags
  ## and can determine the type
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepNodeFlagsToNode, indentLevel, action)):
        it.node = n
        it.flags = flags

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepNodeFlagsToNode, indentLevel, action)):
        it.node = r
        it.flags = flags

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string; n, r: PNode) =
  ## add tracing to procs that are primarily `PNode -> PNode`, and can
  ## determine the type
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepNodeToNode, indentLevel, action)):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepNodeToNode, indentLevel, action)):
        it.node = r

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtilsError*(c: ConfigRef; n, e: PNode) =
  ## add tracing error generation `PNode -> PNode`
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const action = "newError"
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepWrongNode, indentLevel, action)):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepError, indentLevel, action)):
        it.node = e

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string; n: PNode;
                            prev, r: PType) =
  ## add tracing to procs that are primarily `PNode, PType|nil -> PType`,
  ## determining a type node, with a possible previous type.
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepNodeTypeToNode, indentLevel, action)):
        it.node = n
        it.typ = prev

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepNodeTypeToNode, indentLevel, action)):
        it.node = n
        it.typ = r

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(
    c: ConfigRef; action: string; n: PNode; resSym: PSym) =
  ## add tracing to procs that are primarily `PNode -> PSym`
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepNodeToSym, indentLevel, action)):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepNodeToSym, indentLevel, action)):
        it.sym = resSym

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(
    c: ConfigRef; action: string; id: PIdent; resSym: PSym) =
  ## add tracing to procs that are primarily `PIdent -> PSym`
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepIdentToSym, indentLevel, action)):
        it.ident = id

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepIdentToSym, indentLevel, action)):
        it.sym = resSym

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(
    c: ConfigRef; action: string; s: PSym; n: PNode; res: PNode) =
  ## add tracing to procs that are primarily `PSym, PNode -> PNode`, such as
  ## applying pragmas to a symbol
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepSymNodeToNode, indentLevel, action)):
        it.sym = s
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepSymNodeToNode, indentLevel, action)):
        it.node = res

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string; x, y, r: PType) =
  ## add tracing to procs that are primarily `PType, PType -> PType`, looking
  ## for a common type
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepTypeTypeToType, indentLevel, action)):
        it.typ = x
        it.typ1 = y

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepTypeTypeToType, indentLevel, action)):
        it.typ = r

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; 
                            action: string;
                            n: PNode;
                            filter: TSymKinds;
                            e: var seq[SemCallMismatch];
                            res: typed) =
  ## add tracing to procs that are primarily `PNode -> TCandidate`, looking for
  ## a candidate callable
  when res is not TCandidate:
    {.error: "parameter `res` must be a `sigmatch.TCandidate`".}
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(
        loc, stepParams(c, stepResolveOverload, indentLevel, action)):
        it.node = n
        it.filters = filter

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(
        loc, stepParams(c, stepResolveOverload, indentLevel, action)):
        it.candidate = toDebugCallableCandidate(res)
        it.errors = e

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; 
                            action: string;
                            n: PNode;
                            res: typed) =
  ## add tracing to procs that are primarily `PNode -> TCandidate`, looking for
  ## a candidate callable
  when res is not TCandidate:
    {.error: "parameter `res` must be a `sigmatch.TCandidate`".}
  when defined(nimCompilerStacktraceHints):
    {.line.}:
      frameMsg(c, n)
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(loc, stepParams(c, stepNodeSigMatch, indentLevel, action)):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(loc, stepParams(c, stepNodeSigMatch, indentLevel, action)):
        it.candidate = toDebugCallableCandidate(res)

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string) =
  ## add tracing to procs as a stop gap measure, not favour using one that
  ## provides more output for various parts
  when defined(nimDebugUtils):
    const loc = instLoc(locOffset)
    template enterMsg(indentLevel: int) =
      traceEnterIt(loc, stepParams(c, stepTrack, indentLevel, action)):
        discard

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(loc, stepParams(c, stepTrack, indentLevel, action)):
        discard

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)
