##[
Utilities to help with debugging nim compiler.

Experimental API, subject to change.
]##

#[
## example
useful debugging flags:
--stacktrace -d:debug -d:nimDebugUtils
 nim c -o:bin/nim_temp --stacktrace -d:debug -d:nimDebugUtils compiler/nim

]#

import
  front/[
    options,
    msgs
  ],
  ast/[
    reports,
  ]

proc isCompilerDebug*(conf: ConfigRef): bool {.inline.} =
  conf.isDefined("nimCompilerDebug")

proc isCompilerTraceDebug*(conf: ConfigRef): bool =
  ##[
  Provides a simple way for user code to enable/disable logging in the compiler
  in a granular way. This can then be used in the compiler as follows:
  ```nim
  if conf.isCompilerDebug():
    echo n.sym.typ.len
  ```
  ]##
  runnableExamples:
    proc main =
      echo 2
      {.define(nimCompilerDebug).}
      echo 3.5 # code section in which `isCompilerDebug` will be true
      {.undef(nimCompilerDebug).}
      echo 'x'
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
        ## see if we're in compiler debug mode and also use the fact that we know
        ## this early to see if we just entered or just left

      # determine indentitation levels for output
      indentString = "  "
      indentLevel = conf.debugUtilsStack.len

    if isDebug:
      conf.debugUtilsStack.add prcname # use this to track deltas
      enterMsg(indentLevel)
      if indentLevel != 0: # print a delta stack
        # try to print only the part of the stacktrace since the last time,
        # this is done by looking for any previous calls in `debugUtilsStack`
        {.line.}: # stops the template showing up in the StackTraceEntries
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
        var rep = DebugReport(kind: rdbgTraceLine)
        rep.ctraceData.level = indentLevel
        for i in startFrom .. endsWith:
          rep.ctraceData.entries.add entries[i]

        conf.localReport(rep)

    # upon leaving the proc being debugged (`defer`), let's see what changed
    defer:
      if not isDebug and conf.isCompilerTraceDebug():
        # meaning we just analysed a `{.define(nimCompilerDebug).}`
        # it started of as false, now after the proc's work (`semExpr`) this
        # `defer`red logic is seeing `true`, so we must have just started.
        var report = DebugReport(kind: rdbgTraceStart)
        {.line.}:
          # don't let the template show up in the StackTrace gives context
          # to the rest of the partial traces we do a full one instead
          report.ctraceData = (indentLevel, getStackTraceEntries())

        conf.localReport(report)
      elif isDebug and not conf.isCompilerTraceDebug():
        # meaning we just analysed an `{.undef(nimCompilerDebug).}`
        # it started of as true, now in the `defer` it's false
        discard conf.debugUtilsStack.pop()
        conf.localReport(DebugReport(kind: rdbgTraceEnd))
      elif isDebug:
        discard conf.debugUtilsStack.pop()
        leaveMsg(indentLevel)
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

  StepParams(
    c: c,
    kind: kind,
    indentLevel: indentLevel,
    action: action
  )

const hasStacktrace = compileOption"stacktrace"

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
      it.steppedFrom = calledFromInfo()

    block:
      body

    handleReport(p.c, wrap(p.info, DebugReport(
      kind: rdbgTraceStep,
      semstep: it
    )), p.info)

const instDepth = -5
template traceEnterIt*(
    params: StepParams,
    body: untyped,
    templateDepth: int = instDepth
  ): untyped =
  ## Convenience wrapper around the `traceStepImpl`. If called from user
  ## code `templateDepth` parameter must be specified as well - it controls
  ## depth of the template instantiation location that is need to be
  ## accounted for. With current implementation of the debugutils this
  ## value is set to `-5` (`instDepth` default), for your code it might be
  ## different (`-2` when called directly and `-1` for each wrapper template
  ## level).
  var tmp = params
  tmp.info = instLoc(templateDepth)
  traceStepImpl(params, semstepEnter, body)

template traceLeaveIt*(
    params: StepParams,
    body: untyped,
    templateDepth: int = instDepth
  ): untyped =
  ## Convenience wrapper for `traaceStepImpl` - for mode details see the
  ## `traceEnterIt` and `traceStepImpl` documentation.
  var tmp = params
  tmp.info = instLoc(instDepth)
  traceStepImpl(tmp, semstepLeave, body)


template addInNimDebugUtils*(c: ConfigRef; action: string; n, r: PNode;
                            flags: TExprFlags) =
  ## add tracing to procs that are primarily `PNode -> PNode`, with expr flags
  ## and can determine the type
  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepNodeFlagsToNode, indentLevel, action
      )):
        it.node = n
        it.flags = flags

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepNodeFlagsToNode, indentLevel, action
      )):
        it.node = r
        it.flags = flags

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string; n, r: PNode) =
  ## add tracing to procs that are primarily `PNode -> PNode`, and can
  ## determine the type

  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepNodeToNode, indentLevel, action
      )):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepNodeToNode, indentLevel, action
      )):
        it.node = r

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtilsError*(c: ConfigRef; n, e: PNode) =
  ## add tracing error generation `PNode -> PNode`

  when defined(nimDebugUtils):
    const action = "newError"
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepWrongNode, indentLevel, action
      )):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepError, indentLevel, action
      )):
        it.node = e

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string; n: PNode;
                            prev, r: PType) =
  ## add tracing to procs that are primarily `PNode, PType|nil -> PType`,
  ## determining a type node, with a possible previous type.

  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepNodeTypeToNode, indentLevel, action
      )):
        it.node = n
        it.typ = prev

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepNodeTypeToNode, indentLevel, action
      )):
        it.node = n
        it.typ = r

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(
    c: ConfigRef; action: string; n: PNode; resSym: PSym) =
  ## add tracing to procs that are primarily `PNode -> PSym`,

  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepNodeToSym, indentLevel, action
      )):
        it.node = n

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepNodeToSym, indentLevel, action
      )):
        it.sym = resSym

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(
    c: ConfigRef; action: string; n: PNode; resSym: PSym) =
  ## add tracing to procs that are primarily `PNode -> PSym`,

  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      handleReport(c, wrap(instLoc(instDepth), DebugReport(
        kind: rdbgTraceStep,
        semstep: DebugSemStep(
          direction: semstepEnter,
          level: indentLevel,
          name: action,
          steppedFrom: calledFromInfo(),
          node: n,
          kind: stepNodeToSym))), instLoc(instDepth))

    template leaveMsg(indentLevel: int) =
      handleReport(c, wrap(instLoc(instDepth), DebugReport(
        kind: rdbgTraceStep,
        semstep: DebugSemStep(
          direction: semstepLeave,
          level: indentLevel,
          name: action,
          steppedFrom: calledFromInfo(),
          sym: resSym,
          kind: stepNodeToSym))), instLoc(instDepth))

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string; x, y, r: PType) =
  ## add tracing to procs that are primarily `PType, PType -> PType`, looking
  ## for a common type
  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepTypeTypeToType, indentLevel, action
      )):
        it.typ = x
        it.typ1 = y

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepTypeTypeToType, indentLevel, action
      )):
        it.typ = r

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)

template addInNimDebugUtils*(c: ConfigRef; action: string) =
  ## add tracing to procs that are primarily `PType, PType -> PType`, looking
  ## for a common type
  when defined(nimDebugUtils):
    template enterMsg(indentLevel: int) =
      traceEnterIt(stepParams(
        c, stepTrack, indentLevel, action
      )):
        discard

    template leaveMsg(indentLevel: int) =
      traceLeaveIt(stepParams(
        c, stepTrack, indentLevel, action
      )):
        discard

    addInNimDebugUtilsAux(c, action, enterMsg, leaveMsg)
