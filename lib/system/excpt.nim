#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Exception handling code. Carefully coded so that tiny programs which do not
# use the heap (and nor exceptions) do not include the GC or memory allocator.

import std/private/miscdollars
import stacktraces

const noStacktraceAvailable = "No stack traceback available\n"

var
  errorMessageWriter*: (proc(msg: string) {.tags: [WriteIOEffect], benign,
                                            nimcall.})
    ## Function that will be called
    ## instead of `stdmsg.write` when printing stacktrace.
    ## Unstable API.

when defined(windows):
  proc GetLastError(): int32 {.header: "<windows.h>", nodecl.}
  const ERROR_BAD_EXE_FORMAT = 193

when not defined(windows) or not defined(guiapp):
  proc writeToStdErr(msg: cstring) = rawWrite(cstderr, msg)
  proc writeToStdErr(msg: cstring, length: int) =
    rawWriteString(cstderr, msg, length)
else:
  proc MessageBoxA(hWnd: pointer, lpText, lpCaption: cstring, uType: int): int32 {.
    header: "<windows.h>", nodecl.}
  proc writeToStdErr(msg: cstring) =
    discard MessageBoxA(nil, msg, nil, 0)
  proc writeToStdErr(msg: cstring, length: int) =
    discard MessageBoxA(nil, msg, nil, 0)

proc writeToStdErr(msg: string) {.inline.} =
  # fix bug #13115: handles correctly '\0' unlike default implicit conversion to cstring
  writeToStdErr(msg.cstring, msg.len)

proc showErrorMessage(data: cstring, length: int) {.gcsafe, raises: [].} =
  var toWrite = true
  if errorMessageWriter != nil:
    try:
      errorMessageWriter($data)
      toWrite = false
    except:
      discard
  if toWrite:
    writeToStdErr(data, length)

proc showErrorMessage2(data: string) {.inline.} =
  showErrorMessage(data.cstring, data.len)

proc chckIndx(i, a, b: int): int {.inline, compilerproc, benign.}
proc chckRange(i, a, b: int): int {.inline, compilerproc, benign.}
proc chckRangeF(x, a, b: float): float {.inline, compilerproc, benign.}
proc chckNil(p: pointer) {.noinline, compilerproc, benign.}

type
  GcFrame = ptr GcFrameHeader
  GcFrameHeader {.compilerproc.} = object
    len: int
    prev: ptr GcFrameHeader

  ExceptionFrame {.compilerproc.} = object
    ## Represents an exception handler (i.e., an ``except`` section).
    prev: ptr ExceptionFrame
      ## previous frame, or nil
    exc: ref Exception
      ## always non-nil

when NimStackTraceMsgs:
  var frameMsgBuf* {.threadvar.}: string
var
  framePtr {.threadvar.}: PFrame
  currException {.threadvar.}: ref Exception
  gcFramePtr {.threadvar.}: GcFrame

  handlers {.threadvar.}: ptr ExceptionFrame
    ## linked list of all active handlers in the current thread
  activeException {.threadvar.}: ref Exception
    ## stack of in-flight exceptions. An exception stops being in-flight once
    ## an ``except`` section "catches" it.

type
  FrameState = tuple[gcFramePtr: GcFrame, framePtr: PFrame,
                     currException: ref Exception]

proc getFrameState*(): FrameState {.compilerRtl, inl.} =
  return (gcFramePtr, framePtr, currException)

proc setFrameState*(state: FrameState) {.compilerRtl, inl.} =
  gcFramePtr = state.gcFramePtr
  framePtr = state.framePtr
  currException = state.currException

proc getFrame*(): PFrame {.compilerRtl, inl.} = framePtr

proc popFrame {.compilerRtl, inl.} =
  # note: this procedure is potentially called while unwinding is happening,
  # and thus must not, under any circumstances, perfom any action that might
  # test or alter the error flag
  framePtr = framePtr.prev

when false:
  proc popFrameOfAddr(s: PFrame) {.compilerRtl.} =
    var it = framePtr
    if it == s:
      framePtr = framePtr.prev
    else:
      while it != nil:
        if it == s:
          framePtr = it.prev
          break
        it = it.prev

proc setFrame*(s: PFrame) {.compilerRtl, inl.} =
  framePtr = s

proc getGcFrame*(): GcFrame {.compilerRtl, inl.} = gcFramePtr
proc popGcFrame*() {.compilerRtl, inl.} = gcFramePtr = gcFramePtr.prev
proc setGcFrame*(s: GcFrame) {.compilerRtl, inl.} = gcFramePtr = s
proc pushGcFrame*(s: GcFrame) {.compilerRtl, inl.} =
  s.prev = gcFramePtr
  zeroMem(cast[pointer](cast[int](s)+%sizeof(GcFrameHeader)), s.len*sizeof(pointer))
  gcFramePtr = s

proc pushCurrentException(e: sink(ref Exception)) {.compilerRtl, inl.} =
  # XXX: legacy runtime procedure only used by the csources compiler
  e.up = currException
  currException = e
  #showErrorMessage2 "A"

proc popCurrentException {.compilerRtl, inl.} =
  # XXX: legacy runtime procedure only used by the csources compiler
  currException = currException.up
  #showErrorMessage2 "B"

proc popCurrentExceptionEx(id: uint) {.compilerRtl.} =
  discard "only for bootstrapping compatbility"

proc closureIterSetupExc(e: ref Exception) {.compilerproc, inline.} =
  currException = e

# some platforms have native support for stack traces:
const
  nativeStackTraceSupported = (defined(macosx) or defined(linux)) and
                              not NimStackTrace
  hasSomeStackTrace = NimStackTrace or defined(nimStackTraceOverride) or
    (defined(nativeStackTrace) and nativeStackTraceSupported)


when defined(nativeStacktrace) and nativeStackTraceSupported:
  type
    TDl_info {.importc: "Dl_info", header: "<dlfcn.h>",
               final, pure.} = object
      dli_fname: cstring
      dli_fbase: pointer
      dli_sname: cstring
      dli_saddr: pointer

  proc backtrace(symbols: ptr pointer, size: int): int {.
    importc: "backtrace", header: "<execinfo.h>".}
  proc dladdr(addr1: pointer, info: ptr TDl_info): int {.
    importc: "dladdr", header: "<dlfcn.h>".}

  when not hasThreadSupport:
    var
      tempAddresses: array[maxStackTraceLines, pointer] # should not be alloc'd on stack
      tempDlInfo: TDl_info

  proc auxWriteStackTraceWithBacktrace(s: var string) =
    when hasThreadSupport:
      var
        tempAddresses: array[maxStackTraceLines, pointer] # but better than a threadvar
        tempDlInfo: TDl_info
    # This is allowed to be expensive since it only happens during crashes
    # (but this way you don't need manual stack tracing)
    var size = backtrace(cast[ptr pointer](addr(tempAddresses)),
                         len(tempAddresses))
    var enabled = false
    for i in 0..size-1:
      var dlresult = dladdr(tempAddresses[i], addr(tempDlInfo))
      if enabled:
        if dlresult != 0:
          var oldLen = s.len
          add(s, tempDlInfo.dli_fname)
          if tempDlInfo.dli_sname != nil:
            for k in 1..max(1, 25-(s.len-oldLen)): add(s, ' ')
            add(s, tempDlInfo.dli_sname)
        else:
          add(s, '?')
        add(s, "\n")
      else:
        if dlresult != 0 and tempDlInfo.dli_sname != nil and
            c_strcmp(tempDlInfo.dli_sname, "signalHandler") == 0'i32:
          # Once we're past signalHandler, we're at what the user is
          # interested in
          enabled = true

when hasSomeStackTrace and not hasThreadSupport:
  var
    tempFrames: array[maxStackTraceLines, PFrame] # should not be alloc'd on stack

template reraisedFrom(z): untyped =
  StackTraceEntry(procname: nil, line: z, filename: nil)

proc auxWriteStackTrace(f: PFrame; s: var seq[StackTraceEntry]) =
  var
    it = f
    i = 0
  while it != nil:
    inc(i)
    it = it.prev
  var last = i-1
  when true: # not defined(gcDestructors):
    if s.len == 0:
      s = newSeq[StackTraceEntry](i)
    else:
      last = s.len + i - 1
      s.setLen(last+1)
  it = f
  while it != nil:
    s[last] = StackTraceEntry(procname: it.procname,
                              line: it.line,
                              filename: it.filename)
    when NimStackTraceMsgs:
      let first = if it.prev == nil: 0 else: it.prev.frameMsgLen
      if it.frameMsgLen > first:
        s[last].frameMsg.setLen(it.frameMsgLen - first)
        # somehow string slicing not available here
        for i in first .. it.frameMsgLen-1:
          s[last].frameMsg[i-first] = frameMsgBuf[i]
    it = it.prev
    dec last

template addFrameEntry(s: var string, f: StackTraceEntry|PFrame) =
  var oldLen = s.len
  s.toLocation(f.filename, f.line, 0)
  for k in 1..max(1, 25-(s.len-oldLen)): add(s, ' ')
  add(s, f.procname)
  when NimStackTraceMsgs:
    when typeof(f) is StackTraceEntry:
      add(s, f.frameMsg)
    else:
      var first = if f.prev == nil: 0 else: f.prev.frameMsgLen
      for i in first..<f.frameMsgLen: add(s, frameMsgBuf[i])
  add(s, "\n")

proc `$`(stackTraceEntries: seq[StackTraceEntry]): string =
  when defined(nimStackTraceOverride):
    let s = addDebuggingInfo(stackTraceEntries)
  else:
    let s = stackTraceEntries

  result = newStringOfCap(2000)
  for i in 0 .. s.len-1:
    if s[i].line == reraisedFromBegin: result.add "[[reraised from:\n"
    elif s[i].line == reraisedFromEnd: result.add "]]\n"
    else: addFrameEntry(result, s[i])

when hasSomeStackTrace:

  proc auxWriteStackTrace(f: PFrame, s: var string) =
    when hasThreadSupport:
      var
        tempFrames: array[maxStackTraceLines, PFrame] # but better than a threadvar
    const
      firstCalls = 32
    var
      it = f
      i = 0
      total = 0
    # setup long head:
    while it != nil and i <= high(tempFrames)-firstCalls:
      tempFrames[i] = it
      inc(i)
      inc(total)
      it = it.prev
    # go up the stack to count 'total':
    var b = it
    while it != nil:
      inc(total)
      it = it.prev
    var skipped = 0
    if total > len(tempFrames):
      # skip N
      skipped = total-i-firstCalls+1
      for j in 1..skipped:
        if b != nil: b = b.prev
      # create '...' entry:
      tempFrames[i] = nil
      inc(i)
    # setup short tail:
    while b != nil and i <= high(tempFrames):
      tempFrames[i] = b
      inc(i)
      b = b.prev
    for j in countdown(i-1, 0):
      if tempFrames[j] == nil:
        add(s, "(")
        add(s, $skipped)
        add(s, " calls omitted) ...\n")
      else:
        addFrameEntry(s, tempFrames[j])

  proc stackTraceAvailable*(): bool

  proc rawWriteStackTrace(s: var string) =
    when defined(nimStackTraceOverride):
      add(s, "Traceback (most recent call last, using override)\n")
      auxWriteStackTraceWithOverride(s)
    elif NimStackTrace:
      if framePtr == nil:
        add(s, noStacktraceAvailable)
      else:
        add(s, "Traceback (most recent call last)\n")
        auxWriteStackTrace(framePtr, s)
    elif defined(nativeStackTrace) and nativeStackTraceSupported:
      add(s, "Traceback from system (most recent call last)\n")
      auxWriteStackTraceWithBacktrace(s)
    else:
      add(s, noStacktraceAvailable)

  proc rawWriteStackTrace(s: var seq[StackTraceEntry]) =
    when defined(nimStackTraceOverride):
      auxWriteStackTraceWithOverride(s)
    elif NimStackTrace:
      auxWriteStackTrace(framePtr, s)
    else:
      s = @[]

  proc stackTraceAvailable(): bool =
    when defined(nimStackTraceOverride):
      result = true
    elif NimStackTrace:
      if framePtr == nil:
        result = false
      else:
        result = true
    elif defined(nativeStackTrace) and nativeStackTraceSupported:
      result = true
    else:
      result = false
else:
  proc stackTraceAvailable*(): bool = result = false

var onUnhandledException*: (proc (errorMsg: string) {.
  nimcall, gcsafe.}) ## Set this error \
  ## handler to override the existing behaviour on an unhandled exception.
  ##
  ## The default is to write a stacktrace to `stderr` and then call `quit(1)`.
  ## Unstable API.

proc reportUnhandledErrorAux(e: ref Exception) {.nodestroy.} =
  when hasSomeStackTrace:
    var buf = newStringOfCap(2000)
    if e.trace.len == 0:
      rawWriteStackTrace(buf)
    else:
      var trace = $e.trace
      add(buf, trace)
      `=destroy`(trace)
    add(buf, "Error: unhandled exception: ")
    add(buf, e.msg)
    add(buf, " [")
    add(buf, $e.name)
    add(buf, "]\n")

    if onUnhandledException != nil:
      onUnhandledException(buf)
    else:
      showErrorMessage2(buf)
    `=destroy`(buf)
  else:
    # ugly, but avoids heap allocations :-)
    template xadd(buf, s, slen) =
      if L + slen < high(buf):
        copyMem(addr(buf[L]), (when s is cstring: s else: cstring(s)), slen)
        inc L, slen
    template add(buf, s) =
      xadd(buf, s, s.len)
    var buf: array[0..2000, char]
    var L = 0
    if e.trace.len != 0:
      var trace = $e.trace
      add(buf, trace)
      `=destroy`(trace)
    add(buf, "Error: unhandled exception: ")
    add(buf, e.msg)
    add(buf, " [")
    xadd(buf, e.name, e.name.len)
    add(buf, "]\n")
    if onUnhandledException != nil:
      onUnhandledException($buf.addr)
    else:
      showErrorMessage(buf.addr, L)

proc reportUnhandledError(e: ref Exception) {.nodestroy.} =
  if unhandledExceptionHook != nil:
    unhandledExceptionHook(e)
  when hostOS != "any":
    reportUnhandledErrorAux(e)

when true:
  var nimInErrorMode {.threadvar.}: bool

  proc nimErrorFlag(): ptr bool {.compilerRtl, inl.} =
    result = addr(nimInErrorMode)

  proc nimTestErrorFlag() {.compilerRtl.} =
    ## This proc must be called before `currException` is destroyed.
    ## It also must be called at the end of every thread to ensure no
    ## error is swallowed.
    # clear the error flag before doing anything else. Otherwise the first
    # non-magic call would cause this procedure to exit
    var wasInErrorMode = false
    swap(nimInErrorMode, wasInErrorMode)

    if wasInErrorMode and currException != nil:
      reportUnhandledError(currException)
      currException = nil
      quit(1)

proc nimUnhandledException() {.compilerproc, noreturn.} =
  ## Called from generated code when propgation of an exception crosses a
  ## routine boundary it shouldn't.
  reportUnhandledError(currException)
  quit(1)

proc pushActiveException(e: sink(ref Exception)) =
  e.up = activeException
  activeException = e
  # FIXME: don't set the current exception here; it's only done this way for
  #        backwards compatibility
  currException = e # set the current exception already

proc raiseExceptionAux(e: sink(ref Exception)) {.nodestroy.} =
  when defined(nimPanics):
    # XXX: the compiler should reject raise being used with defects. User-code
    #      should raise defects via ``sysFatal``
    if e of Defect:
      reportUnhandledError(e)
      quit(1)

  if localRaiseHook != nil:
    if not localRaiseHook(e): return
  if globalRaiseHook != nil:
    if not globalRaiseHook(e): return

  when defined(nimskullNewExceptionRt):
    pushActiveException(e)
  else:
    pushCurrentException(e)
  inc nimInErrorMode

proc prepareException(e: ref Exception, ename: cstring) {.compilerRtl.} =
  if e.name.isNil: e.name = ename
  when hasSomeStackTrace:
    when defined(nimStackTraceOverride):
      if e.trace.len == 0:
        rawWriteStackTrace(e.trace)
      else:
        e.trace.add reraisedFrom(reraisedFromBegin)
        auxWriteStackTraceWithOverride(e.trace)
        e.trace.add reraisedFrom(reraisedFromEnd)
    elif NimStackTrace:
      if e.trace.len == 0:
        rawWriteStackTrace(e.trace)
      elif framePtr != nil:
        e.trace.add reraisedFrom(reraisedFromBegin)
        auxWriteStackTrace(framePtr, e.trace)
        e.trace.add reraisedFrom(reraisedFromEnd)

proc raiseException2(e: sink(ref Exception), procname, filename: cstring,
                     line: int) {.compilerRtl.} =
  ## Part of the C backend's runtime. Pushes `e` to the stack, invokes the
  ## local and global callback (if any), and enters error mode.
  when not hasSomeStackTrace:
    if procname != nil and filename != nil:
      e.trace.add StackTraceEntry(procname: procname, filename: filename,
                                  line: line)
  raiseExceptionAux(e)

proc raiseExceptionEx(e: sink(ref Exception), ename, procname, filename: cstring,
                      line: int) {.compilerRtl, nodestroy.} =
  # XXX: legacy routine used by the csources' runtime.
  prepareException(e, ename)
  raiseException2(e, procname, filename, line)

proc raiseException(e: sink(ref Exception), ename: cstring) {.compilerRtl.} =
  # XXX: legacy routine used by the csources' runtime.
  raiseExceptionEx(e, ename, nil, nil, 0)

proc reraiseException() {.compilerRtl.} =
  when defined(nimskullNewExceptionRt):
    # the compiler makes sure that a re-raise only take place within an
    # exception handler
    pushActiveException(move handlers.exc)
    inc nimInErrorMode
  else:
    if currException == nil:
      sysFatal(ReraiseDefect, "no exception to reraise")
    else:
      inc nimInErrorMode

proc threadTrouble() =
  # also forward declared, it is 'raises: []' hence the try-except.
  try:
    if currException != nil: reportUnhandledError(currException)
  except:
    discard
  quit 1

proc writeStackTrace() =
  when hasSomeStackTrace:
    var s = ""
    rawWriteStackTrace(s)
  else:
    let s = noStacktraceAvailable
  cast[proc (s: string) {.noSideEffect, tags: [], nimcall, raises: [].}](showErrorMessage2)(s)

proc getStackTrace(): string =
  when hasSomeStackTrace:
    result = ""
    rawWriteStackTrace(result)
  else:
    result = noStacktraceAvailable

proc getStackTrace(e: ref Exception): string =
  if not isNil(e):
    result = $e.trace
  else:
    result = ""

proc getStackTraceEntries*(e: ref Exception): lent seq[StackTraceEntry] =
  ## Returns the attached stack trace to the exception `e` as
  ## a `seq`. This is not yet available for the JS backend.
  e.trace

proc getStackTraceEntries*(): seq[StackTraceEntry] =
  ## Returns the stack trace entries for the current stack trace.
  ## This is not yet available for the JS backend.
  when hasSomeStackTrace:
    rawWriteStackTrace(result)

const nimCallDepthLimit {.intdefine.} = 2000

proc callDepthLimitReached() {.noinline.} =
  writeStackTrace()
  let msg = "Error: call depth limit reached in a debug build (" &
      $nimCallDepthLimit & " function calls). You can change it with " &
      "-d:nimCallDepthLimit=<int> but really try to avoid deep " &
      "recursions instead.\n"
  showErrorMessage2(msg)
  quit(1)

proc nimFrame(s: PFrame) {.compilerRtl, inl, raises: [].} =
  if framePtr == nil:
    s.calldepth = 0
    when NimStackTraceMsgs: s.frameMsgLen = 0
  else:
    s.calldepth = framePtr.calldepth+1
    when NimStackTraceMsgs: s.frameMsgLen = framePtr.frameMsgLen
  s.prev = framePtr
  framePtr = s
  if s.calldepth == nimCallDepthLimit: callDepthLimitReached()

# v2 exception handling runtime
# -----------------------------

{.push stacktrace: off, checks: off.}

proc nimCatchException(frame: ptr ExceptionFrame) {.compilerproc.} =
  ## Pops the top-most in-flight exception from the stack, stores it in
  ## `frame`, and sets it as the current exception. `frame` is pushed to the
  ## handler stack.
  # zero-initialize the location first
  nimZeroMem(frame, sizeof(ExceptionFrame))
  frame.prev = handlers
  frame.exc = activeException
  # push to handler stack:
  handlers = frame

  currException = activeException
  # "pop" the exception from the active stack. Moving from the up pointer
  # makes sure the caught exception is properly disconnected:
  activeException = move activeException.up

proc restoreCurrentEx() =
  # FIXME: don't consider active exceptions; it's only done this way for
  #        backwards compatibility
  if handlers.isNil or handlers.exc.isNil:
    currException = activeException
  elif handlers.exc.up == activeException:
    # the active handler is more recent than the most-recent active exception
    currException = handlers.exc
  else:
    # the active exception was raised after the most-recent handler was entered
    currException = activeException

proc nimLeaveExcept() {.compilerproc, inline.} =
  ## Called when an exception handler is exited. Pops the top-most frame from
  ## the handler stack. Supports being called during unwinding.
  var wasInErrorMode = nimInErrorMode
  nimInErrorMode = false
  handlers.exc = nil # cleanup the ref
  handlers = handlers.prev
  restoreCurrentEx()
  nimInErrorMode = wasInErrorMode

proc nimAbortException() {.compilerproc.} =
  ## Abort (i.e., discard) an in-flight exception. Must only be called if an
  ## exception is actually in-flight.
  var wasInErrorMode = nimInErrorMode
  # disable error mode right away
  nimInErrorMode = false

  if wasInErrorMode:
    # if an exception is aborted by raising another exception, don't pop the
    # active exception; drop its parent
    if activeException.up != nil:
      activeException.up = activeException.up.up
  else:
    activeException = activeException.up

  restoreCurrentEx()
  nimInErrorMode = wasInErrorMode

{.pop.}

# -----

when not defined(noSignalHandler) and not defined(useNimRtl):
  type Sighandler = proc (a: cint) {.noconv, benign.}
    # xxx factor with ansi_c.CSighandlerT, posix.Sighandler

  proc signalHandler(sign: cint) {.exportc: "signalHandler", noconv.} =
    template processSignal(s, action: untyped) {.dirty.} =
      if s == SIGINT: action("SIGINT: Interrupted by Ctrl-C.\n")
      elif s == SIGSEGV:
        action("SIGSEGV: Illegal storage access. (Attempt to read from nil?)\n")
      elif s == SIGABRT:
        action("SIGABRT: Abnormal termination.\n")
      elif s == SIGFPE: action("SIGFPE: Arithmetic error.\n")
      elif s == SIGILL: action("SIGILL: Illegal operation.\n")
      elif (when declared(SIGBUS): s == SIGBUS else: false):
        action("SIGBUS: Illegal storage access. (Attempt to read from nil?)\n")
      else:
        block platformSpecificSignal:
          when declared(SIGPIPE):
            if s == SIGPIPE:
              action("SIGPIPE: Pipe closed.\n")
              break platformSpecificSignal
          action("unknown signal\n")

    # print stack trace and quit
    when defined(memtracker):
      logPendingOps()
    when hasSomeStackTrace:
      var buf = newStringOfCap(2000)
      rawWriteStackTrace(buf)
      processSignal(sign, buf.add) # nice hu? currying a la Nim :-)
      showErrorMessage2(buf)
    else:
      var msg: cstring
      template asgn(y) =
        msg = y
      processSignal(sign, asgn)
      # xxx use string for msg instead of cstring, and here use showErrorMessage2(msg)
      # unless there's a good reason to use cstring in signal handler to avoid
      # using gc?
      showErrorMessage(msg, msg.len)

    when defined(posix):
      # reset the signal handler to OS default
      c_signal(sign, SIG_DFL)

      # re-raise the signal, which will arrive once this handler exit.
      # this lets the OS perform actions like core dumping and will
      # also return the correct exit code to the shell.
      discard c_raise(sign)
    else:
      quit(1)

  var SIG_IGN {.importc: "SIG_IGN", header: "<signal.h>".}: Sighandler

  proc registerSignalHandler() =
    # xxx `signal` is deprecated and has many caveats, we should use `sigaction` instead, e.g.
    # https://stackoverflow.com/questions/231912/what-is-the-difference-between-sigaction-and-signal
    c_signal(SIGINT, signalHandler)
    c_signal(SIGSEGV, signalHandler)
    c_signal(SIGABRT, signalHandler)
    c_signal(SIGFPE, signalHandler)
    c_signal(SIGILL, signalHandler)
    when declared(SIGBUS):
      c_signal(SIGBUS, signalHandler)
    when declared(SIGPIPE):
      when defined(nimLegacySigpipeHandler):
        c_signal(SIGPIPE, signalHandler)
      else:
        c_signal(SIGPIPE, SIG_IGN)

  registerSignalHandler() # call it in initialization section

proc setControlCHook(hook: proc () {.noconv.}) =
  # ugly cast, but should work on all architectures:
  when declared(Sighandler):
    c_signal(SIGINT, cast[Sighandler](hook))

when not defined(noSignalHandler) and not defined(useNimRtl):
  proc unsetControlCHook() =
    # proc to unset a hook set by setControlCHook
    c_signal(SIGINT, signalHandler)
