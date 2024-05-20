#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

include system/indexerrors
import std/private/miscdollars

proc log*(s: cstring) {.importc: "console.log", varargs, nodecl.}

type
  PCallFrame = ptr CallFrame
  CallFrame {.importc, nodecl, final.} = object
    prev: PCallFrame
    procname: cstring
    line: int # current line number
    filename: cstring

  PJSError = ref object
    columnNumber {.importc.}: int
    fileName {.importc.}: cstring
    lineNumber {.importc.}: int
    message {.importc.}: cstring
    stack {.importc.}: cstring

  JSRef = ref RootObj # Fake type.

var
  framePtr {.importc, nodecl, volatile.}: PCallFrame
  lastJSError {.importc, nodecl, volatile.}: PJSError = nil

{.push stacktrace: off, profiler:off.}
proc nimBoolToStr(x: bool): string {.compilerproc.} =
  if x: result = "true"
  else: result = "false"

proc nimCharToStr(x: char): string {.compilerproc.} =
  result = newString(1)
  result[0] = x

proc isNimException(): bool {.asmNoStackFrame.} =
  asm "return `lastJSError` && `lastJSError`.m_type;"

proc getCurrentException*(): ref Exception {.compilerRtl, benign.} =
  if isNimException(): result = cast[ref Exception](lastJSError)

proc getCurrentExceptionMsg*(): string =
  if lastJSError != nil:
    if isNimException():
      return cast[Exception](lastJSError).msg
    else:
      var msg: cstring
      {.emit: """
      if (`lastJSError`.message !== undefined) {
        `msg` = `lastJSError`.message;
      }
      """.}
      if not msg.isNil:
        return $msg
  return ""

proc setCurrentException*(exc: ref Exception) =
  lastJSError = cast[PJSError](exc)

proc closureIterSetupExc(e: ref Exception) {.compilerproc, inline.} =
  ## Used by the closure transformation pass for preparing for exception
  ## handling
  setCurrentException(e)

proc auxWriteStackTrace(f: PCallFrame): string =
  type
    TempFrame = tuple[procname: cstring, line: int, filename: cstring]
  var
    it = f
    i = 0
    total = 0
    tempFrames: array[0..63, TempFrame]
  while it != nil and i <= high(tempFrames):
    tempFrames[i].procname = it.procname
    tempFrames[i].line = it.line
    tempFrames[i].filename = it.filename
    inc(i)
    inc(total)
    it = it.prev
  while it != nil:
    inc(total)
    it = it.prev
  result = ""
  # if the buffer overflowed print '...':
  if total != i:
    add(result, "(")
    add(result, $(total-i))
    add(result, " calls omitted) ...\n")
  for j in countdown(i-1, 0):
    result.toLocation($tempFrames[j].filename, tempFrames[j].line, 0)
    add(result, " at ")
    add(result, tempFrames[j].procname)
    add(result, "\n")

proc rawWriteStackTrace(): string =
  if framePtr != nil:
    result = "Traceback (most recent call last)\n" & auxWriteStackTrace(framePtr)
  else:
    result = "No stack traceback available\n"

proc writeStackTrace() =
  var trace = rawWriteStackTrace()
  trace.setLen(trace.len - 1)
  echo trace

proc getStackTrace*(): string = rawWriteStackTrace()
proc getStackTrace*(e: ref Exception): string = e.trace

proc unhandledExceptionString(e: ref Exception): string =
  var buf = ""
  if e.msg.len != 0:
    add(buf, "Error: unhandled exception: ")
    add(buf, e.msg)
  else:
    add(buf, "Error: unhandled exception")
  add(buf, " [")
  add(buf, e.name)
  add(buf, "]\n")
  when NimStackTrace:
    add(buf, rawWriteStackTrace())
  result = buf

proc unhandledException(e: ref Exception) {.
    compilerproc, asmNoStackFrame.} =
  let cbuf = cstring(unhandledExceptionString(e))
  framePtr = nil
  {.emit: """
  if (typeof(Error) !== "undefined") {
    throw new Error(`cbuf`);
  }
  else {
    throw `cbuf`;
  }
  """.}

proc nimUnhandledException() {.compilerproc, asmNoStackFrame.} =
  # |NimSkull| exceptions are turned into JavaScript errors for the purpose
  # of better error messages
  when defined(nodejs):
    {.emit: """
      if (lastJSError.m_type !== undefined) {
        console.log(`toJSStr`(`unhandledExceptionString`(`lastJSError`)));
      } else {
        console.log('Error: unhandled exception: ', `lastJSError`)
      }
      process.exit(1);
    """.}
  else:
    # it's currently not possible to truly panic (abort excution) for non-
    # node.js JavaScript
    {.emit: """
      if (lastJSError.m_type !== undefined) {
        `unhandledException`(lastJSError);
      } else {
        throw lastJSError;
      }
    """.}

proc prepareException(e: ref Exception, ename: cstring) {.
    compilerproc, asmNoStackFrame.} =
  if e.name.isNil:
    e.name = ename
  when NimStackTrace:
    e.trace = rawWriteStackTrace()

proc raiseException(e: ref Exception) {.compilerproc, asmNoStackFrame.} =
  asm "throw `e`;"

proc reraiseException() {.compilerproc, asmNoStackFrame.} =
  if lastJSError == nil:
    raise newException(ReraiseDefect, "no exception to reraise")
  else:
    asm "throw lastJSError;"

proc raiseOverflow {.exportc: "raiseOverflow", noreturn, compilerproc.} =
  raise newException(OverflowDefect, "over- or underflow")

proc raiseDivByZero {.exportc: "raiseDivByZero", noreturn, compilerproc.} =
  raise newException(DivByZeroDefect, "division by zero")

proc raiseRangeError() {.compilerproc, noreturn.} =
  raise newException(RangeDefect, "value out of range")

proc raiseIndexError(i, a, b: int) {.compilerproc, noreturn.} =
  raise newException(IndexDefect, formatErrorIndexBound(int(i), int(a), int(b)))

proc raiseIndexError1() {.compilerproc, noreturn.} =
  raise newException(IndexDefect, "index out of bounds")

proc raiseFieldError2(f: string, discVal: string) {.compilerproc, noreturn.} =
  raise newException(FieldDefect, formatFieldDefect(f, discVal))

proc setConstr() {.varargs, asmNoStackFrame, compilerproc.} =
  asm """
    var result = {};
    for (var i = 0; i < arguments.length; ++i) {
      var x = arguments[i];
      if (typeof(x) == "object") {
        for (var j = x[0]; j <= x[1]; ++j) {
          result[j] = true;
        }
      } else {
        result[x] = true;
      }
    }
    return result;
  """

proc makeNimstrLit(c: cstring): string {.asmNoStackFrame, compilerproc.} =
  {.emit: """
  var result = [];
  for (var i = 0; i < `c`.length; ++i) {
    result[i] = `c`.charCodeAt(i);
  }
  return result;
  """.}

proc cstrToNimstr(c: cstring): string {.asmNoStackFrame, compilerproc.} =
  {.emit: """
  var ln = `c`.length;
  var result = new Array(ln);
  var r = 0;
  for (var i = 0; i < ln; ++i) {
    var ch = `c`.charCodeAt(i);

    if (ch < 128) {
      result[r] = ch;
    }
    else {
      if (ch < 2048) {
        result[r] = (ch >> 6) | 192;
      }
      else {
        if (ch < 55296 || ch >= 57344) {
          result[r] = (ch >> 12) | 224;
        }
        else {
            ++i;
            ch = 65536 + (((ch & 1023) << 10) | (`c`.charCodeAt(i) & 1023));
            result[r] = (ch >> 18) | 240;
            ++r;
            result[r] = ((ch >> 12) & 63) | 128;
        }
        ++r;
        result[r] = ((ch >> 6) & 63) | 128;
      }
      ++r;
      result[r] = (ch & 63) | 128;
    }
    ++r;
  }
  return result;
  """.}

proc toJSStr(s: string): cstring {.compilerproc.} =
  proc fromCharCode(c: char): cstring {.importc: "String.fromCharCode".}
  proc join(x: openArray[cstring]; d = cstring""): cstring {.
    importjs: "#.join(@)".}
  proc decodeURIComponent(x: cstring): cstring {.
    importc: "decodeURIComponent".}

  proc toHexString(c: char; d = 16): cstring {.importjs: "#.toString(@)".}

  proc log(x: cstring) {.importc: "console.log".}

  var res = newSeq[cstring](s.len)
  var i = 0
  var j = 0
  while i < s.len:
    var c = s[i]
    if c < '\128':
      res[j] = fromCharCode(c)
      inc i
    else:
      var helper = newSeq[cstring]()
      while true:
        let code = toHexString(c)
        if code.len == 1:
          helper.add cstring"%0"
        else:
          helper.add cstring"%"
        helper.add code
        inc i
        if i >= s.len or s[i] < '\128': break
        c = s[i]
      try:
        res[j] = decodeURIComponent join(helper)
      except:
        res[j] = join(helper)
    inc j
  setLen(res, j)
  result = join(res)

proc mnewString(len: int): string {.asmNoStackFrame, compilerproc.} =
  asm """
    return new Array(`len`);
  """

proc SetCard(a: int): int {.compilerproc, asmNoStackFrame.} =
  # argument type is a fake
  asm """
    var result = 0;
    for (var elem in `a`) { ++result; }
    return result;
  """

proc SetEq(a, b: int): bool {.compilerproc, asmNoStackFrame.} =
  asm """
    for (var elem in `a`) { if (!`b`[elem]) return false; }
    for (var elem in `b`) { if (!`a`[elem]) return false; }
    return true;
  """

proc SetLe(a, b: int): bool {.compilerproc, asmNoStackFrame.} =
  asm """
    for (var elem in `a`) { if (!`b`[elem]) return false; }
    return true;
  """

proc SetLt(a, b: int): bool {.compilerproc.} =
  result = SetLe(a, b) and not SetEq(a, b)

proc SetMul(a, b: int): int {.compilerproc, asmNoStackFrame.} =
  asm """
    var result = {};
    for (var elem in `a`) {
      if (`b`[elem]) { result[elem] = true; }
    }
    return result;
  """

proc SetPlus(a, b: int): int {.compilerproc, asmNoStackFrame.} =
  asm """
    var result = {};
    for (var elem in `a`) { result[elem] = true; }
    for (var elem in `b`) { result[elem] = true; }
    return result;
  """

proc SetMinus(a, b: int): int {.compilerproc, asmNoStackFrame.} =
  asm """
    var result = {};
    for (var elem in `a`) {
      if (!`b`[elem]) { result[elem] = true; }
    }
    return result;
  """

proc cmpStrings(a, b: string): int {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`a` == `b`) return 0;
    if (!`a`) return -1;
    if (!`b`) return 1;
    for (var i = 0; i < `a`.length && i < `b`.length; i++) {
      var result = `a`[i] - `b`[i];
      if (result != 0) return result;
    }
    return `a`.length - `b`.length;
  """

proc cmp(x, y: string): int =
  when nimvm:
    if x == y: result = 0
    elif x < y: result = -1
    else: result = 1
  else:
    result = cmpStrings(x, y)

proc eqStrings(a, b: string): bool {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`a` == `b`) return true;
    if (`a` === null && `b`.length == 0) return true;
    if (`b` === null && `a`.length == 0) return true;
    if ((!`a`) || (!`b`)) return false;
    var alen = `a`.length;
    if (alen != `b`.length) return false;
    for (var i = 0; i < alen; ++i)
      if (`a`[i] != `b`[i]) return false;
    return true;
  """

when defined(kwin):
  proc rawEcho {.compilerproc, asmNoStackFrame.} =
    asm """
      var buf = "";
      for (var i = 0; i < arguments.length; ++i) {
        buf += `toJSStr`(arguments[i]);
      }
      print(buf);
    """

elif not defined(nimOldEcho):
  proc ewriteln(x: cstring) = log(x)

  proc rawEcho {.compilerproc, asmNoStackFrame.} =
    asm """
      var buf = "";
      for (var i = 0; i < arguments.length; ++i) {
        buf += `toJSStr`(arguments[i]);
      }
      console.log(buf);
    """

else:
  proc ewriteln(x: cstring) =
    var node : JSRef
    {.emit: "`node` = document.getElementsByTagName('body')[0];".}
    if node.isNil:
      raise newException(ValueError, "<body> element does not exist yet!")
    {.emit: """
    `node`.appendChild(document.createTextNode(`x`));
    `node`.appendChild(document.createElement("br"));
    """.}

  proc rawEcho {.compilerproc.} =
    var node : JSRef
    {.emit: "`node` = document.getElementsByTagName('body')[0];".}
    if node.isNil:
      raise newException(IOError, "<body> element does not exist yet!")
    {.emit: """
    for (var i = 0; i < arguments.length; ++i) {
      var x = `toJSStr`(arguments[i]);
      `node`.appendChild(document.createTextNode(x));
    }
    `node`.appendChild(document.createElement("br"));
    """.}

# Arithmetic:
proc checkOverflowInt(a: int) {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`a` > 2147483647 || `a` < -2147483648) `raiseOverflow`();
  """

proc addInt(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    var result = `a` + `b`;
    `checkOverflowInt`(result);
    return result;
  """

proc subInt(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    var result = `a` - `b`;
    `checkOverflowInt`(result);
    return result;
  """

proc mulInt(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    var result = `a` * `b`;
    `checkOverflowInt`(result);
    return result;
  """

proc divInt(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`b` == 0) `raiseDivByZero`();
    if (`b` == -1 && `a` == 2147483647) `raiseOverflow`();
    return Math.trunc(`a` / `b`);
  """

proc modInt(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`b` == 0) `raiseDivByZero`();
    if (`b` == -1 && `a` == 2147483647) `raiseOverflow`();
    return Math.trunc(`a` % `b`);
  """

proc checkOverflowInt64(a: int64) {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`a` > 9223372036854775807 || `a` < -9223372036854775808) `raiseOverflow`();
  """

proc addInt64(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    var result = `a` + `b`;
    `checkOverflowInt64`(result);
    return result;
  """

proc subInt64(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    var result = `a` - `b`;
    `checkOverflowInt64`(result);
    return result;
  """

proc mulInt64(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    var result = `a` * `b`;
    `checkOverflowInt64`(result);
    return result;
  """

proc divInt64(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`b` == 0) `raiseDivByZero`();
    if (`b` == -1 && `a` == 9223372036854775807) `raiseOverflow`();
    return Math.trunc(`a` / `b`);
  """

proc modInt64(a, b: int): int {.asmNoStackFrame, compilerproc.} =
  asm """
    if (`b` == 0) `raiseDivByZero`();
    if (`b` == -1 && `a` == 9223372036854775807) `raiseOverflow`();
    return Math.trunc(`a` % `b`);
  """

proc negInt(a: int): int {.compilerproc.} =
  result = a*(-1)
  checkOverflowInt(result)

proc negInt64(a: int64): int64 {.compilerproc.} =
  result = a*(-1)
  checkOverflowInt64(result)

proc absInt(a: int): int {.compilerproc.} =
  result = if a < 0: a*(-1) else: a

proc absInt64(a: int64): int64 {.compilerproc.} =
  result = if a < 0: a*(-1) else: a

when not defined(nimNoZeroExtendMagic):
  proc ze*(a: int): int {.compilerproc.} =
    result = a

  proc ze64*(a: int64): int64 {.compilerproc.} =
    result = a

  proc toU8*(a: int): int8 {.asmNoStackFrame, compilerproc.} =
    asm """
      return `a`;
    """

  proc toU16*(a: int): int16 {.asmNoStackFrame, compilerproc.} =
    asm """
      return `a`;
    """

  proc toU32*(a: int64): int32 {.asmNoStackFrame, compilerproc.} =
    asm """
      return `a`;
    """

proc nimMin(a, b: int): int {.compilerproc.} = return if a <= b: a else: b
proc nimMax(a, b: int): int {.compilerproc.} = return if a >= b: a else: b

include "system/hti"

proc isFatPointer(ti: PNimType): bool =
  # This has to be consistent with the code generator!
  return ti.base.kind notin {tyObject,
    tyArray, tyArrayConstr, tyTuple,
    tyOpenArray, tySet, tyVar, tyRef, tyPtr}

proc nimCopy(dest, src: JSRef, ti: PNimType): JSRef {.compilerproc.}

proc nimCopyAux(dest, src: JSRef, n: ptr TNimNode) {.compilerproc.} =
  case n.kind
  of nkNone: sysAssert(false, "nimCopyAux")
  of nkSlot:
    asm """
      `dest`[`n`.offset] = nimCopy(`dest`[`n`.offset], `src`[`n`.offset], `n`.typ);
    """
  of nkList:
    asm """
    for (var i = 0; i < `n`.sons.length; i++) {
      nimCopyAux(`dest`, `src`, `n`.sons[i]);
    }
    """
  of nkCase:
    asm """
      `dest`[`n`.offset] = nimCopy(`dest`[`n`.offset], `src`[`n`.offset], `n`.typ);
      for (var i = 0; i < `n`.sons.length; ++i) {
        nimCopyAux(`dest`, `src`, `n`.sons[i][1]);
      }
    """

proc nimCopy(dest, src: JSRef, ti: PNimType): JSRef =
  case ti.kind
  of tyPtr, tyRef, tyVar, tyNil:
    if not isFatPointer(ti):
      result = src
    else:
      asm "`result` = [`src`[0], `src`[1]];"
  of tySet:
    asm """
      if (`dest` === null || `dest` === undefined) {
        `dest` = {};
      }
      else {
        for (var key in `dest`) { delete `dest`[key]; }
      }
      for (var key in `src`) { `dest`[key] = `src`[key]; }
      `result` = `dest`;
    """
  of tyTuple, tyObject:
    if ti.base != nil: result = nimCopy(dest, src, ti.base)
    elif ti.kind == tyObject:
      asm "`result` = (`dest` === null || `dest` === undefined) ? {m_type: `ti`} : `dest`;"
    else:
      asm "`result` = (`dest` === null || `dest` === undefined) ? {} : `dest`;"
    nimCopyAux(result, src, ti.node)
  of tyArrayConstr, tyArray:
    # In order to prevent a type change (TypedArray -> Array) and to have better copying performance,
    # arrays constructors are considered separately
    asm """
      if(ArrayBuffer.isView(`src`)) { 
        if(`dest` === null || `dest` === undefined || `dest`.length != `src`.length) {
          `dest` = new `src`.constructor(`src`);
        } else {
          `dest`.set(`src`, 0);
        }
        `result` = `dest`;
      } else {
        if (`src` === null) {
          `result` = null;
        }
        else {
          if (`dest` === null || `dest` === undefined || `dest`.length != `src`.length) {
            `dest` = new Array(`src`.length);
          }
          `result` = `dest`;
          for (var i = 0; i < `src`.length; ++i) {
            `result`[i] = nimCopy(`result`[i], `src`[i], `ti`.base);
          }
        }
      }
    """
  of tySequence, tyOpenArray:
    asm """
      if (`src` === null) {
        `result` = null;
      }
      else {
        if (`dest` === null || `dest` === undefined || `dest`.length != `src`.length) {
          `dest` = new Array(`src`.length);
        }
        `result` = `dest`;
        for (var i = 0; i < `src`.length; ++i) {
          `result`[i] = nimCopy(`result`[i], `src`[i], `ti`.base);
        }
      }
    """
  of tyString:
    asm """
      if (`src` !== null) {
        `result` = `src`.slice(0);
      }
    """
  else:
    result = src

proc genericReset(x: JSRef, ti: PNimType): JSRef {.compilerproc.} =
  asm "`result` = null;"
  case ti.kind
  of tyPtr, tyRef, tyVar, tyNil:
    if isFatPointer(ti):
      asm """
        `result` = [null, 0];
      """
  of tySet:
    asm """
      `result` = {};
    """
  of tyTuple, tyObject:
    if ti.kind == tyObject:
      asm "`result` = {m_type: `ti`};"
    else:
      asm "`result` = {};"
  of tySequence, tyOpenArray, tyString:
    asm """
      `result` = [];
    """
  of tyArrayConstr, tyArray:
    asm """
      `result` = new Array(`x`.length);
      for (var i = 0; i < `x`.length; ++i) {
        `result`[i] = genericReset(`x`[i], `ti`.base);
      }
    """
  else:
    discard

proc arrayConstr(len: int, value: JSRef, typ: PNimType): JSRef {.
                asmNoStackFrame, compilerproc.} =
  # types are fake
  asm """
    var result = new Array(`len`);
    for (var i = 0; i < `len`; ++i) result[i] = `nimCopy`(null, `value`, `typ`);
    return result;
  """

proc chckIndx(i, a, b: int): int {.compilerproc.} =
  if i >= a and i <= b: return i
  else: raiseIndexError(i, a, b)

proc chckBounds(lo, hi, a, b: int) {.compilerproc.} =
  if hi-lo != -1 and (hi-lo < -1 or lo < a or lo > b or hi > b or hi < a):
    raiseIndexError1()

proc chckRange(i, a, b: int): int {.compilerproc.} =
  if i >= a and i <= b: return i
  else: raiseRangeError()

proc chckObj(obj, subclass: PNimType) {.compilerproc.} =
  # checks if obj is of type subclass:
  var x = obj
  if x == subclass: return # optimized fast path
  while x != subclass:
    if x == nil:
      raise newException(ObjectConversionDefect, "invalid object conversion")
    x = x.base

proc isObj(obj, subclass: PNimType): bool {.compilerproc.} =
  # checks if obj is of type subclass:
  var x = obj
  if x == subclass: return true # optimized fast path
  while x != subclass:
    if x == nil: return false
    x = x.base
  return true

proc addChar(x: string, c: char) {.compilerproc, asmNoStackFrame.} =
  asm "`x`.push(`c`);"

{.pop.}

proc tenToThePowerOf(b: int): BiggestFloat =
  # xxx deadcode
  var b = b
  var a = 10.0
  result = 1.0
  while true:
    if (b and 1) == 1:
      result = result * a
    b = b shr 1
    if b == 0: break
    a = a * a

const
  IdentChars = {'a'..'z', 'A'..'Z', '0'..'9', '_'}


proc parseFloatNative(a: string): float =
  let a2 = a.cstring
  asm """
  `result` = Number(`a2`);
  """

#[
xxx how come code like this doesn't give IndexDefect ?
let z = s[10000] == 'a'
]#
proc nimParseBiggestFloat(s: string, number: var BiggestFloat, start: int): int {.compilerproc.} =
  var sign: bool
  var i = start
  if s[i] == '+': inc(i)
  elif s[i] == '-':
    sign = true
    inc(i)
  if s[i] == 'N' or s[i] == 'n':
    if s[i+1] == 'A' or s[i+1] == 'a':
      if s[i+2] == 'N' or s[i+2] == 'n':
        if s[i+3] notin IdentChars:
          number = NaN
          return i+3 - start
    return 0
  if s[i] == 'I' or s[i] == 'i':
    if s[i+1] == 'N' or s[i+1] == 'n':
      if s[i+2] == 'F' or s[i+2] == 'f':
        if s[i+3] notin IdentChars:
          number = if sign: -Inf else: Inf
          return i+3 - start
    return 0

  var buf: string
    # we could also use an `array[char, N]` buffer to avoid reallocs, or
    # use a 2-pass algorithm that first computes the length.
  if sign: buf.add '-'
  template addInc =
    buf.add s[i]
    inc(i)
  template eatUnderscores =
    while s[i] == '_': inc(i)
  while s[i] in {'0'..'9'}: # Read integer part
    buf.add s[i]
    inc(i)
    eatUnderscores()
  if s[i] == '.': # Decimal?
    addInc()
    while s[i] in {'0'..'9'}: # Read fractional part
      addInc()
      eatUnderscores()
  # Again, read integer and fractional part
  if buf.len == ord(sign): return 0
  if s[i] in {'e', 'E'}: # Exponent?
    addInc()
    if s[i] == '+': inc(i)
    elif s[i] == '-': addInc()
    if s[i] notin {'0'..'9'}: return 0
    while s[i] in {'0'..'9'}:
      addInc()
      eatUnderscores()
  number = parseFloatNative(buf)
  result = i - start

# Workaround for IE, IE up to version 11 lacks 'Math.trunc'. We produce
# 'Math.trunc' for Nim's ``div`` and ``mod`` operators:
const jsMathTrunc = """
if (!Math.trunc) {
  Math.trunc = function(v) {
    v = +v;
    if (!isFinite(v)) return v;
    return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
  };
}
"""
when not defined(nodejs): {.emit: jsMathTrunc .}

proc makeClosure(f: JSRef, env: JSRef) {.compilerproc, asmNoStackFrame.} =
  ## Used by the code generator to create a |NimSkull| closure object. A
  ## JavaScript function is used as the base object, so that passing
  ## |NimSkull| closures through the FFI where the other side expects a
  ## functions object works.
  asm """
    var cl = `f`.bind(`env`);
    cl.prc = `f`;
    cl.env = `env`;
    return cl;
  """

proc accessEnv(cl: JSRef): JSRef {.compilerproc, asmNoStackFrame.} =
  ## Used by the code generator to access the environment of a closure object
  ## (`cl`).
  ##
  ## The closure object might not be a proper one if it's provided from the
  ## JavaScript side via the FFI. In order to also support such usage for
  ## now, `null` is returned in that case.
  asm """
    (`cl`== null || `cl`.env == undefined ? null : `cl`.env)
  """

proc cmpClosures(a, b: JSRef): bool {.compilerproc, asmNoStackFrame.} =
  ## Compares two closure for value equality. If at least one of them is not a
  ## real closure (because an JavaScript object passed through the FFI
  ## incorrectly uses the ``.closure`` calling convention), the default
  ## equality operator is used.
  asm """
    if (`a` != null && `a`.env != undefined &&
        `b` != null && `b`.env != undefined) {
      return `a`.prc == `b`.prc && `a`.env == `b`.env;
    } else {
      return `a` == `b`;
    }
  """