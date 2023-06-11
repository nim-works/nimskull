#
#
#            Nim's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Implementation of some runtime checks.
include system/indexerrors

proc raiseRangeError(val: BiggestInt) {.compilerproc, noinline.} =
  when hostOS == "standalone":
    sysFatal(RangeDefect, "value out of range")
  else:
    sysFatal(RangeDefect, "value out of range: ", $val)

proc raiseIndexError3(i, a, b: int) {.compilerproc, noinline.} =
  sysFatal(IndexDefect, formatErrorIndexBound(i, a, b))

proc raiseIndexError2(i, n: int) {.compilerproc, noinline.} =
  sysFatal(IndexDefect, formatErrorIndexBound(i, n))

proc raiseIndexError() {.compilerproc, noinline.} =
  sysFatal(IndexDefect, "index out of bounds")

proc raiseFieldError(f: string) {.compilerproc, noinline.} =
  ## remove after bootstrap > 1.5.1
  sysFatal(FieldDefect, f)

proc raiseFieldErrorBool(f: string, val: bool) {.compilerproc, noinline.} =
  sysFatal(FieldError, formatFieldDefect(f, $val))

when false:
  # XXX: the character value needs to be escaped properly, but the ``reprChar``
  #      is not defined yet (or at all)
  proc raiseFieldErrorChar(f: string, val: char) {.compilerproc, noinline.} =
    sysFatal(FieldError, formatFieldDefect(f, $val))

proc raiseFieldErrorInt(f: string, val: int64) {.compilerproc, noinline.} =
  sysFatal(FieldError, formatFieldDefect(f, $val))

proc raiseFieldErrorUInt(f: string, val: uint64) {.compilerproc, noinline.} =
  sysFatal(FieldError, formatFieldDefect(f, $val))

proc raiseFieldErrorStr(f: string, val: string) {.compilerproc, noinline.} =
  sysFatal(FieldError, formatFieldDefect(f, val))

when defined(nimV2):
  proc raiseFieldError2(f: string, discVal: int) {.compilerproc, noinline.} =
    ## Obsolete. Remove after updating the csources compiler
    sysFatal(FieldError, formatFieldDefect(f, $discVal))
else:
  proc raiseFieldError2(f: string, discVal: string) {.compilerproc, noinline.} =
    ## Obsolete. Remove after updating the csources compiler
    sysFatal(FieldError, formatFieldDefect(f, discVal))

proc raiseRangeErrorI(i, a, b: BiggestInt) {.compilerproc, noinline.} =
  when defined(standalone):
    sysFatal(RangeDefect, "value out of range")
  else:
    sysFatal(RangeDefect, "value out of range: " & $i & " notin " & $a & " .. " & $b)

proc raiseRangeErrorF(i, a, b: float) {.compilerproc, noinline.} =
  when defined(standalone):
    sysFatal(RangeDefect, "value out of range")
  else:
    sysFatal(RangeDefect, "value out of range: " & $i & " notin " & $a & " .. " & $b)

proc raiseRangeErrorU(i, a, b: uint64) {.compilerproc, noinline.} =
  # todo: better error reporting
  sysFatal(RangeDefect, "value out of range")

proc raiseRangeErrorNoArgs() {.compilerproc, noinline.} =
  sysFatal(RangeDefect, "value out of range")

proc raiseObjectConversionError() {.compilerproc, noinline.} =
  sysFatal(ObjectConversionDefect, "invalid object conversion")

proc chckIndx(i, a, b: int): int =
  if i >= a and i <= b:
    return i
  else:
    raiseIndexError3(i, a, b)

proc chckRange(i, a, b: int): int =
  if i >= a and i <= b:
    return i
  else:
    raiseRangeError(i)

proc chckRange64(i, a, b: int64): int64 {.compilerproc.} =
  if i >= a and i <= b:
    return i
  else:
    raiseRangeError(i)

proc chckRangeU(i, a, b: uint64): uint64 {.compilerproc.} =
  if i >= a and i <= b:
    return i
  else:
    sysFatal(RangeDefect, "value out of range")

proc chckRangeF(x, a, b: float): float =
  if x >= a and x <= b:
    return x
  else:
    when hostOS == "standalone":
      sysFatal(RangeDefect, "value out of range")
    else:
      sysFatal(RangeDefect, "value out of range: ", $x)

proc chckNil(p: pointer) =
  if p == nil:
    sysFatal(NilAccessDefect, "attempt to write to a nil address")

proc chckNilDisp(p: pointer) {.compilerproc.} =
  if p == nil:
    sysFatal(NilAccessDefect, "cannot dispatch; dispatcher is nil")

when defined(nimV2):
  proc raiseObjectCaseTransition() {.compilerproc.} =
    sysFatal(FieldDefect, "assignment to discriminant changes object branch")
