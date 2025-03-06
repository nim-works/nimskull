#
#
#                 NimSkull's runtime library
#     (c) Copyright 2025 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

## This module defines default command line parsers for primitive types

import std/strutils
import std/typetraits

proc parseCli*(T: typedesc[SomeInteger], value: string): T =
  let parsed =
    when T is SomeSignedInt:
      parseBiggestInt(value)
    else:
      parseBiggestUInt(value)

  if parsed notin low(T)..high(T):
    raise newException(ValueError):
      $parsed & " is not in the range of " & $low(T) & ".." & $high(T)

  result = T(parsed)

proc parseCli*(T: typedesc[SomeFloat], value: string): T =
  let parsed = parseFloat(value)

  if parsed notin low(T)..high(T):
    raise newException(ValueError):
      $parsed & " is not in the range of " & $low(T) & ".." & $high(T)

  result = T(parsed)

proc parseCli*(T: typedesc[string], value: string): T =
  value

proc parseCli*(T: typedesc[bool], value: string): T =
  parseBool(value)

proc parseCli*(T: typedesc[enum], value: string): T =
  parseEnum(value)

proc parseCli*(T: typedesc[range], value: string): T =
  let parsed = parseCli(rangeBase(T), value)

  if parsed notin low(T)..high(T):
    raise newException(ValueError):
      $parsed & " is not in the range of " & $low(T) & ".." & $high(T)

  result = T(parsed)
