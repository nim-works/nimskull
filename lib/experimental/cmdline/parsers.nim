#
#
#                 NimSkull's runtime library
#     (c) Copyright 2025 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

## This module defines default command line parsers for primitive types as
## expected by `cmdline`.
##
## These parsers are employed when using typed parsers, for example, in:
##
## ```nim
## cli.flagBuilder()
##   .parser(int, proc (opt: string, value: int, c: var Result) = discard)
## ```
##
## `parseCli(int, input)` will be called on the input string from the command
## line before being handed over to the registered parser.
##
## `cmdline` expects custom parsers to employ the following signature:
##
## ```nim
## proc parseCli(T: typedesc[CustomType], value: string): T
## ```
##
## The custom parser shall raise `ValueError` if `value` could not be correctly
## parsed as `CustomType`.
runnableExamples:
  import experimental/cmdline
  import std/json
  import std/sugar

  ## This example implements `parseCli` for type `Custom`

  type Custom = object
    i: int

  proc parseCli(T: typedesc[Custom], value: string): T =
    let jnode = parseJson(value)
    try:
      result = jnode.to(T)
    except CatchableError as err:
      raise newException(ValueError):
        "'" & value & "' is invalid: " & err.msg

  var cli = commandBuilder(Custom)
    .initCli()
  cli.positionalBuilder()
    .name("VALUE")
    .describe("JSON form of Custom")
    .parser(Custom, (v, var c) => (c = v))
    .addTo(cli)

  doAssert cli.parse(@["""{"i": 10}"""]) == Custom(i: 10)
  doAssertRaises(InvalidPositionalError):
    discard cli.parse(@["not-json"])
  doAssertRaises(InvalidPositionalError):
    discard cli.parse(@["""{"x": 10}"""])

import std/strutils
import std/typetraits

proc parseCli*(T: typedesc[SomeInteger], value: string): T =
  ## Implements `parseCli` for all integers.
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
  ## Implements `parseCli` for all floats.
  let parsed = parseFloat(value)

  if parsed notin low(T)..high(T):
    raise newException(ValueError):
      $parsed & " is not in the range of " & $low(T) & ".." & $high(T)

  result = T(parsed)

proc parseCli*(T: typedesc[string], value: string): T =
  ## Implements `parseCli` for `string`.
  value

proc parseCli*(T: typedesc[bool], value: string): T =
  ## Implements `parseCli` for boolean.
  parseBool(value)

proc parseCli*(T: typedesc[enum], value: string): T =
  ## Implements `parseCli` for enum types.
  parseEnum[T](value)

proc parseCli*(T: typedesc[range], value: string): T =
  ## Implements `parseCli` for range types.
  let parsed = parseCli(rangeBase(T), value)

  if parsed notin low(T)..high(T):
    raise newException(ValueError):
      $parsed & " is not in the range of " & $low(T) & ".." & $high(T)

  result = T(parsed)
