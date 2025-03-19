#
#
#                 NimSkull's runtime library
#     (c) Copyright 2025 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/hashes
import std/options
import std/strutils
import std/tables

import cmdline/parsers
export parsers

import lexopt

# FIXME: Move containers to private stdlib
import "$nim"/compiler/utils/containers

type
  Parser*[T] = proc (option, value: string, result: var T): Action
  OptionalParser*[T] = proc (option: string, value: Option[string], result: var T): Action
  TypedParser*[T; U] = proc (option: string, value: U, result: var T): Action
  OptionalTypedParser*[T; U] = proc (option: string, value: Option[U], result: var T): Action

  ParserAny[T] = object
    case isOptional: bool
    of true: optParser: OptionalParser[T]
    of false: parser: Parser[T]

  Cli*[T] = object
    ## A command line parser
    flag: Table[string, Id] ## Lookup mapping of flag names to Id
    parser: Store[Id, ParserAny[T]] ## Parser to process input for Id
    # XXX: Maybe allow name to store 2 values since that's the common case, then
    # alias for the rest
    name: Store[Id, string] ## Canonical names for all Ids
    alias: Table[Id, seq[string]] ## Mapping of Id to aliases

    # Documentation storage
    #
    # Might be useful to support not having these for space-constrained
    # targets using a define.
    usage: Store[Id, string] ## Canonical usage for Ids
    placeholder: Store[Id, string] ## Canonical placeholder for Ids

  ParseResult*[T] = object
    result*: T ## Result of the parse
    remaining*: seq[string] ## Arguments that were not parsed

  FlagBuilder*[T] = object
    flagParser: ParserAny[T]
    flagName: string
    aliases: seq[string]
    usage: string
    placeholder: string

  Action* {.pure.} = enum
    Continue, ## Continue parameter parsing
    ShowHelp ## Abort and show help message

  ParseErrorBase* = object of CatchableError

  ParseError*[T] = object of ParseErrorBase
    ## An error during command line parsing
    result*: ParseResult[T] ## The parse result upto the error

  FlagError*[T] = object of ParseError[T]
    ## An error parsing flags
    flagName*: string ## Name of the flag causing the error, as specified by
                      ## input
  UnknownFlagError*[T] = object of FlagError[T]
    ## The flag parsed was not recognized
  MissingValueError*[T] = object of FlagError[T]
    ## The flag parsed requires a value but was not provided
    flag*: Flag ## Handle to the flag
  InvalidValueError*[T] = object of FlagError[T]
    ## Invalid value passed to a flag
    ##
    ## The parent `ValueError` can be found in the `parent` field
    flagValue*: Option[string] ## The string value received. This should always
                               ## be `some(string)` for flags with non-optional
                               ## value
    flag*: Flag ## Handle to the flag

  HelpError*[T] = object of ParseError[T]
    ## Help was requested
    flagName*: string ## Name of the flag triggering help, as specified by
                      ## input
    flag*: Flag ## Handle to the flag triggering help

  Id = distinct uint32
  Flag* = distinct Id

proc hash(x: Id): Hash {.borrow.}

# FIXME: I have no idea why this has to be exported
proc `==`*(a, b: Id): bool {.borrow.}
proc `==`*(a, b: Flag): bool {.borrow.}

func initCli*(T: typedesc): Cli[T] =
  result = Cli[T]()

func flagBuilder*[T](cli: Cli[T]): FlagBuilder[T] =
  result = FlagBuilder[T]()

func name*[T](b: sink FlagBuilder[T], name: string): FlagBuilder[T] =
  result = b
  result.flagName = name

func alias*[T](b: sink FlagBuilder[T], names: varargs[string]): FlagBuilder[T] =
  result = b

  # Not the fastest method, but it's expected that users will
  # specify at most 4 of these.
  for name in names.items:
    if result.flagName == name or name in result.aliases:
      continue
    result.aliases.add names

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink OptionalParser[T],
): FlagBuilder[T] =
  result = b
  result.flagParser = ParserAny[T](isOptional: true, optParser: p)

func parser*[T](b: sink FlagBuilder[T], p: sink Parser[T]): FlagBuilder[T] =
  result = b
  result.flagParser = ParserAny[T](isOptional: false, parser: p)

func optionalParser*[T, U](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink OptionalTypedParser[T, U],
): FlagBuilder[T] =
  mixin parseCli

  when U is string:
    result = b.optionalParser(OptionalParser[T] parser)
  else:
    result = b.optionalParser(
      proc (option: string, value: Option[string], r: var T): Action =
        let value = value.map(proc (x: string): U = parseCli(U, x))
        result = parser(option, value, r)
    )

func parser*[T, U](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink TypedParser[T, U],
): FlagBuilder[T] =
  mixin parseCli

  when U is bool:
    result = b.optionalParser(
      proc (option: string, value: Option[string], r: var T): Action =
        result = parser(option, parseCli(U, value.get("true")), r)
    )

  elif U is string:
    result = b.parser(Parser[T] parser)

  else:
    result = b.parser(
      proc (option, value: string, r: var T): Action =
        result = parser(option, parseCli(U, value), r)
    )

func describe*[T](
  b: sink FlagBuilder[T],
  usage: sink string,
  placeholder: sink string = "",
): FlagBuilder[T] =
  result = b
  result.usage = usage
  result.placeholder = placeholder

func addTo*[T](b: sink FlagBuilder[T], cli: var Cli[T]): Flag {.discardable.} =
  assert b.flagName.len > 0, "Flag name must not be empty"
  if b.flagName in cli.flag:
    raise newException(ValueError, "Flag '" & b.flagName & "' already exists")
  for alias in b.aliases.items:
    if alias in cli.flag:
      raise newException(ValueError, "Flag '" & alias & "' already exists")

  if b.flagParser.isOptional:
    assert b.flagParser.optParser != nil, "Parser must be non-nil"
  else:
    assert b.flagParser.parser != nil, "Parser must be non-nil"

  result = Flag cli.name.add(b.flagName)
  discard cli.usage.add(b.usage)
  discard cli.placeholder.add(b.placeholder)
  discard cli.parser.add(b.flagParser)
  cli.flag[b.flagName] = Id result
  for alias in b.aliases.items:
    cli.flag[alias] = Id result
  if b.aliases.len > 0:
    cli.alias[Id result] = b.aliases

func flagWithName*(cli: Cli, name: string): Option[Flag] =
  try: some(Flag cli.flag[name])
  except KeyError: none Flag

func nameOf*(cli: Cli, flag: Flag): lent string =
  cli.name[Id flag]

iterator namesOf*(cli: Cli, flag: Flag): lent string =
  try:
    yield cli.name[Id flag]
    for name in cli.alias[Id flag].items:
      yield name
  except KeyError:
    discard "Flag has no aliases"

func longNameOf*(cli: Cli, flag: Flag): Option[string] =
  result = none string
  for name in cli.namesOf(flag):
    if name.len > 1:
      return some name

func shortNameOf*(cli: Cli, flag: Flag): Option[string] =
  result = none string
  for name in cli.namesOf(flag):
    if name.len == 1:
      return some name

func usageOf*(cli: Cli, flag: Flag): lent string =
  cli.usage[Id flag]

func placeholderOf*(cli: Cli, flag: Flag): string =
  cli.placeholder[Id flag]

func isValueOptional*(cli: Cli, flag: Flag): bool =
  cli.parser[Id flag].isOptional

proc helpFlagBuilder*[T](cli: var Cli[T], name: sink string = "help"): FlagBuilder[T] =
  cli.flagBuilder
    .name(name)
    .optionalParser(
      proc (_: string, v: Option[string], r: var T): Action = ShowHelp
    )
    .describe("display help message")

proc addHelpFlag*[T](cli: var Cli[T], name: sink string = "help"): Flag {.discardable.} =
  cli.helpFlagBuilder
    .name(name)
    .addTo(cli)

func collectRemaining(result: var ParseResult, lexer: var CmdLexer) =
  for arg in lexer.remaining:
    result.remaining.add arg

func newHelpError[T](
  flagName: sink string,
  flag: Flag,
  pr: sink ParseResult[T],
): ref HelpError[T] {.raises: [].} =
  (ref HelpError[T])(
    msg: "help requested",
    result: pr,
    flagName: flagName,
    flag: flag,
  )

func newUnknownFlagError[T](
  flagName: sink string,
  pr: sink ParseResult[T]
): ref UnknownFlagError[T] {.raises: [].} =
  (ref UnknownFlagError[T])(
    msg: "unexpected flag '" & flagName & "'",
    result: pr,
    flagName: flagName
  )

func newMissingValueError[T](
  flagName: sink string,
  flag: Flag,
  pr: sink ParseResult[T]
): ref MissingValueError[T] {.raises: [].} =
  (ref MissingValueError[T])(
    msg: "missing value for flag '" & flagName & "'",
    result: pr,
    flagName: flagName,
    flag: flag
  )

func newInvalidValueError[T](
  parent: ref ValueError,
  flagName: sink string,
  flag: Flag,
  value: sink Option[string],
  pr: sink ParseResult[T]
): ref InvalidValueError[T] {.raises: [].} =
  (ref InvalidValueError[T])(
    msg: "invalid value for flag '" & flagName & "': " & $value,
    result: pr,
    flagName: flagName,
    flagValue: value,
    flag: flag,
    parent: parent
  )

func parse*[T](
  cli: Cli[T],
  args: sink seq[string],
  init: sink T = default(T)
): ParseResult[T] {.raises: [ParseError[T]].} =
  var lexer = initCmdLexer(args)

  result.result = move init

  try:
    while (let (kind, option) = lexer.next(); kind != cmdEnd):
      case kind
      of cmdLong, cmdShort:
        let flagId =
          try: cli.flag[option]
          except KeyError:
            result.collectRemaining lexer
            raise newUnknownFlagError(kind.prefix & option, move result)

        let parser = cli.parser[flagId]
        let optValue = lexer.value(delimitedOnly = parser.isOptional)
        let action =
          try:
            if parser.isOptional:
              parser.optParser(option, optValue, result.result)
            else:
              if optValue.isNone:
                result.collectRemaining lexer
                raise newMissingValueError(kind.prefix & option, Flag flagId, move result)

              parser.parser(option, optValue.unsafeGet(), result.result)
          except ValueError as e:
            result.collectRemaining lexer
            raise newInvalidValueError(e, kind.prefix & option, Flag flagId, optValue, move result)

        if action == ShowHelp:
          result.collectRemaining lexer
          raise newHelpError(kind.prefix & option, Flag flagId, move result)

      of cmdValue:
        if option != "--":
          result.remaining.add option
        else:
          result.collectRemaining lexer

      of cmdEnd:
        doAssert false, "unreachable!"
  except UnexpectedValueError:
    doAssert false, "unreachable!"

func parse*[T](cli: Cli[T], args: openArray[string], init: sink T = default(T)): ParseResult[T] {.inline.} =
  parse(cli, @args, init)

iterator flags*(cli: Cli): Flag =
  for i in 0 ..< cli.name.nextId.int:
    yield Flag(i)

func flagsUsage*(cli: Cli): string =
  var lines: seq[(string, string)]
  var flagPad: int
  for flag in cli.flags:
    let optional = cli.isValueOptional(flag)
    let placeholder = cli.placeholderOf(flag)
    let short = cli.shortNameOf(flag).map(proc (v: string): string = "-" & v).get("")
    let long = cli.longNameOf(flag).map(proc (v: string): string = "--" & v).get("")
    let valueSuffix =
      if optional:
        if placeholder == "":
          ""
        else:
          "[=<" & placeholder & ">]"
      else:
        if placeholder == "":
          " <VALUE>"
        else:
          " <" & placeholder & ">"

    var display: string
    display.add short
    if display != "" and long != "":
      display.add ", "
    display.add long
    display.add valueSuffix

    flagPad = max(flagPad, display.len)
    lines.add (display, cli.usageOf(flag))

  flagPad.inc 2
  for (flag, usage) in lines.items:
    if result.len > 0:
      result.add "\n"
    result.add "  "
    result.add:
      flag.alignLeft:
        if usage.len > 0: flagPad else: 0
    result.add usage
