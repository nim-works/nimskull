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

import std/private/containers

import cmdline/parsers
export parsers

import lexopt

type
  Parser*[T] = proc (option, value: string, result: var T): Action
  OptionalParser*[T] = proc (option: string, value: Option[string], result: var T): Action
  PositionalParser*[T] = proc (value: string, result: var T): Action
  TypedParser*[T; U] = proc (option: string, value: U, result: var T): Action
  OptionalTypedParser*[T; U] = proc (option: string, value: Option[U], result: var T): Action
  TypedPositionalParser*[T; U] = proc (value: U, result: var T): Action

  Cli*[T] = object
    ## A command line parser
    flag: Table[string, Parameter] ## Lookup mapping of flag names to Parameter
    positional: seq[Parameter] ## Lookup mapping of position to Parameter
    parser: Store[Parameter, ParserAny[T]] ## Parser to process input for Parameter
    # XXX: Maybe allow name to store 2 values since that's the common case, then
    # alias for the rest
    name: Store[Parameter, string] ## Canonical names for all Parameters
    alias: Table[Parameter, seq[string]] ## Mapping of Parameter to aliases

    # Documentation storage
    #
    # Might be useful to support not having these for space-constrained
    # targets using a define.
    usage: Store[Parameter, string] ## Canonical usage for Parameters
    placeholder: Store[Parameter, string] ## Canonical placeholder for
                                          ## Parameters. Only used for flags

  FlagBuilder*[T] = object
    flagParser: ParserAny[T]
    flagName: string
    aliases: seq[string]
    usage: string
    placeholder: string

  PositionalBuilder*[T] = object
    posParser: ParserAny[T]
    posName: string
    usage: string

  Action* {.pure.} = enum
    Continue ## Continue parameter parsing
    ShowHelp ## Abort and show help message
    DisableFlagProcessing ## Parameters following this will be considered
                          ## to be values

  ParserKind {.pure.} = enum
    Flag
    FlagOptionalValue
    Positional
    OptionalPositional
    CatchAll
    OptionalCatchAll

  ParserAny[T] = object
    case kind: ParserKind
    of FlagOptionalValue: optParser: OptionalParser[T]
    of ParserKind.Flag: parser: Parser[T]
    of ParserKind.Positional, OptionalPositional, CatchAll,
       OptionalCatchAll: posParser: PositionalParser[T]

  ParseContext = object
    ## Parser internal state
    lexer: CmdLexer ## Lexer driving the parse
    nextPositional: Natural ## Next positional parser to use
    positionalCount: Natural ## Number of positionals parsed

  ParseError* = object of CatchableError
    ## An error during command line parsing
    remaining*: seq[string] ## Parameters that were not parsed

  FlagError* = object of ParseError
    ## An error parsing flags
    flagName*: string ## Name of the flag causing the error, as specified by
                      ## input
  UnknownFlagError* = object of FlagError
    ## The flag parsed was not recognized
  MissingValueError* = object of FlagError
    ## The flag parsed requires a value but was not provided
    flag*: Flag ## Handle to the flag
  InvalidValueError* = object of FlagError
    ## Invalid value passed to a flag
    ##
    ## The parent `ValueError` can be found in the `parent` field
    flagValue*: Option[string] ## The string value received. This should always
                               ## be `some(string)` for flags with non-optional
                               ## value
    flag*: Flag ## Handle to the flag

  PositionalError* = object of ParseError
    ## An error parsing positional parameters
    position*: Natural ## Position of the parameter in the input stream
    positionalValue*: string ## Input value causing the error
  UnknownPositionalError* = object of PositionalError
    ## The positional parsed was not recognized
  MissingPositionalError* = object of PositionalError
    ## A required positional parameter is missing from input
    positional*: Positional ## Handle to the positional
  InvalidPositionalError* = object of PositionalError
    ## The positional parsed was invalid
    ##
    ## The parent `ValueError` can be found in the `parent` field
    positional*: Positional ## Handle to the positional

  HelpError* = object of ParseError
    ## Help was requested
    paramName*: string ## Name of the parameter triggering help, as specified by
                       ## input
    param*: Parameter ## Handle to the flag triggering help

  Parameter* = distinct uint32
  Flag* = distinct Parameter
  Positional* = distinct Parameter

proc hash(x: Parameter): Hash {.borrow.}

proc `==`*(a, b: Parameter): bool {.borrow.}
proc `==`*(a, b: Flag): bool {.borrow.}
proc `==`*(a, b: Positional): bool {.borrow.}

# FIXME: Put this in system.nim
func drop[T](_: sink T) = discard

func initCli*(T: typedesc): Cli[T] =
  result = Cli[T]()

func flagBuilder*[T](cli: Cli[T]): FlagBuilder[T] =
  result = FlagBuilder[T]()

func positionalBuilder*[T](cli: Cli[T]): PositionalBuilder[T] =
  result = PositionalBuilder[T]()

func optional*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  result = b
  let parser =
    case result.posParser.kind
    of ParserKind.Flag, FlagOptionalValue:
      nil
    of ParserKind.Positional..OptionalCatchAll:
      result.posParser.posParser

  case result.posParser.kind
  of ParserKind.Flag..OptionalPositional:
    result.posParser = ParserAny[T](
      kind: OptionalPositional,
      posParser: parser
    )
  of CatchAll, OptionalCatchAll:
    result.posParser = ParserAny[T](
      kind: OptionalCatchAll,
      posParser: parser
    )

func catchAll*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  result = b
  let parser =
    case result.posParser.kind
    of ParserKind.Flag, FlagOptionalValue:
      nil
    of ParserKind.Positional..OptionalCatchAll:
      result.posParser.posParser

  case result.posParser.kind
  of ParserKind.Flag..ParserKind.Positional, CatchAll:
    result.posParser = ParserAny[T](
      kind: ParserKind.CatchAll,
      posParser: parser
    )
  of OptionalPositional, OptionalCatchAll:
    result.posParser = ParserAny[T](
      kind: OptionalCatchAll,
      posParser: parser
    )

func name*[T](b: sink FlagBuilder[T], name: string): FlagBuilder[T] =
  result = b
  result.flagName = name

func name*[T](b: sink PositionalBuilder[T], name: string): PositionalBuilder[T] =
  result = b
  result.posName = name

func alias*[T](b: sink FlagBuilder[T], names: varargs[string]): FlagBuilder[T] =
  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's expected that users will
  # specify at most 4 of these.
  for name in names.items:
    if name == result.flagName or name in result.aliases:
      continue
    result.aliases.add names

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink OptionalParser[T],
): FlagBuilder[T] =
  result = b
  result.flagParser = ParserAny[T](kind: FlagOptionalValue, optParser: p)

func parser*[T](b: sink FlagBuilder[T], p: sink Parser[T]): FlagBuilder[T] =
  result = b
  result.flagParser = ParserAny[T](kind: ParserKind.Flag, parser: p)

func parser*[T](b: sink PositionalBuilder[T], p: sink PositionalParser[T]): PositionalBuilder[T] =
  result = b
  case result.posParser.kind
  of ParserKind.Positional..OptionalCatchAll:
    result.posParser.posParser = p
  else:
    result.posParser = ParserAny[T](kind: ParserKind.Positional, posParser: p)

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

func parser*[T, U](
  b: sink PositionalBuilder[T],
  _: typedesc[U],
  parser: sink TypedPositionalParser[T, U],
): PositionalBuilder[T] =
  mixin parseCli

  when U is string:
    result = b.parser(PositionalParser[T] parser)
  else:
    result = b.parser(
      proc (value: string, r: var T): Action =
        result = parser(parseCli(U, value), r)
    )

func describe*[T](
  b: sink FlagBuilder[T],
  usage: sink string,
  placeholder: sink string = "",
): FlagBuilder[T] =
  result = b
  result.usage = usage
  result.placeholder = placeholder

func describe*[T](
  b: sink PositionalBuilder[T],
  usage: sink string,
): PositionalBuilder[T] =
  result = b
  result.usage = usage

func isNil[T](p: ParserAny[T]): bool =
  case p.kind
  of ParserKind.Flag: p.parser == nil
  of FlagOptionalValue: p.optParser == nil
  of ParserKind.Positional..OptionalCatchAll: p.posParser == nil

func addCommon[T](
  cli: var Cli[T],
  name, usage, placeholder: sink string,
  parser: sink ParserAny[T]
): Parameter =
  result = cli.name.add(name)
  discard cli.usage.add(usage)
  discard cli.placeholder.add(placeholder)
  discard cli.parser.add(parser)

func addTo*[T](b: sink FlagBuilder[T], cli: var Cli[T]): Flag {.discardable.} =
  assert b.flagName.len > 0, "Flag name must not be empty"
  if b.flagName in cli.flag:
    raise newException(ValueError, "Flag '" & b.flagName & "' already exists")
  for alias in b.aliases.items:
    assert alias != "", "Flag alias cannot be empty"
    if alias in cli.flag:
      raise newException(ValueError, "Flag '" & alias & "' already exists")

  assert b.flagParser.kind in {ParserKind.Flag, FlagOptionalValue}
  assert not b.flagParser.isNil(), "Parser must be non-nil"

  result = Flag cli.addCommon(b.flagName, b.usage, b.placeholder, b.flagParser)
  cli.flag[b.flagName] = Parameter result
  for alias in b.aliases.items:
    cli.flag[alias] = Parameter result
  if b.aliases.len > 0:
    cli.alias[Parameter result] = b.aliases

func addTo*[T](b: sink PositionalBuilder[T], cli: var Cli[T]): Positional {.discardable.} =
  assert b.posName.len > 0, "Positional name should not be empty"
  for param in cli.positional.items:
    if b.posName == cli.name[param]:
      raise newException(ValueError):
        "Positional with name '" & cli.name[param] & "' already exists"

  assert b.posParser.kind in {ParserKind.Positional..OptionalCatchAll}
  assert not b.posParser.isNil(), "Parser must be non-nil"

  if cli.positional != []:
    let lastPos = cli.positional[^1]
    case cli.parser[lastPos].kind
    of CatchAll, OptionalCatchAll:
      raise newException(ValueError):
        "Cannot add positional after catch all positional: " & cli.name[lastPos]
    of OptionalPositional:
      if b.posParser.kind notin {OptionalPositional, OptionalCatchAll}:
        raise newException(ValueError):
          "Cannot add non-optional positional after optional positional: " & cli.name[lastPos]
    else:
      discard "No constraints"

  result = Positional cli.addCommon(b.posName, b.usage, placeholder = "", parser = b.posParser)
  cli.positional.add Parameter(result)

func flagWithName*(cli: Cli, name: string): Option[Flag] =
  try: some(Flag cli.flag[name])
  except KeyError: none Flag

func nameOf*(cli: Cli, flag: Flag): lent string =
  cli.name[Parameter flag]

func nameOf*(cli: Cli, positional: Positional): lent string =
  cli.name[Parameter positional]

iterator namesOf*(cli: Cli, flag: Flag): lent string =
  try:
    yield cli.name[Parameter flag]
    for name in cli.alias[Parameter flag].items:
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
  cli.usage[Parameter flag]

func usageOf*(cli: Cli, positional: Positional): lent string =
  cli.usage[Parameter positional]

func placeholderOf*(cli: Cli, flag: Flag): string =
  cli.placeholder[Parameter flag]

func isValueOptional*(cli: Cli, flag: Flag): bool =
  cli.parser[Parameter flag].kind == FlagOptionalValue

func isOptional*(cli: Cli, positional: Positional): bool =
  cli.parser[Parameter positional].kind in {OptionalPositional, OptionalCatchAll}

func isCatchAll*(cli: Cli, positional: Positional): bool =
  cli.parser[Parameter positional].kind in {CatchAll, OptionalCatchAll}

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

func collectRemaining(lexer: var CmdLexer): seq[string] =
  for arg in lexer.remaining:
    result.add arg

func newHelpError(
  paramName: sink string,
  param: Parameter,
  remaining: sink seq[string],
): ref HelpError {.raises: [].} =
  (ref HelpError)(
    msg: "help requested",
    paramName: paramName,
    param: param,
    remaining: remaining,
  )

func newUnknownFlagError(
  flagName: sink string,
  remaining: sink seq[string],
): ref UnknownFlagError {.raises: [].} =
  (ref UnknownFlagError)(
    msg: "unexpected flag '" & flagName & "'",
    flagName: flagName,
    remaining: remaining,
  )

func newMissingValueError(
  flagName: sink string,
  flag: Flag,
  remaining: sink seq[string],
): ref MissingValueError {.raises: [].} =
  (ref MissingValueError)(
    msg: "missing value for flag '" & flagName & "'",
    flagName: flagName,
    flag: flag,
    remaining: remaining,
  )

func newInvalidValueError(
  parent: ref ValueError,
  flagName: sink string,
  flag: Flag,
  value: sink Option[string],
  remaining: sink seq[string],
): ref InvalidValueError {.raises: [].} =
  (ref InvalidValueError)(
    msg: "invalid value for flag '" & flagName & "': " & $value,
    flagName: flagName,
    flagValue: value,
    flag: flag,
    parent: parent,
    remaining: remaining,
  )

func newUnknownPositionalError(
  position: Natural,
  positionalValue: sink string,
  remaining: sink seq[string],
): ref UnknownPositionalError {.raises: [].} =
  (ref UnknownPositionalError)(
    msg: "unexpected parameter: " & positionalValue,
    position: position,
    positionalValue: positionalValue,
    remaining: remaining,
  )

func newInvalidPositionalError(
  parent: ref ValueError,
  position: Natural,
  value: sink string,
  positional: Positional,
  remaining: sink seq[string],
): ref InvalidPositionalError {.raises: [].} =
  (ref InvalidPositionalError)(
    msg: "invalid parameter: " & $value,
    position: position,
    positionalValue: value,
    positional: positional,
    parent: parent,
    remaining: remaining,
  )

func newMissingPositionalError(
  position: Natural,
  positional: Positional
): ref MissingPositionalError {.raises: [].} =
  (ref MissingPositionalError)(
    msg: "missing required value for positional parameter",
    position: position,
    positional: positional,
  )

func option(kind: CmdlineKind, opt: string): string =
  ## Format `opt` to be an option of `kind`.
  kind.prefix & opt

func parseNext[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
) {.tailcall.}

func parseRemaining[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
) {.tailcall.}

func parsePositional[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  value: sink string,
  goNext = true,
) {.tailcall.} =
  if ctx.nextPositional < cli.positional.len:
    let
      posId = cli.positional[ctx.nextPositional]
      parser = cli.parser[posId]
      action =
        try:
          parser.posParser(value, accumulator)
        except ValueError as e:
          raise newInvalidPositionalError(
            e,
            ctx.positionalCount,
            value,
            Positional posId,
            collectRemaining ctx.lexer
          )

    inc ctx.positionalCount
    inc ctx.nextPositional, ord(parser.kind notin {CatchAll, OptionalCatchAll})

    case action
    of Continue:
      drop value
      if goNext:
        parseNext(ctx, cli, accumulator)
    of DisableFlagProcessing:
      drop value
      if goNext:
        parseRemaining(ctx, cli, accumulator)
    of ShowHelp:
      raise newHelpError(
        value, posId, collectRemaining ctx.lexer
      )

  else:
    raise newUnknownPositionalError(
      ctx.positionalCount, value, collectRemaining ctx.lexer
    )

func parseRemaining[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T
) {.tailcall.} =
  for value in ctx.lexer.remaining:
    # TODO: request a feature to override tail constraints
    (proc (ctx: var ParseContext, cli: Cli[T], accumulator: var T, value: string) =
      parsePositional(ctx, cli, accumulator, value, goNext = false)
    )(ctx, cli, accumulator, value)

  parseNext(ctx, cli, accumulator)

func parseFlag[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  kind: CmdlineKind,
  option: sink string,
) {.tailcall.} =
  let flagId =
    try: cli.flag[option]
    except KeyError:
      raise newUnknownFlagError(kind.option(option), collectRemaining ctx.lexer)

  let parser = cli.parser[flagId]
  let action = block:
    let optValue = ctx.lexer.value(delimitedOnly = parser.kind == FlagOptionalValue)
    try:
      case parser.kind
      of FlagOptionalValue:
        parser.optParser(option, optValue, accumulator)
      of ParserKind.Flag:
        if optValue.isNone:
          raise newMissingValueError(
            kind.option(option),
            Flag flagId,
            collectRemaining ctx.lexer
          )

        parser.parser(option, optValue.unsafeGet(), accumulator)
      else:
        unreachable()
    except ValueError as e:
      raise newInvalidValueError(
        e,
        kind.option(option),
        Flag flagId,
        optValue,
        collectRemaining ctx.lexer
      )

  case action
  of Continue:
    drop option
    parseNext(ctx, cli, accumulator)
  of DisableFlagProcessing:
    drop option
    parseRemaining(ctx, cli, accumulator)
  of ShowHelp:
    raise newHelpError(
      kind.option(option), flagId, collectRemaining ctx.lexer
    )

func parseNext[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T
) {.tailcall.} =
  let (kind, option) = ctx.lexer.next()

  case kind
  of cmdLong, cmdShort:
    parseFlag(ctx, cli, accumulator, kind, option)
  of cmdValue:
    if option == "--":
      drop option
      parseRemaining(ctx, cli, accumulator)
    else: parsePositional(ctx, cli, accumulator, option)
  of cmdEnd:
    drop option

    # Verify that we collected all required parameters
    if ctx.nextPositional < cli.positional.len:
      let posId = cli.positional[ctx.nextPositional]
      case cli.parser[posId].kind
      of ParserKind.Positional:
        raise newMissingPositionalError(ctx.positionalCount, Positional posId)
      of CatchAll:
        # Catch all hasn't collected any parameters
        if ctx.positionalCount <= ctx.nextPositional:
          raise newMissingPositionalError(ctx.positionalCount, Positional posId)
      of OptionalPositional, OptionalCatchAll:
        discard "nothing to do"
      else:
        unreachable()

func parse*[T](
  cli: Cli[T],
  accumulator: var T,
  args: sink seq[string],
) {.raises: [ParseError].} =
  var ctx = ParseContext(
    lexer: initCmdLexer(args),
  )

  try: parseNext(ctx, cli, accumulator)
  except UnexpectedValueError: unreachable()

func parse*[T](
  cli: Cli[T],
  args: sink seq[string],
): T {.inline, raises: [ParseError].} =
  parse(cli, result, args)

iterator flags*(cli: Cli): Flag =
  for i in 0 ..< cli.parser.nextId.int:
    if cli.parser[Parameter i].kind in {ParserKind.Flag, FlagOptionalValue}:
      yield Flag(i)

iterator positionals*(cli: Cli): Positional =
  for i in cli.positional.items:
    yield Positional(i)

func flagsUsage*(cli: Cli): string =
  var lines: seq[(string, string)]
  var flagPad: int
  for flag in cli.flags:
    let
      optional = cli.isValueOptional(flag)
      placeholder = cli.placeholderOf(flag)
      short = cli.shortNameOf(flag)
        .map(proc (v: string): string = "-" & v)
        .get("")
      long = cli.longNameOf(flag)
        .map(proc (v: string): string = "--" & v)
        .get("")
      valueSuffix =
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

func displayOf(cli: Cli, positional: Positional): string =
  if cli.isOptional(positional):
    result.add '['
    result.add cli.nameOf(positional)
    result.add ']'
  else:
    result.add '<'
    result.add cli.nameOf(positional)
    result.add '>'

  if cli.isCatchAll(positional):
    result.add "..."

func positionalsUsage*(cli: Cli): string =
  var
    lines: seq[(string, string)]
    posPad: int

  for positional in cli.positionals:
    let display = cli.displayOf(positional)
    posPad = max(posPad, display.len)
    lines.add (display, cli.usageOf(positional))

  posPad.inc 2
  for (display, usage) in lines.items:
    if result.len > 0:
      result.add "\n"
    result.add "  "
    result.add:
      display.alignLeft:
        if usage.len > 0: posPad else: 0
    result.add usage

func commandUsage*(cli: Cli, name: string): string =
  result.add name
  if result.len > 0:
    result.add ' '
  result.add "[OPTIONS]"
  for positional in cli.positionals:
    if result.len > 0:
      result.add ' '
    result.add cli.displayOf(positional)

func help*(cli: Cli, name: string): string =
  let
    usage = cli.commandUsage(name)
    args = cli.positionalsUsage
    flags = cli.flagsUsage

  result.add "Usage: "
  result.add usage

  if args.len > 0:
    result.add "\n\nArguments:\n"
    result.add args

  if flags.len > 0:
    result.add "\n\nOptions:\n"
    result.add flags
