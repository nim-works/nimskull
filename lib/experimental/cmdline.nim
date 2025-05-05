#
#
#                 NimSkull's runtime library
#     (c) Copyright 2025 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

from os import commandLineParams

import std/algorithm
import std/hashes
import std/options
import std/strutils
import std/tables

import std/private/containers

import cmdline/parsers
export parsers

import lexopt

type
  Parser*[T; R: MaybeAction] = proc (option, value: string, result: var T): R
  OptionalParser*[T; R: MaybeAction] = proc (option: string, value: Option[string], result: var T): R
  PositionalParser*[T; R: MaybeAction] = proc (value: string, result: var T): R
  CommandParser*[T; R: MaybeAction] = proc (command: Command, result: var T): R
  TypedParser*[T; U; R: MaybeAction] = proc (option: string, value: U, result: var T): R
  OptionalTypedParser*[T; U; R: MaybeAction] = proc (option: string, value: Option[U], result: var T): R
  TypedPositionalParser*[T; U; R: MaybeAction] = proc (value: U, result: var T): R

  Cli*[T] {.requiresInit.} = object
    ## A command line parser
    command: Table[Command, CliCommand] ## Lookup mapping of command to lookup tables
    parser: Store[Parameter, ParserAny[T]] ## Parser to process input for Parameter
    name: Store[Parameter, string] ## Canonical names for all Parameters
    alias: Table[Parameter, seq[string]] ## Mapping of Parameter to aliases
    parent: Store[Parameter, Parameter] ## Mapping of Parameter to their parent

    # Documentation storage
    #
    # Might be useful to support not having these for space-constrained
    # targets using a define.
    usage: Store[Parameter, string] ## Canonical usage for Parameters
    placeholder: Store[Parameter, string] ## Canonical placeholder for
                                          ## Parameters. Only used for flags

  CliCommand = object
    flag: Table[string, Parameter] ## Lookup mapping of flag names to Parameter
    command: Table[string, Command] ## Lookup mapping of command names to Command
    positional: seq[Parameter] ## Lookup mapping of position to Parameter

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

  CommandBuilder*[T] = object
    cmdParser: ParserAny[T]
    cmdName: string
    aliases: seq[string]
    usage: string
    isDefault: bool

  Action* {.pure.} = enum
    Continue ## Continue parameter parsing
    ShowHelp ## Abort and show help message
    DisableFlagProcessing ## Parameters following this will be considered
                          ## to be values

  MaybeAction* = Action | void

  ParserKind {.pure.} = enum
    Command
    Flag
    FlagOptionalValue
    Positional
    OptionalPositional
    CatchAll
    OptionalCatchAll

  ParserAny[T] = object
    case kind: ParserKind
    of FlagOptionalValue: optParser: OptionalParser[T, Action]
    of ParserKind.Flag: parser: Parser[T, Action]
    of ParserKind.Positional, OptionalPositional, CatchAll,
       OptionalCatchAll: posParser: PositionalParser[T, Action]
    of ParserKind.Command: cmdParser: CommandParser[T, Action]

  ParseContext = object
    ## Parser internal state
    lexer: CmdLexer ## Lexer driving the parse
    nextPositional: Natural ## Next positional parser to use
    positionalCount: Natural ## Number of positionals parsed
    command: Command ## The active command
    isValueOnly: bool ## Whether flags are ignored

  ParseError* = object of CatchableError
    ## An error during command line parsing
    command*: Command ## Active command during error
    remaining*: seq[string] ## Parameters that were not parsed

  FlagError* = object of ParseError
    ## An error parsing flags
    flagName*: string ## Name of the flag causing the error, as specified by
                      ## input
  UnknownFlagError* = object of FlagError
    ## The flag parsed was not recognized
    flagValue*: Option[string] ## Inline value of flag causing error
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

  CommandError* = object of ParseError
    ## An error parsing command
    commandName*: string ## Input value causing the error
  UnknownCommandError* = object of CommandError
    ## The command parsed was not recognized
  MissingCommandError* = object of CommandError
    ## A required command is missing from input
  InvalidCommandError* = object of CommandError
    ## The command parsed was rejected by parser
    targetCommand*: Command ## Handle to the target command

  HelpError* = object of ParseError
    ## Help was requested
    paramName*: string ## Name of the parameter triggering help, as specified by
                       ## input
    param*: Parameter ## Handle to the flag triggering help

  ParameterKind* {.pure.} = enum
    Command
    Flag
    Positional

  Parameter* = distinct uint32
  Command* = distinct Parameter
  Flag* = distinct Parameter
  Positional* = distinct Parameter

const
  InvalidParameter* = high(Parameter)
  RootCommand* = Command(0)

proc hash(x: Parameter): Hash {.borrow.}

proc `==`*(a, b: Parameter): bool {.borrow.}
proc `==`*(a, b: Command): bool {.borrow.}
proc `==`*(a, b: Flag): bool {.borrow.}
proc `==`*(a, b: Positional): bool {.borrow.}

# FIXME: Put this in system.nim
func drop[T](_: sink T) = discard

func commandBuilder*[T](cli: Cli[T]): CommandBuilder[T] =
  result = CommandBuilder[T]()

func commandBuilder*(T: typedesc): CommandBuilder[T] =
  result = CommandBuilder[T]()

func flagBuilder*[T](cli: Cli[T]): FlagBuilder[T] =
  result = FlagBuilder[T]()

func positionalBuilder*[T](cli: Cli[T]): PositionalBuilder[T] =
  result = PositionalBuilder[T]()

func optional*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  result = b
  let parser =
    case result.posParser.kind
    of ParserKind.Command..FlagOptionalValue:
      nil
    of ParserKind.Positional..OptionalCatchAll:
      result.posParser.posParser

  case result.posParser.kind
  of ParserKind.Command..OptionalPositional:
    result.posParser = ParserAny[T](
      kind: OptionalPositional,
      posParser: parser
    )
  of CatchAll, OptionalCatchAll:
    result.posParser = ParserAny[T](
      kind: OptionalCatchAll,
      posParser: parser
    )

func default*[T](b: sink CommandBuilder[T]): CommandBuilder[T] =
  result = b
  result.isDefault = true

func catchAll*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  result = b
  let parser =
    case result.posParser.kind
    of ParserKind.Command..FlagOptionalValue:
      nil
    of ParserKind.Positional..OptionalCatchAll:
      result.posParser.posParser

  case result.posParser.kind
  of ParserKind.Command..ParserKind.Positional, CatchAll:
    result.posParser = ParserAny[T](
      kind: ParserKind.CatchAll,
      posParser: parser
    )
  of OptionalPositional, OptionalCatchAll:
    result.posParser = ParserAny[T](
      kind: OptionalCatchAll,
      posParser: parser
    )

func name*[T](b: sink CommandBuilder[T], name: string): CommandBuilder[T] =
  result = b
  result.cmdName = name

func name*[T](b: sink FlagBuilder[T], name: string): FlagBuilder[T] =
  result = b
  result.flagName = name

func name*[T](b: sink PositionalBuilder[T], name: string): PositionalBuilder[T] =
  result = b
  result.posName = name

func alias*[T](b: sink CommandBuilder[T], names: varargs[string]): CommandBuilder[T] =
  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's expected that users will
  # specify at most 4 of these.
  for name in names.items:
    if name == result.cmdName or name in result.aliases:
      continue
    result.aliases.add names

func alias*[T](b: sink FlagBuilder[T], names: varargs[string]): FlagBuilder[T] =
  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's expected that users will
  # specify at most 4 of these.
  for name in names.items:
    if name == result.flagName or name in result.aliases:
      continue
    result.aliases.add names

func parser*[T](
  b: sink CommandBuilder[T],
  p: sink CommandParser[T, Action],
): CommandBuilder[T] =
  result = b
  result.cmdParser = ParserAny[T](kind: ParserKind.Command, cmdParser: p)

func parser*[T](
  b: sink CommandBuilder[T],
  p: sink CommandParser[T, void],
): CommandBuilder[T] =
  b.parser(
    proc (command: Command, accumulator: var T): Action =
      p(command, accumulator)
  )

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink OptionalParser[T, Action],
): FlagBuilder[T] =
  result = b
  result.flagParser = ParserAny[T](kind: FlagOptionalValue, optParser: p)

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink OptionalParser[T, void],
): FlagBuilder[T] =
  b.optionalParser(
    proc (option: string, value: Option[string], accumulator: var T): Action =
      p(option, value, accumulator)
  )

func parser*[T](b: sink FlagBuilder[T], p: sink Parser[T, Action]): FlagBuilder[T] =
  result = b
  result.flagParser = ParserAny[T](kind: ParserKind.Flag, parser: p)

func parser*[T](b: sink FlagBuilder[T], p: sink Parser[T, void]): FlagBuilder[T] =
  b.parser(
    proc (option: string, value: string, accumulator: var T): Action =
      p(option, value, accumulator)
  )

func parser*[T](b: sink PositionalBuilder[T], p: sink PositionalParser[T, Action]): PositionalBuilder[T] =
  result = b
  case result.posParser.kind
  of ParserKind.Positional..OptionalCatchAll:
    result.posParser.posParser = p
  else:
    result.posParser = ParserAny[T](kind: ParserKind.Positional, posParser: p)

func parser*[T](b: sink PositionalBuilder[T], p: sink PositionalParser[T, void]): PositionalBuilder[T] =
  b.parser(
    proc (value: string, accumulator: var T): Action =
      p(value, accumulator)
  )

func optionalParser*[T, U; R: MaybeAction](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink OptionalTypedParser[T, U, R],
): FlagBuilder[T] =
  mixin parseCli

  when U is string:
    result = b.optionalParser(OptionalParser[T, R] parser)
  else:
    result = b.optionalParser(
      proc (option: string, value: Option[string], r: var T): Action =
        let value = value.map(proc (x: string): U = parseCli(U, x))
        parser(option, value, r)
    )

func parser*[T, U; R: MaybeAction](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink TypedParser[T, U, R],
): FlagBuilder[T] =
  mixin parseCli

  when U is bool:
    result = b.optionalParser(
      proc (option: string, value: Option[string], r: var T): R =
        parser(option, parseCli(U, value.get("true")), r)
    )

  elif U is string:
    result = b.parser(Parser[T, R] parser)

  else:
    result = b.parser(
      proc (option, value: string, r: var T): Action =
        parser(option, parseCli(U, value), r)
    )

func parser*[T, U; R: MaybeAction](
  b: sink PositionalBuilder[T],
  _: typedesc[U],
  parser: sink TypedPositionalParser[T, U, R],
): PositionalBuilder[T] =
  mixin parseCli

  when U is string:
    result = b.parser(PositionalParser[T, R] parser)

  else:
    result = b.parser(
      proc (value: string, r: var T): Action =
        parser(parseCli(U, value), r)
    )

func describe*[T](
  b: sink CommandBuilder[T],
  usage: sink string,
): CommandBuilder[T] =
  result = b
  result.usage = usage

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
  of ParserKind.Command: p.cmdParser == nil

func addCommon[T](
  cli: var Cli[T],
  parent: Parameter,
  name, usage, placeholder: sink string,
  parser: sink ParserAny[T]
): Parameter =
  result = cli.name.add(name)
  discard cli.usage.add(usage)
  discard cli.placeholder.add(placeholder)
  discard cli.parser.add(parser)
  discard cli.parent.add(parent)

func initCli*[T](b: sink CommandBuilder[T]): Cli[T] =
  result = Cli[T](
    command: default(typeof result.command),
    parser: default(typeof result.parser),
    name: default(typeof result.name),
    alias: default(typeof result.alias),
    parent: default(typeof result.parent),
    usage: default(typeof result.usage),
    placeholder: default(typeof result.placeholder),
  )

  assert b.cmdParser.isNil(), "Root parser cannot be non-nil"
  assert b.aliases == [], "Root parser cannot have aliases"

  discard result.addCommon(
    InvalidParameter,
    b.cmdName,
    b.usage,
    "",
    ParserAny[T](kind: ParserKind.Command)
  )
  result.command[RootCommand] = CliCommand()

func isDispatcher(cmd: CliCommand): bool =
  cmd.command.len > 0

func hasPositional(cmd: CliCommand): bool =
  not cmd.isDispatcher and cmd.positional.len > 0

func hasDefaultCommand(cmd: CliCommand): bool =
  cmd.isDispatcher and cmd.positional.len > 0

func addTo*[T](
  b: sink CommandBuilder[T],
  cli: var Cli[T],
  command: Command,
): Command {.discardable.} =
  assert b.cmdName.len > 0, "Command name must not be empty"
  if b.cmdName in cli.command[command].command:
    raise newException(ValueError, "Command '" & b.cmdName & "' already exists")
  for alias in b.aliases.items:
    assert alias != "", "Command alias cannot be empty"
    if alias in cli.command[command].command:
      raise newException(ValueError, "Command '" & alias & "' already exists")

  if b.cmdParser.isNil():
    b.cmdParser = ParserAny[T](kind: ParserKind.Command)
  assert b.cmdParser.kind == ParserKind.Command

  if cli.command[command].hasPositional:
    raise newException(ValueError, "Cannot add subcommand to command with positional parameters")

  if b.isDefault and cli.command[command].hasDefaultCommand:
    raise newException(ValueError, "Command already has a default subcommand registered")

  result = Command cli.addCommon(Parameter command, b.cmdName, b.usage, "", b.cmdParser)
  cli.command[result] = CliCommand()
  cli.command[command].command[b.cmdName] = result
  if b.isDefault:
    cli.command[command].positional.add(Parameter result)
  for alias in b.aliases.items:
    cli.command[command].command[alias] = result
  if b.aliases.len > 0:
    cli.alias[Parameter result] = b.aliases

func addTo*[T](
  b: sink FlagBuilder[T],
  cli: var Cli[T],
  command: Command = RootCommand,
): Flag {.discardable.} =
  assert b.flagName.len > 0, "Flag name must not be empty"
  if b.flagName in cli.command[command].flag:
    raise newException(ValueError, "Flag '" & b.flagName & "' already exists")
  for alias in b.aliases.items:
    assert alias != "", "Flag alias cannot be empty"
    if alias in cli.command[command].flag:
      raise newException(ValueError, "Flag '" & alias & "' already exists")

  assert not b.flagParser.isNil(), "Parser must be non-nil"
  assert b.flagParser.kind in {ParserKind.Flag, FlagOptionalValue}

  result = Flag cli.addCommon(Parameter command, b.flagName, b.usage, b.placeholder, b.flagParser)
  cli.command[command].flag[b.flagName] = Parameter result
  for alias in b.aliases.items:
    cli.command[command].flag[alias] = Parameter result
  if b.aliases.len > 0:
    cli.alias[Parameter result] = b.aliases

func addTo*[T](
  b: sink PositionalBuilder[T],
  cli: var Cli[T],
  command: Command = RootCommand,
): Positional {.discardable.} =
  assert b.posName.len > 0, "Positional name should not be empty"
  for param in cli.command[command].positional.items:
    if b.posName == cli.name[param]:
      raise newException(ValueError):
        "Positional with name '" & cli.name[param] & "' already exists"

  assert not b.posParser.isNil(), "Parser must be non-nil"
  assert b.posParser.kind in {ParserKind.Positional..OptionalCatchAll}

  if cli.command[command].isDispatcher:
    raise newException(ValueError, "Cannot add positional parameters: command is a dispatcher")

  if cli.command[command].hasPositional:
    let lastPos = cli.command[command].positional[^1]
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

  result = Positional cli.addCommon(Parameter command, b.posName, b.usage, placeholder = "", parser = b.posParser)
  cli.command[command].positional.add Parameter(result)

func flagWithName*(cli: Cli, command: Command, name: string): Option[Flag] =
  assert command in cli.command, "Invalid command"
  try: some(Flag cli.command[command].flag[name])
  except KeyError: none Flag

func commandWithName*(cli: Cli, command: Command, name: string): Option[Command] =
  assert command in cli.command, "Invalid command"
  try: some(Command cli.command[command].command[name])
  except KeyError: none Command

func nameOf*(cli: Cli, flag: Flag): lent string =
  cli.name[Parameter flag]

func nameOf*(cli: Cli, positional: Positional): lent string =
  cli.name[Parameter positional]

func nameOf*(cli: Cli, command: Command): lent string =
  cli.name[Parameter command]

iterator namesOf*(cli: Cli, flag: Flag): lent string =
  try:
    yield cli.name[Parameter flag]
    for name in cli.alias[Parameter flag].items:
      yield name
  except KeyError:
    discard "Flag has no aliases"

iterator namesOf*(cli: Cli, command: Command): lent string =
  try:
    yield cli.name[Parameter command]
    for name in cli.alias[Parameter command].items:
      yield name
  except KeyError:
    discard "Command has no aliases"

func parentOf*(cli: Cli, command: Command): Option[Command] =
  let parent = Command cli.parent[Parameter command]
  if Parameter(parent) == InvalidParameter:
    none Command
  else:
    some parent

func pathOf*(cli: Cli, command: Command): seq[Command] =
  result.add command

  var command = command
  while true:
    let parentOpt = cli.parentOf(command)
    if parentOpt.isNone():
      break
    let parent = parentOpt.unsafeGet()

    result.add parent
    command = parent

  reverse result

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

func usageOf*(cli: Cli, command: Command): lent string =
  cli.usage[Parameter command]

func usageOf*(cli: Cli, flag: Flag): lent string =
  cli.usage[Parameter flag]

func usageOf*(cli: Cli, positional: Positional): lent string =
  cli.usage[Parameter positional]

func placeholderOf*(cli: Cli, flag: Flag): string =
  cli.placeholder[Parameter flag]

func isDispatcher*(cli: Cli, command: Command): bool =
  cli.command[command].isDispatcher()

func hasDefaultCommand*(cli: Cli, command: Command): bool =
  cli.command[command].hasDefaultCommand()

func defaultCommandOf*(cli: Cli, command: Command): Option[Command] =
  if cli.command[command].hasDefaultCommand():
    some(Command cli.command[command].positional[0])
  else:
    none(Command)

func classify*(cli: Cli, param: Parameter): ParameterKind =
  case cli.parser[param].kind
  of ParserKind.Command: ParameterKind.Command
  of ParserKind.Flag, FlagOptionalValue: ParameterKind.Flag
  of ParserKind.Positional..OptionalCatchAll: ParameterKind.Positional

func isDefault*(cli: Cli, command: Command): bool =
  cli.parentOf(command)
    .flatMap(
      proc (parent: Command): Option[bool] =
        cli.defaultCommandOf(parent)
          .map(
            proc(x: Command): bool = x == command
          )
    )
    .get(otherwise = false)

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

proc addHelpFlag*[T](
  cli: var Cli[T],
  command: Command = RootCommand,
  name: sink string = "help",
  aliases: varargs[string] = []
): Flag {.discardable.} =
  cli.helpFlagBuilder
    .name(name)
    .alias(aliases)
    .addTo(cli, command)

func collectRemaining(lexer: var CmdLexer): seq[string] =
  for arg in lexer.remaining:
    result.add arg

func newHelpError(
  command: Command,
  paramName: sink string,
  param: Parameter,
  remaining: sink seq[string],
): ref HelpError {.raises: [].} =
  (ref HelpError)(
    msg: "help requested",
    command: command,
    paramName: paramName,
    param: param,
    remaining: remaining,
  )

func newUnknownFlagError(
  command: Command,
  flagName: sink string,
  flagValue: sink Option[string],
  remaining: sink seq[string],
): ref UnknownFlagError {.raises: [].} =
  (ref UnknownFlagError)(
    msg: "unexpected flag '" & flagName & "'",
    command: command,
    flagName: flagName,
    flagValue: flagValue,
    remaining: remaining,
  )

func newMissingValueError(
  command: Command,
  flagName: sink string,
  flag: Flag,
  remaining: sink seq[string],
): ref MissingValueError {.raises: [].} =
  (ref MissingValueError)(
    msg: "missing value for flag '" & flagName & "'",
    command: command,
    flagName: flagName,
    flag: flag,
    remaining: remaining,
  )

func newInvalidValueError(
  command: Command,
  parent: ref ValueError,
  flagName: sink string,
  flag: Flag,
  value: sink Option[string],
  remaining: sink seq[string],
): ref InvalidValueError {.raises: [].} =
  (ref InvalidValueError)(
    msg: "invalid value for flag '" & flagName & "': " & $value,
    command: command,
    flagName: flagName,
    flagValue: value,
    flag: flag,
    parent: parent,
    remaining: remaining,
  )

func newUnknownPositionalError(
  command: Command,
  positionalValue: sink string,
  remaining: sink seq[string],
): ref UnknownPositionalError {.raises: [].} =
  (ref UnknownPositionalError)(
    msg: "unexpected parameter: " & positionalValue,
    command: command,
    positionalValue: positionalValue,
    remaining: remaining,
  )

func newInvalidPositionalError(
  command: Command,
  parent: ref ValueError,
  value: sink string,
  positional: Positional,
  remaining: sink seq[string],
): ref InvalidPositionalError {.raises: [].} =
  (ref InvalidPositionalError)(
    msg: "invalid parameter: " & $value,
    command: command,
    positionalValue: value,
    positional: positional,
    parent: parent,
    remaining: remaining,
  )

func newMissingPositionalError(
  command: Command,
  positional: Positional
): ref MissingPositionalError {.raises: [].} =
  (ref MissingPositionalError)(
    msg: "missing required value for positional parameter",
    command: command,
    positional: positional,
  )

func newUnknownCommandError(
  command: Command,
  value: sink string,
  remaining: sink seq[string],
): ref UnknownCommandError {.raises: [].} =
  (ref UnknownCommandError)(
    msg: "unknown command: " & $value,
    command: command,
    commandName: value,
    remaining: remaining,
  )

func newInvalidCommandError(
  command: Command,
  parent: ref ValueError,
  value: sink string,
  target: Command,
  remaining: sink seq[string],
): ref InvalidCommandError {.raises: [].} =
  (ref InvalidCommandError)(
    msg: "invalid command: " & $value,
    command: command,
    commandName: value,
    targetCommand: target,
    parent: parent,
    remaining: remaining,
  )

func newMissingCommandError(
  command: Command,
): ref MissingCommandError {.raises: [].} =
  (ref MissingCommandError)(
    msg: "missing command",
    command: command,
  )

func parseNext[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
) {.tailcall.}

func performAction[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  input: sink string,
  parameter: Parameter,
  action: Action,
) {.tailcall.} =
  if action == ShowHelp:
    raise newHelpError(
      ctx.command, input, parameter, collectRemaining ctx.lexer
    )

  drop input
  case action
  of Continue: discard
  of DisableFlagProcessing:
    ctx.isValueOnly = true
  of ShowHelp: unreachable()

  parseNext(ctx, cli, accumulator)

func switchCommand(ctx: var ParseContext, command: Command) =
  ctx.command = command
  ctx.nextPositional = 0
  ctx.positionalCount = 0

func parsePositional[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  value: sink string,
) {.tailcall.} =
  let posLen = block: cli.command[ctx.command].positional.len
  if ctx.nextPositional < posLen:
    let
      posId = block: cli.command[ctx.command].positional[ctx.nextPositional]
      parser = cli.parser[posId]
      action =
        try:
          parser.posParser(value, accumulator)
        except ValueError as e:
          raise newInvalidPositionalError(
            ctx.command,
            e,
            value,
            Positional posId,
            collectRemaining ctx.lexer
          )

    inc ctx.positionalCount
    inc ctx.nextPositional, ord(parser.kind notin {CatchAll, OptionalCatchAll})

    performAction(ctx, cli, accumulator, value, posId, action)

  else:
    raise newUnknownPositionalError(
      ctx.command, value, collectRemaining ctx.lexer
    )

func parseCommand[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  command: Command,
  input: sink string,
) {.tailcall.} =
  let
    parser = cli.parser[Parameter command]
    action =
      try:
        if parser.cmdParser != nil:
          parser.cmdParser(command, accumulator)
        else:
          Continue
      except ValueError as e:
        raise newInvalidCommandError(
          ctx.command,
          e,
          input,
          command,
          collectRemaining ctx.lexer,
        )

  ctx.switchCommand(command)

  performAction(ctx, cli, accumulator, input, Parameter command, action)

func parseCommand[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  value: sink string,
) {.tailcall.} =
  let cmd =
    try: cli.command[ctx.command].command[value]
    except KeyError:
      raise newUnknownCommandError(
        ctx.command,
        value,
        collectRemaining ctx.lexer
      )

  parseCommand(ctx, cli, accumulator, cmd, value)

func parseFlag[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  kind: CmdlineKind,
  option: sink string,
) {.tailcall.} =
  let flagId =
    try: cli.command[ctx.command].flag[option]
    except KeyError:
      raise newUnknownFlagError(
        ctx.command,
        option,
        ctx.lexer.value(delimitedOnly = true),
        collectRemaining ctx.lexer
      )

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
            ctx.command,
            option,
            Flag flagId,
            collectRemaining ctx.lexer
          )

        parser.parser(option, optValue.unsafeGet(), accumulator)
      else:
        unreachable()
    except ValueError as e:
      raise newInvalidValueError(
        ctx.command,
        e,
        option,
        Flag flagId,
        optValue,
        collectRemaining ctx.lexer
      )

  performAction(ctx, cli, accumulator, option, flagId, action)

func parseNext[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T
) {.tailcall.} =
  let (kind, option) =
    if not ctx.isValueOnly:
      ctx.lexer.next()
    else:
      ctx.lexer
        .value()
        .map(
          proc (value: string): (CmdlineKind, string) =
            (cmdValue, value)
        )
        .get(otherwise = (cmdEnd, ""))

  case kind
  of cmdLong, cmdShort:
    parseFlag(ctx, cli, accumulator, kind, option)
  of cmdValue:
    if not ctx.isValueOnly and option == "--":
      ctx.isValueOnly = true
      drop option
      parseNext(ctx, cli, accumulator)
    elif (block: cli.command[ctx.command].isDispatcher):
      parseCommand(ctx, cli, accumulator, option)
    else:
      parsePositional(ctx, cli, accumulator, option)
  of cmdEnd:
    # Verify that we collected all required parameters
    # TODO: remove this copy once tables return lent T
    let currentCommand = cli.command[ctx.command]
    if currentCommand.hasDefaultCommand:
      let cmd = Command currentCommand.positional[0]

      drop currentCommand
      drop option
      parseCommand(ctx, cli, accumulator, cmd, "")
    elif currentCommand.isDispatcher:
      raise newMissingCommandError(ctx.command)
    elif ctx.nextPositional < currentCommand.positional.len:
      let posId = currentCommand.positional[ctx.nextPositional]
      case cli.parser[posId].kind
      of ParserKind.Positional:
        raise newMissingPositionalError(ctx.command, Positional posId)
      of CatchAll:
        # Catch all hasn't collected any parameters
        if ctx.positionalCount <= ctx.nextPositional:
          raise newMissingPositionalError(ctx.command, Positional posId)
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
    command: RootCommand,
  )

  try: parseNext(ctx, cli, accumulator)
  except KeyError, UnexpectedValueError:
    unreachable()

func parse*[T](
  cli: Cli[T],
  args: sink seq[string],
): T {.inline, raises: [ParseError].} =
  parse(cli, result, args)

iterator flags*(cli: Cli, command: Command): Flag =
  # Not the most efficient, but CLIs shouldn't be big enough for this to be an
  # issue
  for i in 0 ..< cli.parser.nextId.int:
    if cli.parent[Parameter i] == Parameter(command) and
      cli.parser[Parameter i].kind in {ParserKind.Flag, FlagOptionalValue}:
      yield Flag(i)

iterator positionals*(cli: Cli, command: Command): Positional =
  if not cli.command[command].isDispatcher:
    for i in cli.command[command].positional.items:
      yield Positional(i)

iterator subcommands*(cli: Cli, command: Command): Command =
  for (param, parent) in cli.parent.pairs:
    if parent == Parameter(command) and
      cli.parser[param].kind == ParserKind.Command:
      yield Command(param)

func flagsUsage*(cli: Cli, command: Command): string =
  var lines: seq[(string, string)]
  var flagPad: int
  for flag in cli.flags(command):
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

func positionalsUsage*(cli: Cli, command: Command): string =
  var
    lines: seq[(string, string)]
    posPad: int

  for positional in cli.positionals(command):
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

func commandUsage*(cli: Cli, command: Command, rootName: string = cli.nameOf(RootCommand)): string =
  for idx, command in cli.pathOf(command).pairs():
    if idx == 0:
      result.add rootName
    else:
      if result.len > 0:
        result.add ' '
      result.add cli.nameOf(command)

  if result.len > 0:
    result.add ' '
  result.add "[OPTIONS]"

  if cli.hasDefaultCommand(command):
    result.add " [COMMAND]"
  elif cli.isDispatcher(command):
    result.add " <COMMAND>"

  for positional in cli.positionals(command):
    if result.len > 0:
      result.add ' '
    result.add cli.displayOf(positional)

func subcommandsUsage*(cli: Cli, command: Command): string =
  var
    commands: seq[Command]
    cmdPad: int
  for command in cli.subcommands(command):
    cmdPad = max(cmdPad, cli.nameOf(command).len)
    commands.add command

  cmdPad.inc 2
  for command in commands.items:
    var usage = cli.usageOf(command)
    if cli.isDefault(command):
      if usage.len > 0: usage.add ' '
      usage.add "[default]"

    if result.len > 0: result.add "\n"
    result.add "  "
    result.add:
      alignLeft(cli.nameOf(command)):
        if usage.len > 0: cmdPad else: 0
    result.add usage

func help*(cli: Cli, command: Command, rootName: string = cli.nameOf(RootCommand)): string =
  let
    usage = cli.commandUsage(command, rootName)
    subcmds = cli.subcommandsUsage(command)
    args = cli.positionalsUsage(command)
    flags = cli.flagsUsage(command)

  result.add cli.usageOf(command)

  if result.len > 0: result.add "\n\n"
  result.add "Usage: "
  result.add usage

  if subcmds.len > 0:
    result.add "\n\nCommands:\n"
    result.add subcmds

  if args.len > 0:
    result.add "\n\nArguments:\n"
    result.add args

  if flags.len > 0:
    result.add "\n\nOptions:\n"
    result.add flags

func prettifyError*[T](cli: Cli[T], error: ref ParseError): string =
  func prefixedFlag(name: string): string =
    if name.len > 1:
      "--" & name
    elif name == "-":
      "---" # The only unambiguous form of this flag
    else:
      "-" & name

  func formatFlag(cli: Cli[T], flag: Flag, name: string): string =
    let
      placeholder = cli.placeholderOf(flag)
      suffix =
        if not cli.isValueOptional(flag):
          if placeholder != "":
            " <" & placeholder & ">"
          else:
            " <VALUE>"
        else:
          if placeholder == "":
            ""
          else:
            "[=<" & placeholder & ">]"

    prefixedFlag(name) & suffix

  if error of UnknownFlagError:
    let error = (ref UnknownFlagError)(error)
    "unexpected flag '" & prefixedFlag(error.flagName) & "' found"
  elif error of MissingValueError:
    let error = (ref MissingValueError)(error)
    "a value is required for '" & cli.formatFlag(error.flag, error.flagName) & "' but none was supplied"
  elif error of InvalidValueError:
    let
      error = (ref InvalidValueError)(error)
      msg =
        if error.parent != nil:
          ": " & error.parent.msg
        else:
          ""
    "invalid value for '" & cli.formatFlag(error.flag, error.flagName) & "'" & msg
  elif error of FlagError:
    let error = (ref FlagError)(error)
    "could not parse flag '" & prefixedFlag(error.flagName) & "': " & error.msg
  elif error of UnknownPositionalError:
    let error = (ref UnknownPositionalError)(error)
    "unexpected positional argument '" & error.positionalValue & "' found"
  elif error of MissingPositionalError:
    let error = (ref MissingPositionalError)(error)
    "missing required positional argument: " & cli.displayOf(error.positional)
  elif error of InvalidPositionalError:
    let
      error = (ref InvalidPositionalError)(error)
      msg =
        if error.parent != nil:
          ": " & error.parent.msg
        else:
          ""
    "invalid value for '" & cli.displayOf(error.positional) & "'" & msg
  elif error of PositionalError:
    let error = (ref PositionalError)(error)
    "could not parse positional parameter '" & error.positionalValue & "': " & error.msg
  elif error of UnknownCommandError:
    let error = (ref UnknownCommandError)(error)
    "unrecognized subcommand: " & error.commandName
  elif error of MissingCommandError:
    "missing subcommand"
  elif error of InvalidCommandError:
    let
      error = (ref InvalidCommandError)(error)
      msg =
        if error.parent != nil:
          ": " & error.parent.msg
        else:
          ""
    "invalid subcommand '" & error.commandName & "'" & msg
  elif error of CommandError:
    let error = (ref CommandError)(error)
    "could not parse subcommand '" & error.commandName & "': " & error.msg
  elif error of HelpError:
    "help requested"
  else:
    "unexpected error parsing command line: " & error.msg

proc run*[T](
  cli: Cli[T],
  accumulator: var T,
  args: sink seq[string] = commandLineParams(),
  messageOutput: File = stdmsg,
) =
  try:
    parse(cli, accumulator, args)
  except HelpError as e:
    messageOutput.writeLine(cli.help(e.command))
    quit 0
  except ParseError as e:
    messageOutput.writeLine("error: ", cli.prettifyError(e))
    messageOutput.writeLine("\nUsage: ", cli.commandUsage(e.command))
    quit 1

proc run*[T](
  cli: Cli[T],
  args: sink seq[string] = commandLineParams(),
  messageOutput: File = stdmsg,
): T =
  run(cli, result, args, messageOutput)
