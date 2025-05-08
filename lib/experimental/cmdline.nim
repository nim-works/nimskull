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
  Cli*[T] {.requiresInit.} = object
    ## A command line parser.
    ##
    ## See also: `initCli <#initCli%2CsinkCommandBuilder[T]>`_
    command: Table[Command, CliCommand] ## Lookup mapping of command to lookup tables.
    parser: Store[Parameter, ParserAny[T]] ## Parser to process input for Parameter.
    name: Store[Parameter, string] ## Canonical names for all Parameters.
    alias: Table[Parameter, seq[string]] ## Mapping of Parameter to aliases.
    parent: Store[Parameter, Parameter] ## Mapping of Parameter to their parent.

    # Documentation storage
    #
    # Might be useful to support not having these for space-constrained
    # targets using a define.
    usage: Store[Parameter, string] ## Canonical usage for Parameters.
    placeholder: Store[Parameter, string] ## Canonical placeholder for
                                          ## Parameters. Only used for flags.

  CliCommand = object
    ## Lookup table for flags and positionals.
    flag: Table[string, Parameter] ## Lookup mapping of flag names to Parameter.
    command: Table[string, Command] ## Lookup mapping of command names to Command.
    positional: seq[Parameter] ## Lookup mapping of position to Parameter.

  FlagBuilder*[T] = object
    ## Builder for command line flags.
    ##
    ## See also: `flagBuilder <#flagBuilder%2CCli[T]>`_
    flagParser: ParserAny[T]
    flagName: string
    aliases: seq[string]
    usage: string
    placeholder: string

  PositionalBuilder*[T] = object
    ## Builder for command line positional parameters.
    ##
    ## See also: `positionalBuilder <#positionalBuilder%2CCli[T]>`_
    posParser: ParserAny[T]
    posName: string
    usage: string

  CommandBuilder*[T] = object
    ## Builder for command line subcommands.
    ##
    ## See also: `commandBuilder <#commandBuilder%2CCli[T]>`_
    cmdParser: ParserAny[T]
    cmdName: string
    aliases: seq[string]
    usage: string
    isDefault: bool

  Action* {.pure.} = enum
    ## Action to be taken after parsing.
    Continue ## Continue parameter parsing.
    ShowHelp ## Abort and show help message.
    DisableFlagProcessing ## Parameters following this will be considered
                          ## to be values.

  MaybeAction* = Action | void
    ## Typeclass to support parsers returning either `Action` or nothing.

  Parser*[T; R: MaybeAction] = proc (option, value: string, accumulator: var T): R
    ## A parser for flag with name `option` and value `value`. The accumulator
    ## passed to `run()`_ or `parse()`_ can be accessed and modified via
    ## `accumulator`.
    ##
    ## A parser may choose to return an `Action` or not return anything,
    ## which will be interpreted as `Action.Continue`.
    ##
    ## A parser may raise `ValueError` for invalid inputs, which will be
    ## processed accordingly by the library. Any other exceptions signify
    ## an internal error and will not be handled automatically.
    ##
    ## .. _run(): #run%2CCli[T]%2CT%2Csinkseq[string]%2CFile
    ## .. _parse(): #parse%2CCli[T]%2CT%2Csinkseq[string]

  OptionalParser*[T; R: MaybeAction] = proc (option: string, value: Option[string], accumulator: var T): R
    ## A parser for flag with name `option` and optional value `value`. The
    ## accumulator passed to `run()`_ or `parse()`_ can be accessed and
    ## modified via `accumulator`.
    ##
    ## The parser may choose to return an `Action` or not return anything,
    ## which will be interpreted as `Action.Continue`.
    ##
    ## The parser may raise `ValueError` for invalid inputs, which will be
    ## processed accordingly by the library. Any other exceptions signify
    ## an internal error and will not be handled automatically.
    ##
    ## .. _run(): #run%2CCli[T]%2CT%2Csinkseq[string]%2CFile
    ## .. _parse(): #parse%2CCli[T]%2CT%2Csinkseq[string]

  PositionalParser*[T; R: MaybeAction] = proc (value: string, accumulator: var T): R
    ## A parser for positional parameter with value `value`. The accumulator
    ## passed to `run()`_ or `parse()`_ can be accessed and modified via
    ## `accumulator`.
    ##
    ## The parser may choose to return an `Action` or not return anything,
    ## which will be interpreted as `Action.Continue`.
    ##
    ## The parser may raise `ValueError` for invalid inputs, which will be
    ## processed accordingly by the library. Any other exceptions signify
    ## an internal error and will not be handled automatically.
    ##
    ## .. _run(): #run%2CCli[T]%2CT%2Csinkseq[string]%2CFile
    ## .. _parse(): #parse%2CCli[T]%2CT%2Csinkseq[string]

  CommandParser*[T; R: MaybeAction] = proc (command: Command, accumulator: var T): R
    ## A parser for command parameter with command `command`. The accumulator
    ## passed to `run()`_ or `parse()`_ can be accessed and modified via
    ## `accumulator`.
    ##
    ## The parser may choose to return an `Action` or not return anything,
    ## which will be interpreted as `Action.Continue`.
    ##
    ## The parser may raise `ValueError` for invalid inputs, which will be
    ## processed accordingly by the library. Any other exceptions signify
    ## an internal error and will not be handled automatically.
    ##
    ## .. _run(): #run%2CCli[T]%2CT%2Csinkseq[string]%2CFile
    ## .. _parse(): #parse%2CCli[T]%2CT%2Csinkseq[string]

  TypedParser*[T; U; R: MaybeAction] = proc (option: string, value: U, accumulator: var T): R
    ## Typed variant of `Parser <#Parser>`_.

  OptionalTypedParser*[T; U; R: MaybeAction] = proc (option: string, value: Option[U], accumulator: var T): R
    ## Typed variant of `OptionalParser <#OptionalParser>`_.

  TypedPositionalParser*[T; U; R: MaybeAction] = proc (value: U, accumulator: var T): R
    ## Typed variant of `PositionalParser <#PositionalParser>`_.

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
    ## Parser internal state.
    lexer: CmdLexer ## Lexer driving the parse.
    nextPositional: Natural ## Next positional parser to use.
    positionalCount: Natural ## Number of positionals parsed.
    command: Command ## The active command.
    isValueOnly: bool ## Whether flags are ignored.

  ParseError* = object of CatchableError
    ## An error during command line parsing.
    command*: Command ## Active command during error.
    remaining*: seq[string] ## Parameters that were not parsed.

  FlagError* = object of ParseError
    ## An error parsing flags.
    flagName*: string ## Name of the flag causing the error, as specified by
                      ## input
  UnknownFlagError* = object of FlagError
    ## The flag parsed was not recognized.
    flagValue*: Option[string] ## Inline value of flag causing error
  MissingValueError* = object of FlagError
    ## The flag parsed requires a value but was not provided.
    flag*: Flag ## Handle to the flag
  InvalidValueError* = object of FlagError
    ## Invalid value passed to a flag.
    ##
    ## The `ValueError` causing this can be found in the `parent` field.
    flagValue*: Option[string] ## The string value received. This should always
                               ## be `some(string)` for flags with non-optional
                               ## value.
    flag*: Flag ## Handle to the flag.

  PositionalError* = object of ParseError
    ## An error parsing positional parameters.
    positionalValue*: string ## Input value causing the error
  UnknownPositionalError* = object of PositionalError
    ## The positional parsed was not recognized.
  MissingPositionalError* = object of PositionalError
    ## A required positional parameter is missing from input.
    positional*: Positional ## Handle to the positional
  InvalidPositionalError* = object of PositionalError
    ## The positional parsed was invalid.
    ##
    ## The `ValueError` causing this can be found in the `parent` field.
    positional*: Positional ## Handle to the positional

  CommandError* = object of ParseError
    ## An error parsing command.
    commandName*: string ## Input value causing the error.
  UnknownCommandError* = object of CommandError
    ## The command parsed was not recognized.
  MissingCommandError* = object of CommandError
    ## A required command is missing from input.
  InvalidCommandError* = object of CommandError
    ## The command parsed was rejected by parser.
    ##
    ## The `ValueError` causing this can be found in the `parent` field.
    targetCommand*: Command ## Handle to the target command

  HelpError* = object of ParseError
    ## Help was requested using `Action.ShowHelp`.
    paramName*: string ## Name of the parameter triggering help, as specified by
                       ## input.
    param*: Parameter ## Handle to the parameter triggering help.

  ParameterKind* {.pure.} = enum
    Command
    Flag
    Positional

  Parameter* = distinct uint32
    ## A command line parameter. Values of this type are tied to the originating
    ## `Cli` instance.
    ##
    ## A `Parameter` might be a `Command`, `Flag`, or `Positional`. The
    ## `classify` function can be used to distinguish between them.

  Command* = distinct Parameter
    ## A command line subcommand. Values of this type are tied to the
    ## originating `Cli` instance.

  Flag* = distinct Parameter
    ## A command line flag. Values of this type are tied to the originating
    ## `Cli` instance.

  Positional* = distinct Parameter
    ## A command line positional parameter. Values of this type are tied to the
    ## originating `Cli` instance.

const
  InvalidParameter = high(Parameter)
    ## An invalid parameter
  RootCommand* = Command(0)
    ## The top-level command of `Cli`

proc hash(x: Parameter): Hash {.borrow.}

proc `==`*(a, b: Parameter): bool {.borrow.}
proc `==`*(a, b: Command): bool {.borrow.}
proc `==`*(a, b: Flag): bool {.borrow.}
proc `==`*(a, b: Positional): bool {.borrow.}

# FIXME: Put this in system.nim
func drop[T](_: sink T) = discard

func commandBuilder*[T](cli: Cli[T]): CommandBuilder[T] =
  ## Create a new `CommandBuilder`.
  result = CommandBuilder[T]()

func commandBuilder*(T: typedesc): CommandBuilder[T] =
  ## Create a new `CommandBuilder`.
  result = CommandBuilder[T]()

func flagBuilder*[T](cli: Cli[T]): FlagBuilder[T] =
  ## Create a new `FlagBuilder`.
  result = FlagBuilder[T]()

func positionalBuilder*[T](cli: Cli[T]): PositionalBuilder[T] =
  ## Create a new `PositionalBuilder`.
  result = PositionalBuilder[T]()

func name*[T](b: sink CommandBuilder[T], name: string): CommandBuilder[T] =
  ## Set the canonical name of this command. This name is used to identify
  ## the command on the command line.
  ##
  ## .. note::
  ##   A name is optional for the root command.
  ##
  ## See also: `alias <#alias%2CsinkCommandBuilder[T]%2Cvarargs[string]>`_
  result = b
  result.cmdName = name

func name*[T](b: sink FlagBuilder[T], name: string): FlagBuilder[T] =
  ## Set the canonical name of this flag. This name is used to identify
  ## this flag on the command line.
  ##
  ## If `name` is one-character long, it can also be recognized on the command
  ## line using the short form syntax (e.g. `-n`).
  ##
  ## See also: `alias <#alias%2CsinkFlagBuilder[T]%2Cvarargs[string]>`_
  result = b
  result.flagName = name

func name*[T](b: sink PositionalBuilder[T], name: string): PositionalBuilder[T] =
  ## Set the canonical name of this positional.
  result = b
  result.posName = name

func alias*[T](b: sink CommandBuilder[T], names: varargs[string]): CommandBuilder[T] =
  ## Set aliases for this command.
  ##
  ## This command can then be matched using any of the provided `names` in
  ## addition to its canonical name.
  ##
  ## See also: `name <#name%2CsinkCommandBuilder[T]%2Cstring>`_
  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's expected that users will
  # specify at most 4 of these.
  for name in names.items:
    if name == result.cmdName or name in result.aliases:
      continue
    result.aliases.add names

func alias*[T](b: sink FlagBuilder[T], names: varargs[string]): FlagBuilder[T] =
  ## Set aliases for this flag.
  ##
  ## The command can be matched using any of the provided `names` in addition to
  ## its canonical name.
  ##
  ## See also: `name <#name%2CsinkFlagBuilder[T]%2Cstring>`_
  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's expected that users will
  # specify at most 4 of these.
  for name in names.items:
    if name == result.flagName or name in result.aliases:
      continue
    result.aliases.add names

func default*[T](b: sink CommandBuilder[T]): CommandBuilder[T] =
  ## Mark this command as the default subcommand of the current dispatcher.
  ## When no subcommand are specified on the command line, this command
  ## will be selected.
  ##
  ## Only one default subcommand is permitted per command. Attempting to
  ## add more than one is considered an error.
  ##
  ## This attribute is ignored for the root command.
  result = b
  result.isDefault = true

func catchAll*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  ## Mark this positional as "catch all". All positional parameters encountered
  ## starting at this positional will be handled by the associated parser.
  ##
  ## A catch all positional can only be added as the last parameter of a
  ## command. No other positional parameters might be added after this.
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

func optional*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  ## Mark this positional as optional. When not specified on the command line,
  ## the associated parser will not be called.
  ##
  ## No non-optional positional parameters might be added to a command after the
  ## first optional.
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

func parser*[T](
  b: sink CommandBuilder[T],
  p: sink CommandParser[T, Action],
): CommandBuilder[T] =
  ## Set the parser for this command.
  ##
  ## .. note::
  ##   A parser is optional for commands.
  ##
  ## .. warning::
  ##   It is an error to set a parser for the root command.
  result = b
  result.cmdParser = ParserAny[T](kind: ParserKind.Command, cmdParser: p)

func parser*[T](
  b: sink CommandBuilder[T],
  p: sink CommandParser[T, void],
): CommandBuilder[T] {.inline.} =
  ## Set the parser for this command with `Action.Continue` as the default
  ## action.
  ##
  ## .. note::
  ##   A parser is optional for commands.
  ##
  ## .. warning::
  ##   It is an error to set a parser for the root command.
  b.parser(
    proc (command: Command, accumulator: var T): Action =
      p(command, accumulator)
  )

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink OptionalParser[T, Action],
): FlagBuilder[T] =
  ## Set the parser for this flag, and mark the flag as not requiring any value.
  ##
  ## Flags with optional value will only receive their value when it is
  ## specified inline, for example: `--flag=value` or `--flag:value`.
  result = b
  result.flagParser = ParserAny[T](kind: FlagOptionalValue, optParser: p)

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink OptionalParser[T, void],
): FlagBuilder[T] =
  ## Set the parser for this flag, and mark the flag as not requiring any value.
  ## On successful parse, `Action.Continue` is taken as the default action.
  ##
  ## Flags with optional value will only receive their value when it is
  ## specified inline, for example: `--flag=value` or `--flag:value`.
  b.optionalParser(
    proc (option: string, value: Option[string], accumulator: var T): Action =
      p(option, value, accumulator)
  )

func parser*[T](b: sink FlagBuilder[T], p: sink Parser[T, Action]): FlagBuilder[T] =
  ## Set the parser for this flag, and mark the flag as requiring values.
  ##
  ## Flags with required value can receive any of the following forms:
  ##
  ## - `--flag=value`
  ## - `--flag:value`
  ## - `--flag value`
  result = b
  result.flagParser = ParserAny[T](kind: ParserKind.Flag, parser: p)

func parser*[T](b: sink FlagBuilder[T], p: sink Parser[T, void]): FlagBuilder[T] =
  ## Set the parser for this flag, and mark the flag as requiring values.
  ## On successful parse, `Action.Continue` is taken as the default action.
  ##
  ## Flags with required value can receive any of the following forms:
  ##
  ## - `--flag=value`
  ## - `--flag:value`
  ## - `--flag value`
  b.parser(
    proc (option: string, value: string, accumulator: var T): Action =
      p(option, value, accumulator)
  )

func optionalParser*[T, U; R: MaybeAction](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink OptionalTypedParser[T, U, R],
): FlagBuilder[T] =
  ## Set the parser for this flag, and mark the flag as not requiring any value.
  ##
  ## Input string values from the command line will first be parsed using
  ## `parseCli` before handing off to the parser. See `parsers` module for
  ## more information.
  ##
  ## Flags with optional value will only receive their value when it is
  ## specified inline, for example: `--flag=value` or `--flag:value`.
  mixin parseCli

  when U is string:
    result = b.optionalParser(OptionalParser[T, R] parser)
  else:
    result = b.optionalParser(
      proc (option: string, value: Option[string], r: var T): Action =
        let value = value.map(proc (x: string): U = parseCli(U, x))
        parser(option, value, r)
    )

proc parser*[T, U; R: MaybeAction](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink TypedParser[T, U, R],
): FlagBuilder[T] =
  ## Set the parser for this flag, and mark the flag as requiring values.
  ##
  ## Input string values from the command line will first be parsed using
  ## `parseCli` before handing off to the parser. See `parsers` module for
  ## more information.
  ##
  ## Flags with required value can receive any of the following forms:
  ##
  ## - `--flag=value`
  ## - `--flag:value`
  ## - `--flag value`
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

func parser*[T](
  b: sink PositionalBuilder[T],
  p: sink PositionalParser[T, Action]
): PositionalBuilder[T] =
  ## Set the parser for this positional parameter.
  result = b
  case result.posParser.kind
  of ParserKind.Positional..OptionalCatchAll:
    result.posParser.posParser = p
  else:
    result.posParser = ParserAny[T](kind: ParserKind.Positional, posParser: p)

func parser*[T](
  b: sink PositionalBuilder[T],
  p: sink PositionalParser[T, void]
): PositionalBuilder[T] =
  ## Set the parser for this positional parameter.
  ## On successful parse, `Action.Continue` is taken as the default action.
  b.parser(
    proc (value: string, accumulator: var T): Action =
      p(value, accumulator)
  )

proc parser*[T, U; R: MaybeAction](
  b: sink PositionalBuilder[T],
  _: typedesc[U],
  parser: sink TypedPositionalParser[T, U, R],
): PositionalBuilder[T] =
  ## Set the parser for this positional parameter.
  ##
  ## Input string values from the command line will first be parsed using
  ## `parseCli` before handing off to the parser. See `parsers` module for
  ## more information.
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
  ## Set a short `usage` description for this command.
  ##
  ## .. note::
  ##
  ##   `usage` should only be one-line long. This is not enforced, but
  ##   the built-in documentation renderer assumes this and might not produce
  ##   satisfactory results with multi-line `usage`.
  result = b
  result.usage = usage

func describe*[T](
  b: sink FlagBuilder[T],
  usage: sink string,
  placeholder: sink string = "",
): FlagBuilder[T] =
  ## Set a short `usage` description and a value `placeholder` for this flag.
  ##
  ## For required flags, an empty placeholder defaults to `VALUE`. For optional
  ## flags, an empty placeholder will be omitted from documentation rendering.
  ##
  ## .. note::
  ##
  ##   `usage` should only be one-line long. This is not enforced, but
  ##   the built-in documentation renderer assumes this and might not produce
  ##   satisfactory results with multi-line `usage`.
  result = b
  result.usage = usage
  result.placeholder = placeholder

func describe*[T](
  b: sink PositionalBuilder[T],
  usage: sink string,
): PositionalBuilder[T] =
  ## Set a short `usage` description and a value `placeholder` for this flag.
  ##
  ## For required flags, an empty placeholder defaults to `VALUE`. For optional
  ## flags, an empty placeholder will be omitted from documentation rendering.
  ##
  ## .. note::
  ##
  ##   `usage` should only be one-line long. This is not enforced, but
  ##   the built-in documentation renderer assumes this and might not produce
  ##   satisfactory results with multi-line `usage`.
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
  ## Add a new parameter to `cli`.
  ##
  ## .. important::
  ##   Input parameters are assumed to meet operational constraints.
  result = cli.name.add(name)
  discard cli.usage.add(usage)
  discard cli.placeholder.add(placeholder)
  discard cli.parser.add(parser)
  discard cli.parent.add(parent)

func initCli*[T](b: sink CommandBuilder[T]): Cli[T] =
  ## Creates a new `Cli`, with `b` used to construct the root command.
  ##
  ## The root command must:
  ##
  ## - Have no `parser` set.
  ## - Have no `aliases`.
  ##
  ## Unlike subcommands, the root command may:
  ##
  ## - Have no `name` set. The `name` of a root command is used for
  ##   documentation generation and does not participate in parsing.
  ##
  ## The root command can be referred to using `RootCommand` constant.
  ##
  ## See also: `commandBuilder <#commandBuilder%2Ctypedesc>`_
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
  ## Add the command specified by `b` as a subcommand of `command`.
  ##
  ## A subcommand can only be added to `command` if:
  ##
  ## - `command` does not contain any positional parameters.
  ## - A non-empty `name` must be set.
  ## - None of the names specified by either `name` or `alias` are added to
  ##   `command` previously.
  ##
  ## `ValueError` will be raised if any of the specified constraints are
  ## violated.
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
  ## Add the flag specified by `b` to `command`.
  ##
  ## A flag can only be added to `command` if:
  ##
  ## - A non-empty `name` must be set.
  ## - None of the names specified by either `name` or `alias` are added to
  ##   `command` previously.
  ## - A `parser` or `optionalParser` must be set.
  ##
  ## `ValueError` will be raised if any of the specified constraints are
  ## violated.
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
  ## Add the positional parameter specified by `b` to `command`.
  ##
  ## A positional parameter can only be added to `command` if:
  ##
  ## - `command` does not contain any subcommands.
  ## - A non-empty `name` must be set and must not have been added to `command`
  ##   previously.
  ## - A `parser` must be set.
  ## - If non-optional, must be added prior to any optional parameters.
  ## - If catch all, must be added after all other parameters.
  ##
  ## `ValueError` will be raised if any of the specified constraints are
  ## violated.
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
  ## Returns the `Flag` handle for the flag identifiable by `name` registered
  ## for `command`.
  assert command in cli.command, "Invalid command"
  try: some(Flag cli.command[command].flag[name])
  except KeyError: none Flag

func commandWithName*(cli: Cli, command: Command, name: string): Option[Command] =
  ## Returns the `Command` handle for the subcommand identifiable by `name`
  ## registered for `command`.
  assert command in cli.command, "Invalid command"
  try: some(Command cli.command[command].command[name])
  except KeyError: none Command

func nameOf*(cli: Cli, flag: Flag): lent string =
  ## Returns the canonical name for `flag`.
  cli.name[Parameter flag]

func nameOf*(cli: Cli, positional: Positional): lent string =
  ## Returns the canonical name for `positional`.
  cli.name[Parameter positional]

func nameOf*(cli: Cli, command: Command): lent string =
  ## Returns the canonical name for `command`.
  cli.name[Parameter command]

iterator namesOf*(cli: Cli, flag: Flag): lent string =
  ## Yields names that can be used to refer to `flag`.
  ##
  ## The first name yielded is always the canonical name of `flag`.
  try:
    yield cli.name[Parameter flag]
    for name in cli.alias[Parameter flag].items:
      yield name
  except KeyError:
    discard "Flag has no aliases"

iterator namesOf*(cli: Cli, command: Command): lent string =
  ## Yields names that can be used to refer to `command`.
  ##
  ## The first name yielded is always the canonical name of `command`.
  try:
    yield cli.name[Parameter command]
    for name in cli.alias[Parameter command].items:
      yield name
  except KeyError:
    discard "Command has no aliases"

func parentOf*(cli: Cli, command: Command): Option[Command] =
  ## Returns the parent command of `command`.
  ##
  ## `none(Command)` is only returned for `RootCommand`.
  let parent = Command cli.parent[Parameter command]
  if Parameter(parent) == InvalidParameter:
    none Command
  else:
    some parent

func pathOf*(cli: Cli, command: Command): seq[Command] =
  ## Returns the path leading to `command`.
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
  ## Returns the first long name of `flag`, if one exists.
  ##
  ## A long name is defined as a name longer than one character.
  result = none string
  for name in cli.namesOf(flag):
    if name.len > 1:
      return some name

func shortNameOf*(cli: Cli, flag: Flag): Option[string] =
  ## Returns the first short name of `flag`, if one exists.
  ##
  ## A short name is defined as a one-character long name.
  result = none string
  for name in cli.namesOf(flag):
    if name.len == 1:
      return some name

func usageOf*(cli: Cli, command: Command): lent string =
  ## Returns the `usage` for `command`, as described with `describe`.
  cli.usage[Parameter command]

func usageOf*(cli: Cli, flag: Flag): lent string =
  ## Returns the `usage` for `flag`, as described with `describe`.
  cli.usage[Parameter flag]

func usageOf*(cli: Cli, positional: Positional): lent string =
  ## Returns the `usage` for `positional`, as described with `describe`.
  cli.usage[Parameter positional]

func placeholderOf*(cli: Cli, flag: Flag): string =
  ## Returns the `placeholder` for `flag`, as described with `describe`.
  cli.placeholder[Parameter flag]

func isDispatcher*(cli: Cli, command: Command): bool =
  ## Returns whether `command` contains subcommands.
  cli.command[command].isDispatcher()

func hasDefaultCommand*(cli: Cli, command: Command): bool =
  ## Returns whether `command` has a default subcommand.
  cli.command[command].hasDefaultCommand()

func defaultCommandOf*(cli: Cli, command: Command): Option[Command] =
  ## Returns the default subcommand of `command`.
  if cli.command[command].hasDefaultCommand():
    some(Command cli.command[command].positional[0])
  else:
    none(Command)

func classify*(cli: Cli, param: Parameter): ParameterKind =
  ## Returns the type of `param`.
  case cli.parser[param].kind
  of ParserKind.Command: ParameterKind.Command
  of ParserKind.Flag, FlagOptionalValue: ParameterKind.Flag
  of ParserKind.Positional..OptionalCatchAll: ParameterKind.Positional

func isDefault*(cli: Cli, command: Command): bool =
  ## Returns whether `command` is the default subcommand of its parent.
  ##
  ## .. note::
  ##   This is always false for `RootCommand`.
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
  ## Returns whether a value must be specified on the command line for `flag`.
  cli.parser[Parameter flag].kind == FlagOptionalValue

func isOptional*(cli: Cli, positional: Positional): bool =
  ## Returns whether a value must be specified on the command line for
  ## `positional`.
  cli.parser[Parameter positional].kind in {OptionalPositional, OptionalCatchAll}

func isCatchAll*(cli: Cli, positional: Positional): bool =
  ## Returns whether `positional` is a catch all parameter.
  cli.parser[Parameter positional].kind in {CatchAll, OptionalCatchAll}

proc helpFlagBuilder*[T](
  cli: var Cli[T],
  name: sink string = "help"
): FlagBuilder[T] =
  ## Build a simple flag that triggers `Action.ShowHelp` when specified on the
  ## command line.
  ##
  ## The flag can be added directly to a `cli` or further customized.
  ##
  ## See also: `addHelpFlag <#addHelpFlag%2CCli[T]%2CCommand%2Csinkstring%2Cvarargs[string]>`_
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
  ## Add a flag triggering `Action.ShowHelp` with the given `name` and
  ## `aliases`.
  ##
  ## See also: `helpFlagBuilder <#helpFlagBuilder%2CCli[T]%2Csinkstring>`_
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
  ## Parses the given `args` list, collecting changes made by added parsers
  ## to `accumulator`.
  ##
  ## See also: `run <#run%2CCli[T]%2CT%2Csinkseq[string]%2CFile>`_
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
  ## Parses the given `args` list, then returns the accumulated changes done by
  ## added parsers.
  ##
  ## See also: `run <#run%2CCli[T]%2Csinkseq[string]%2CFile>`_
  parse(cli, result, args)

iterator flags*(cli: Cli, command: Command): Flag =
  ## Iterates through all added flags for `command`.
  # Not the most efficient, but CLIs shouldn't be big enough for this to be an
  # issue
  for i in 0 ..< cli.parser.nextId.int:
    if cli.parent[Parameter i] == Parameter(command) and
      cli.parser[Parameter i].kind in {ParserKind.Flag, FlagOptionalValue}:
      yield Flag(i)

iterator positionals*(cli: Cli, command: Command): Positional =
  ## Iterates through all added positional parameters for `command`.
  if not cli.command[command].isDispatcher:
    for i in cli.command[command].positional.items:
      yield Positional(i)

iterator subcommands*(cli: Cli, command: Command): Command =
  ## Iterates through all added subcommands for `command`.
  for (param, parent) in cli.parent.pairs:
    if parent == Parameter(command) and
      cli.parser[param].kind == ParserKind.Command:
      yield Command(param)

func flagsUsage*(cli: Cli, command: Command): string =
  ## Produces an usage message for all flags registered for `command`.
  ##
  ## For each flag, the rendered message contains the first short and long
  ## name, a placeholder and the described `usage`.
  ##
  ## For flags with required values, the placeholder is rendered as `VALUE`
  ## if not set.
  ##
  ## A placeholder is not shown for flags with optional values if not set.
  ##
  ## The output is separated into two columns: how to specify the flag on
  ## the command line and its associated `usage`.
  ##
  ## Refer to the code sample for an example output.
  runnableExamples:
    func noop(k, v: auto, a: var auto): Action = discard
      ## A parser that does nothing

    var cli = commandBuilder(string)
      .initCli()

    cli.flagBuilder()
      .name("x")
      .alias("exclude")
      .describe("exclude strings")
      .parser(noop)
      .addTo(cli, RootCommand)
    cli.flagBuilder()
      .name("s")
      .describe("silence output")
      .optionalParser(noop)
      .addTo(cli, RootCommand)
    cli.flagBuilder()
      .name("color")
      .describe("whether to show color", "MODE")
      .optionalParser(noop)
      .addTo(cli, RootCommand)

    doAssert cli.flagsUsage(RootCommand) == """
  -x, --exclude <VALUE>  exclude strings
  -s                     silence output
  --color[=<MODE>]       whether to show color"""

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
  ## Render the display form of a positional parameter.
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
  ## Produces an usage message for all positionals registered for `command`.
  ##
  ## For each positional, the rendered message contains the name and the usage
  ## as described by `describe`. The name is wrapped in either `<>` or `[]`
  ## to signify its requirement with `...` suffix added for catch all parameters.
  ##
  ## The output is separated into two columns: the positional display form and
  ## its associated `usage`.
  ##
  ## Refer to the code sample for an example output.
  runnableExamples:
    func noop(v: auto, a: var auto): Action = discard
      ## A parser that does nothing

    var cli = commandBuilder(string)
      .initCli()

    cli.positionalBuilder()
      .name("FIRST")
      .describe("first value")
      .parser(noop)
      .addTo(cli, RootCommand)
    cli.positionalBuilder()
      .name("SEPARATOR")
      .describe("value separator")
      .optional()
      .parser(noop)
      .addTo(cli, RootCommand)
    cli.positionalBuilder()
      .name("VALUE")
      .describe("extra values to join")
      .optional()
      .catchAll()
      .parser(noop)
      .addTo(cli, RootCommand)

    doAssert cli.positionalsUsage(RootCommand) == """
  <FIRST>      first value
  [SEPARATOR]  value separator
  [VALUE]...   extra values to join"""

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

func commandUsage*(
  cli: Cli,
  command: Command,
  rootName: string = cli.nameOf(RootCommand)
): string =
  ## Produces an usage message for the given command. The message is meant to
  ## convey the general shape of the command line.
  ##
  ## The name of the root command can be set for this message using `rootName`.
  ##
  ## Refer to the code sample for example outputs.
  runnableExamples:
    func noop(v: auto, a: var auto): Action = discard
      ## A parser that does nothing

    var cli = commandBuilder(string)
      .name("cmd")
      .initCli()

    let joinCmd = cli.commandBuilder()
      .name("join")
      .addTo(cli, RootCommand)

    cli.positionalBuilder()
      .name("FIRST")
      .describe("first value")
      .parser(noop)
      .addTo(cli, joinCmd)
    cli.positionalBuilder()
      .name("SEPARATOR")
      .describe("value separator")
      .optional()
      .parser(noop)
      .addTo(cli, joinCmd)
    cli.positionalBuilder()
      .name("VALUE")
      .describe("extra values to join")
      .optional()
      .catchAll()
      .parser(noop)
      .addTo(cli, joinCmd)

    doAssert cli.commandUsage(RootCommand) == "cmd [OPTIONS] <COMMAND>"
    doAssert cli.commandUsage(joinCmd) ==
      "cmd join [OPTIONS] <FIRST> [SEPARATOR] [VALUE]..."
    doAssert cli.commandUsage(joinCmd, "cmdbox.exe") ==
      "cmdbox.exe join [OPTIONS] <FIRST> [SEPARATOR] [VALUE]..."

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
  ## Produces an usage message for all subcommands registered for `command`.
  ##
  ## The output is separated into two columns: the subcommand canonical name and
  ## its associated `usage`. A `[default]` suffix is added to the usage message
  ## for default subcommand.
  ##
  ## Refer to the code sample for an example output.
  runnableExamples:
    func noop(v: auto, a: var auto): Action = discard
      ## A parser that does nothing

    var cli = commandBuilder(string)
      .name("cmd")
      .initCli()
    cli.commandBuilder()
      .name("join")
      .describe("join value(s)")
      .addTo(cli, RootCommand)
    cli.commandBuilder()
      .name("cat")
      .describe("concatenate file(s)")
      .addTo(cli, RootCommand)
    cli.commandBuilder()
      .name("install")
      .describe("install file(s) to a given destination")
      .addTo(cli, RootCommand)

    doAssert cli.subcommandsUsage(RootCommand) == """
  join     join value(s)
  cat      concatenate file(s)
  install  install file(s) to a given destination"""

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
  ## Produces the help message for a `command`.
  ##
  ## See the code example for sample outputs.
  runnableExamples:
    func noop(v: auto, a: var auto): Action = discard
      ## A parser that does nothing

    var cli = commandBuilder(string)
      .name("cmd")
      .initCli()
    cli.addHelpFlag(RootCommand, "help", "h")

    let joinCmd = cli.commandBuilder()
      .name("join")
      .describe("join value(s)")
      .addTo(cli, RootCommand)
    cli.addHelpFlag(joinCmd, "help", "h")

    cli.positionalBuilder()
      .name("FIRST")
      .describe("first value")
      .parser(noop)
      .addTo(cli, joinCmd)
    cli.positionalBuilder()
      .name("SEPARATOR")
      .describe("value separator")
      .optional()
      .parser(noop)
      .addTo(cli, joinCmd)
    cli.positionalBuilder()
      .name("VALUE")
      .describe("extra values to join")
      .optional()
      .catchAll()
      .parser(noop)
      .addTo(cli, joinCmd)

    cli.commandBuilder()
      .name("cat")
      .describe("concatenate file(s)")
      .addTo(cli, RootCommand)
    cli.commandBuilder()
      .name("install")
      .describe("install file(s) to a given destination")
      .addTo(cli, RootCommand)

    doAssert cli.help(RootCommand) == """
Usage: cmd [OPTIONS] <COMMAND>

Commands:
  join     join value(s)
  cat      concatenate file(s)
  install  install file(s) to a given destination

Options:
  -h, --help  display help message"""

    doAssert cli.help(joinCmd) == """
join value(s)

Usage: cmd join [OPTIONS] <FIRST> [SEPARATOR] [VALUE]...

Arguments:
  <FIRST>      first value
  [SEPARATOR]  value separator
  [VALUE]...   extra values to join

Options:
  -h, --help  display help message"""

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
  ## Produce a pretty error message for `error`. The provided `error` must be
  ## produced by `parse(cli)`.
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
  ## Parses the command line, collecting changes made by added parsers
  ## to `accumulator`.
  ##
  ## If an error occurs during parsing, the error message will be printed
  ## to `messageOutput` alongside helpful information and the command will
  ## terminate with a failure exit code automatically. The only exception
  ## to this is when `HelpError` occurs, of which the command will terminate
  ## with a successful exit code.
  ##
  ## See also: `parse <#parse%2CCli[T]%2CT%2Csinkseq[string]>`_
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
  ## Parses the command line, returning accumulated changes made by added
  ## parsers.
  ##
  ## See `run <#run%2CCli[T]%2CT%2Csinkseq[string]%2CFile>`_ for more information.
  ##
  ## See also: `parse <#parse%2CCli[T]%2Csinkseq[string]>`_
  run(cli, result, args, messageOutput)
