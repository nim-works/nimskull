#
#
#                 NimSkull's runtime library
#     (c) Copyright 2025 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

## This module implements a fast and lightweight declarative command line
## parser, aiming to simplify the creation of user-friendly command-line
## interfaces.
##
## Key features:
## - Arbitrary command nesting.
## - Type-safe, declarative parsing.
## - Automatic generation of help output.
##
## ## Examples
## ### Greeting program

runnableExamples:
  import std/sugar

  type Args = object
    count: Natural
    name: string

  var cli = commandBuilder(Args)
    .name("hello")
    .describe("simple greeting program")
    .initCli()
  cli.addHelpFlag()
  cli.flagBuilder()
    .name("count")
    .parser(Natural, (opt, val, var args) => (args.count = val))
    .describe("number of greetings")
    .addTo(cli)
  cli.flagBuilder()
    .name("name")
    .parser(string, (opt, val, var args) => (args.name = val))
    .describe("the person to greet")
    .addTo(cli)

  let args = cli.run(defaults = Args(count: 1))
  for _ in 1..args.count:
    if args.name == "":
      echo "Hello!"
    else:
      echo "Hello ", args.name, "!"

## What this looks like when run:
##
## ```
## $ ./hello --count 3
## Hello!
## Hello!
## Hello!
## ```
##
## The help page is generated for you:
##
## ```
## $ ./hello --help
## simple greeting program
##
## Usage: hello [OPTIONS]
##
## Options:
##   --help           display help message
##   --count <VALUE>  number of greetings
##   --name <VALUE>   the person to greet
## ```
##
## Comes with error handling, too:
##
## ```
## $ ./hello --count=no
## error: invalid value for '--count <VALUE>': invalid integer: no
##
## Usage: hello [OPTIONS]
## ```
##
## ### Program with subcommands

runnableExamples("-r:off"):
  import std/setutils
  import std/sugar

  type
    Operation = enum
      Ls
      Rm

    LsArgs = object
      paths: seq[string]

    RmOpt {.pure.} = enum
      Force
      Recursive

    RmArgs = object
      opts: set[RmOpt]
      paths: seq[string]

    Config = object
      case op: Operation
      of Ls:
        ls: LsArgs
      of Rm:
        rm: RmArgs

  var cli = commandBuilder(Config)
    .name("cmd")
    .describe("multi tool")
    .initCli()
  cli.addHelpFlag(RootCommand, "help", "h")

  let lsCmd = cli.commandBuilder()
    .name("ls")
    .describe("list paths")
    .parser((_, var cfg) => (cfg = Config(op: Ls)))
    .addTo(cli, RootCommand)
  cli.addHelpFlag(lsCmd)
  cli.positionalBuilder()
    .name("PATH")
    .describe("path(s) to list, default to current directory")
    .optional()
    .catchAll()
    .parser(string, (val, var cfg) => cfg.ls.paths.add val)
    .addTo(cli, lsCmd)

  let rmCmd = cli.commandBuilder()
    .name("rm")
    .describe("remove files")
    .parser((_, var cfg) => (cfg = Config(op: Rm)))
    .addTo(cli, RootCommand)
  cli.addHelpFlag(rmCmd)
  cli.flagBuilder()
    .name("force")
    .alias("f")
    .describe("force removal")
    .parser(bool, (_, val, var cfg) => (cfg.rm.opts[Force] = val))
    .addTo(cli, rmCmd)
  cli.flagBuilder()
    .name("recursive")
    .alias("r")
    .describe("recursively remove files")
    .parser(bool, (_, val, var cfg) => (cfg.rm.opts[Recursive] = val))
    .addTo(cli, rmCmd)
  cli.positionalBuilder()
    .name("PATH")
    .describe("path(s) to remove")
    .catchAll()
    .parser(string, (val, var cfg) => cfg.rm.paths.add val)
    .addTo(cli, rmCmd)

  let config = cli.run()
  case config.op
  of Ls:
    echo "list files at: ", config.ls.paths
  of Rm:
    echo "remove flags: ", config.rm.opts
    echo "remove paths: ", config.rm.paths

## What this looks like when run:
##
## ```
## $ ./cmd ls /
## list files at: @["/"]
##
## $ ./cmd rm -rf secret
## remove flags: {Force, Recursive}
## remove paths: @["secret"]
## ```
##
## Generated help pages:
##
## ```
## $ ./cmd --help
## multi tool
##
## Usage: cmd [OPTIONS] <COMMAND>
##
## Commands:
##   ls  list paths
##   rm  remove files
##
## Options:
##   -h, --help  display help message
##
## $ ./cmd ls --help
## list paths
##
## Usage: cmd ls [OPTIONS] [PATH]...
##
## Arguments:
##   [PATH]...  path(s) to list, default to current directory
##
## Options:
##   --help  display help message
##
## $ ./cmd rm --help
## remove files
##
## Usage: cmd rm [OPTIONS] <PATH>...
##
## Arguments:
##   <PATH>...  path(s) to remove
##
## Options:
##   --help           display help message
##   -f, --force      force removal
##   -r, --recursive  recursively remove files
## ```
##
## ## Interacting with the parsing process
##
## `cmdline` allows registered parsers to interact with the command-line
## parsing process via a few mechanisms:
##
## Parsers may signal errors in value interpretation by raising a
## `ValueError`, which will be processed automatically by the library. Any
## other exceptions are considered internal errors, and will be propagated
## directly to caller.
runnableExamples:
  proc errorParser(key, value: string, acc: var string) =
    raise newException(ValueError, "some error with: " & value)

  proc faultyParser(key, value: string, acc: var string) =
    raise newException(IOError, "something unexpected")

  var cli = commandBuilder(string).initCli()
  cli.flagBuilder()
    .name("error")
    .parser(errorParser)
    .addTo(cli)
  cli.flagBuilder()
    .name("fault")
    .parser(faultyParser)
    .addTo(cli)

  doAssertRaises(InvalidValueError):
    discard cli.parse(@["--error=value"])

  doAssertRaises(IOError):
    discard cli.parse(@["--fault=value"])

## Parsers may control parsing behavior following its parameter by returning an
## `Action`. The default action taken if not specified is `Action.Continue`.
runnableExamples:
  import std/sugar

  proc actionParser(key, value: string, acc: var seq[string]): Action =
    case value
    of "help": Action.ShowHelp
    of "noflag": Action.DisableFlagProcessing
    else: Action.Continue

  var cli = commandBuilder(seq[string]).initCli()
  cli.flagBuilder()
    .name("action")
    .parser(actionParser)
    .addTo(cli)
  # To collect anything that's not a flag
  cli.positionalBuilder()
    .name("ANY")
    .catchAll()
    .optional()
    .parser((v, var s) => s.add v)
    .addTo(cli)

  doAssertRaises(HelpError):
    discard cli.parse(@["--action=help", "--invalid-flag"])

  doAssert cli.parse(
    @["start", "--action=none", "--action=noflag", "--action=help"]
  ) == ["start", "--action=help"]

# XXX: VM cannot compile std/os
when not defined(vm):
  from std/os import nil

import
  std/[
    algorithm,
    hashes,
    options,
    sequtils,
    strutils,
    tables
  ],
  std/private/[
    containers
  ],
  cmdline/[
    parsers
  ],
  experimental/[
    lexopt
  ]

export parsers

type
  Cli*[T] {.requiresInit.} = object
    ## A command line interface description.
    ##
    ## See also:
    ## - `initCli proc <#initCli,sinkCommandBuilder[T]>`_
    commands: Table[CommandId, CliCommand]
      ## Lookup mapping of command to lookup tables.
    parsers: Store[ParameterId, ParserAny[T]]
      ## Parser to process input for ParameterId.
    names: Store[ParameterId, string]
      ## Canonical names for all ParameterIds.
    aliases: Table[ParameterId, seq[string]]
      ## Mapping of ParameterId to aliases.
    parents: Store[ParameterId, ParameterId]
      ## Mapping of ParameterId to their parent.

    # Documentation storage
    #
    # Might be useful to support not having these for space-constrained
    # targets using a define.
    usages: Store[ParameterId, string] ## Canonical usage for ParameterIds.
    placeholders: Store[ParameterId, string]
      ## Canonical placeholder for ParameterIds. Only used for flags.

  CliCommand = object
    ## Lookup table for flags and positionals.
    flags: Table[string, ParameterId]
      ## Lookup mapping of flag names to ParameterId.
    subcommands: Table[string, CommandId]
      ## Lookup mapping of command names to CommandId.
    positionals: seq[ParameterId]
      ## Lookup mapping of position to ParameterId.

  FlagBuilder*[T] = object
    ## Builder for command line flags.
    ##
    ## See also:
    ## - `flagBuilder proc <#flagBuilder,Cli[T]>`_
    flagParser: ParserAny[T]
    flagName: string
    aliases: seq[string]
    usage: string
    placeholder: string

  PositionalBuilder*[T] = object
    ## Builder for command line positional parameters.
    ##
    ## See also:
    ## - `positionalBuilder proc <#positionalBuilder,Cli[T]>`_
    posParser: PositionalParser[T, Action]
    posName: string
    usage: string
    isOptional: bool
    isCatchAll: bool

  CommandBuilder*[T] = object
    ## Builder for command line subcommands.
    ##
    ## See also:
    ## - `commandBuilder proc <#commandBuilder,Cli[T]>`_
    cmdParser: CommandParser[T, Action]
    cmdName: string
    aliases: seq[string]
    usage: string
    isDefault: bool

  Action* {.pure.} = enum
    ## Action to be taken after parsing.
    Continue ## Continue parameter parsing.
    ShowHelp ## Abort and show help message.
    DisableFlagProcessing ## Parameters following this will no longer be
                          ## interpreted as flags.

  MaybeAction* = Action | void
    ## Typeclass to support parsers returning either `Action` or nothing.

  FlagParser*[T; R: MaybeAction] = proc (name, value: string, accumulator: var T): R
    ## A parser for flag with `option` and `value`. The accumulator passed to
    ## `run`_ or `parse`_ can be accessed and modified via `accumulator`.
    ##
    ## See also:
    ## - `Interacting with the parsing process <#interacting-with-the-parsing-process>`_
    ##
    ## .. _run: #run,Cli[T],T,sinkseq[string],File
    ## .. _parse: #parse,Cli[T],T,sinkseq[string]

  FlagOptionalParser*[T; R: MaybeAction] = proc (name: string, value: Option[string], accumulator: var T): R
    ## A parser for flag with `name` and optional `value`. The accumulator
    ## passed to `run`_ or `parse`_ can be accessed and modified via
    ## `accumulator`.
    ##
    ## See also:
    ## - `Interacting with the parsing process <#interacting-with-the-parsing-process>`_
    ##
    ## .. _run: #run,Cli[T],T,sinkseq[string],File
    ## .. _parse: #parse,Cli[T],T,sinkseq[string]

  PositionalParser*[T; R: MaybeAction] = proc (value: string, accumulator: var T): R
    ## A parser for positional parameter with value `value`. The accumulator
    ## passed to `run`_ or `parse`_ can be accessed and modified via
    ## `accumulator`.
    ##
    ## See also:
    ## - `Interacting with the parsing process <#interacting-with-the-parsing-process>`_
    ##
    ## .. _run: #run,Cli[T],T,sinkseq[string],File
    ## .. _parse: #parse,Cli[T],T,sinkseq[string]

  CommandParser*[T; R: MaybeAction] = proc (command: CommandId, accumulator: var T): R
    ## A parser for command parameter with command `command`. The accumulator
    ## passed to `run`_ or `parse`_ can be accessed and modified via
    ## `accumulator`.
    ##
    ## See also:
    ## - `Interacting with the parsing process <#interacting-with-the-parsing-process>`_
    ##
    ## .. _run: #run,Cli[T],T,sinkseq[string],File
    ## .. _parse: #parse,Cli[T],T,sinkseq[string]

  FlagTypedParser*[T; U; R: MaybeAction] = proc (name: string, value: U, accumulator: var T): R
    ## Typed variant of `FlagParser <#FlagParser>`_.

  FlagOptionalTypedParser*[T; U; R: MaybeAction] = proc (name: string, value: Option[U], accumulator: var T): R
    ## Typed variant of `FlagOptionalParser <#FlagOptionalParser>`_.

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
    of FlagOptionalValue:
      optParser: FlagOptionalParser[T, Action]
    of ParserKind.Flag:
      parser: FlagParser[T, Action]
    of ParserKind.Positional..OptionalCatchAll:
      posParser: PositionalParser[T, Action]
    of ParserKind.Command:
      cmdParser: CommandParser[T, Action]

  ParseContext = object
    ## Parser internal state.
    lexer: CmdLexer ## Lexer driving the parse.
    nextPositional: Natural ## Next positional parser to use.
    positionalCount: Natural ## Number of positionals parsed.
    command: CommandId ## The active command.
    isValueOnly: bool ## Whether flags are ignored.

  ParseError* = object of CatchableError
    ## An error during command line parsing.
    command*: CommandId ## Active command during error.
    remaining*: seq[string] ## ParameterIds that were not parsed.

  FlagError* = object of ParseError
    ## An error parsing flags.
    flagName*: string ## Name of the flag causing the error, as specified by
                      ## input
  UnknownFlagError* = object of FlagError
    ## The flag parsed was not recognized.
    flagValue*: Option[string] ## Inline value of flag causing error
  MissingValueError* = object of FlagError
    ## The flag parsed requires a value but was not provided.
    flag*: FlagId ## Handle to the flag
  InvalidValueError* = object of FlagError
    ## Invalid value passed to a flag.
    ##
    ## The `ValueError` causing this can be found in the `parent` field.
    flagValue*: Option[string] ## The string value received. This should always
                               ## be `some(string)` for flags with non-optional
                               ## value.
    flag*: FlagId ## Handle to the flag.

  PositionalError* = object of ParseError
    ## An error parsing positional parameters.
    positionalValue*: string ## Input value causing the error
  UnknownPositionalError* = object of PositionalError
    ## The positional parsed was not recognized.
  MissingPositionalError* = object of PositionalError
    ## A required positional parameter is missing from input.
    positional*: PositionalId ## Handle to the positional
  InvalidPositionalError* = object of PositionalError
    ## The positional parsed was invalid.
    ##
    ## The `ValueError` causing this can be found in the `parent` field.
    positional*: PositionalId ## Handle to the positional

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
    targetCommand*: CommandId ## Handle to the target command

  HelpError* = object of ParseError
    ## Help was requested using `Action.ShowHelp`.
    paramName*: string ## Name of the parameter triggering help, as specified by
                       ## input.
    param*: ParameterId ## Handle to the parameter triggering help.

  ParameterKind* {.pure.} = enum
    Command
    Flag
    Positional

  ParameterId* = distinct uint32
    ## A command line parameter. Values of this type are tied to the originating
    ## `Cli` instance.
    ##
    ## A `ParameterId` might be a `CommandId`, `FlagId`, or `PositionalId`. The
    ## `classify` function can be used to distinguish between them.

  CommandId* = distinct ParameterId
    ## A command line subcommand. Values of this type are tied to the
    ## originating `Cli` instance.

  FlagId* = distinct ParameterId
    ## A command line flag. Values of this type are tied to the originating
    ## `Cli` instance.

  PositionalId* = distinct ParameterId
    ## A command line positional parameter. Values of this type are tied to the
    ## originating `Cli` instance.

const
  InvalidParameter = high(ParameterId)
    ## An invalid parameter
  RootCommand* = CommandId(0)
    ## The top-level command of `Cli`

proc hash(x: ParameterId): Hash {.borrow.}

proc `==`*(a, b: ParameterId): bool {.borrow.}
proc `==`*(a, b: CommandId): bool {.borrow.}
proc `==`*(a, b: FlagId): bool {.borrow.}
proc `==`*(a, b: PositionalId): bool {.borrow.}

# FIXME: Put this in system.nim
func drop[T](_: sink T) = discard

func commandBuilder*[T](cli: Cli[T]): CommandBuilder[T] =
  ## Creates a new `CommandBuilder`.
  result = CommandBuilder[T]()

func commandBuilder*(T: typedesc): CommandBuilder[T] =
  ## Creates a new `CommandBuilder`.
  result = CommandBuilder[T]()

func flagBuilder*[T](cli: Cli[T]): FlagBuilder[T] =
  ## Creates a new `FlagBuilder`.
  result = FlagBuilder[T]()

func positionalBuilder*[T](cli: Cli[T]): PositionalBuilder[T] =
  ## Creates a new `PositionalBuilder`.
  result = PositionalBuilder[T]()

func name*[T](b: sink CommandBuilder[T], name: string): CommandBuilder[T] =
  ## Sets the canonical name of this command, which is used to identify
  ## this command on the command line.
  ##
  ## .. note::
  ##   A name is optional for the root command.
  ##
  ## See also:
  ## - `alias func <#alias,sinkCommandBuilder[T],varargs[string]>`_
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.commandBuilder()
      .name("act")
      .parser((_, var s) => (s = "acted"))
      .addTo(cli, RootCommand)
    doAssert cli.parse(@["act"]) == "acted"

  result = b
  result.cmdName = name
  result.aliases.keepItIf: it != result.cmdName

func name*[T](b: sink FlagBuilder[T], name: string): FlagBuilder[T] =
  ## Sets the canonical name of this flag, which is used to identify
  ## this flag on the command line.
  ##
  ## If `name` is only one ASCII character long, it may use the short form
  ## syntax (e.g. `-n`).
  ##
  ## See also:
  ## - `alias func <#alias,sinkFlagBuilder[T],varargs[string]>`_
  runnableExamples:
    import std/sugar

    type Args = object
      str: string
      i: int

    var cli = commandBuilder(Args)
      .initCli()
    cli.flagBuilder()
      .name("string")
      .parser(string, (_, val, var args) => (args.str = val))
      .addTo(cli)
    cli.flagBuilder()
      .name("i")
      .parser(int, (_, val, var args) => (args.i = val))
      .addTo(cli)
    doAssert cli.parse(@["--string=str"]) == Args(str: "str")
    doAssert cli.parse(@["-i=10"]) == Args(i: 10)

  result = b
  result.flagName = name
  result.aliases.keepItIf: it != result.flagName

func name*[T](b: sink PositionalBuilder[T], name: string): PositionalBuilder[T] =
  ## Sets the canonical name of this positional, which is used when providing
  ## diagnostics and help message.
  result = b
  result.posName = name

func alias*[T](b: sink CommandBuilder[T], names: varargs[string]): CommandBuilder[T] =
  ## Sets aliases for this command.
  ##
  ## This command can then be matched using any of the provided `names` in
  ## addition to its canonical name.
  ##
  ## See also:
  ## - `name func <#name,sinkCommandBuilder[T],string>`_
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.commandBuilder()
      .name("act")
      .alias("a", "do")
      .parser((_, var s) => (s = "acted"))
      .addTo(cli, RootCommand)
    doAssert cli.parse(@["act"]) == "acted"
    doAssert cli.parse(@["a"]) == "acted"
    doAssert cli.parse(@["do"]) == "acted"

  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's assumed that users will
  # specify a handful (approximately 1-4) of these.
  for name in names.items:
    if name == result.cmdName or name in result.aliases:
      continue
    result.aliases.add names

func alias*[T](b: sink FlagBuilder[T], names: varargs[string]): FlagBuilder[T] =
  ## Sets aliases for this flag.
  ##
  ## The flag can be matched using any of the provided `names` in addition to
  ## its canonical name.
  ##
  ## See also:
  ## - `name func <#name,sinkFlagBuilder[T],string>`_
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.flagBuilder()
      .name("string")
      .alias("str", "s")
      .parser(string, (_, val, var str) => (str = val))
      .addTo(cli)
    doAssert cli.parse(@["--string=str"]) == "str"
    doAssert cli.parse(@["--str=str"]) == "str"
    doAssert cli.parse(@["-s=str"]) == "str"

  result = b
  result.aliases.setLen(0)

  # Not the fastest method, but it's assumed that users will
  # specify a handful (approximately 1-4) of these.
  for name in names.items:
    if name == result.flagName or name in result.aliases:
      continue
    result.aliases.add names

func default*[T](b: sink CommandBuilder[T]): CommandBuilder[T] =
  ## Marks this command as the default subcommand of its parent. When no
  ## subcommand is present on the command line, this command will be selected.
  ##
  ## Only one default subcommand is permitted per command. Attempting to
  ## add another default subcommand to a command will cause an error.
  ##
  ## This attribute is ignored for the root command.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.commandBuilder()
      .name("act")
      .default()
      .parser((_, var s) => (s = "acted"))
      .addTo(cli, RootCommand)

    doAssert cli.parse(@[]) == "acted"

  result = b
  result.isDefault = true

func catchAll*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  ## Marks this positional as "catch all". All positional parameters encountered
  ## starting at this positional will be handled by the associated parser.
  ##
  ## A catch all positional can only be added as the last parameter of a
  ## command. No other positional parameters might be added after this.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(seq[string])
      .initCli()
    cli.positionalBuilder()
      .name("STR")
      .catchAll()
      .parser((v, var s) => s.add v)
      .addTo(cli)

    doAssert cli.parse(@["a", "b", "c"]) == ["a", "b", "c"]

  result = b
  result.isCatchAll = true

func optional*[T](b: sink PositionalBuilder[T]): PositionalBuilder[T] =
  ## Marks this positional as optional. When not specified on the command line,
  ## the associated parser will not be called.
  ##
  ## No non-optional positional parameters might be added to a command after the
  ## first optional.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.positionalBuilder()
      .name("STR")
      .optional()
      .parser((v, var s) => (s = v))
      .addTo(cli)

    doAssert cli.parse(@[]) == ""
    doAssert cli.parse(@["a"]) == "a"

  result = b
  result.isOptional = true

func parser*[T](
  b: sink CommandBuilder[T],
  p: sink CommandParser[T, Action],
): CommandBuilder[T] =
  ## Sets the parser for this command. This is called when the command is
  ## matched on the command line.
  ##
  ## .. note::
  ##   A parser is optional for commands.
  ##
  ## .. warning::
  ##   It is an error to set a parser for the root command.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.commandBuilder()
      .name("help")
      .parser((_, var s) => Action.ShowHelp)
      .addTo(cli, RootCommand)
    doAssertRaises(HelpError):
      discard cli.parse(@["help"])

  result = b
  result.cmdParser = p

func parser*[T](
  b: sink CommandBuilder[T],
  p: sink CommandParser[T, void],
): CommandBuilder[T] {.inline.} =
  ## Sets the parser for this command with `Action.Continue` as the default
  ## action.
  ##
  ## .. note::
  ##   A parser is optional for commands.
  ##
  ## .. warning::
  ##   It is an error to set a parser for the root command.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.commandBuilder()
      .name("act")
      .parser((_, var s) => (s = "acted"))
      .addTo(cli, RootCommand)
    doAssert cli.parse(@["act"]) == "acted"

  b.parser(
    proc (command: CommandId, accumulator: var T): Action =
      p(command, accumulator)
  )

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink FlagOptionalParser[T, Action],
): FlagBuilder[T] =
  ## Sets the parser for this flag, and marks the flag as not requiring any
  ## value.
  ##
  ## For flags with an optional value, their parser will only be called when
  ## the value is specified inline, for example: `--flag=value` or
  ## `--flag:value`.
  ##
  ## See also:
  ## - `parser proc <#parser,sinkFlagBuilder[T],sinkFlagParser[T,Action]>`_
  runnableExamples:
    import std/options

    proc parser(opt: string, val: Option[string], str: var string): Action =
      if val == some("help"):
        Action.ShowHelp
      else:
        str = val.get(otherwise = "default")
        Action.Continue

    var cli = commandBuilder(string)
      .initCli()
    cli.flagBuilder()
      .name("string")
      .optionalParser(parser)
      .addTo(cli)
    doAssert cli.parse(@["--string"]) == "default"
    doAssert cli.parse(@["--string", "--string"]) == "default"
    doAssert cli.parse(@["--string=--string"]) == "--string"
    doAssertRaises(HelpError):
      discard cli.parse(@["--string:help"])

  result = b
  result.flagParser = ParserAny[T](kind: FlagOptionalValue, optParser: p)

func optionalParser*[T](
  b: sink FlagBuilder[T],
  p: sink FlagOptionalParser[T, void],
): FlagBuilder[T] =
  ## Sets the parser for this flag with `Action.Continue` as the default action,
  ## and marks the flag as not requiring any value.
  ##
  ## Flags with optional value will only receive their value when it is
  ## specified inline, for example: `--flag=value` or `--flag:value`.
  ##
  ## See also:
  ## - `parser proc <#parser,sinkFlagBuilder[T],sinkFlagParser[T,void]>`_
  runnableExamples:
    import std/options
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.flagBuilder()
      .name("string")
      .optionalParser((_, val, var str) => (str = val.get("default")))
      .addTo(cli)
    doAssert cli.parse(@["--string"]) == "default"
    doAssert cli.parse(@["--string", "--string"]) == "default"
    doAssert cli.parse(@["--string=--string"]) == "--string"
    doAssert cli.parse(@["--string:help"]) == "help"

  b.optionalParser(
    proc (name: string, value: Option[string], accumulator: var T): Action =
      p(name, value, accumulator)
  )

func parser*[T](b: sink FlagBuilder[T], p: sink FlagParser[T, Action]): FlagBuilder[T] =
  ## Sets the parser for this flag, and marks the flag as requiring values.
  ##
  ## Flags with a required value can receive any of the following forms:
  ##
  ## - `--flag=value`
  ## - `--flag:value`
  ## - `--flag value`
  ##
  ## See also:
  ## - `optionalParser proc <#optionalParser,sinkFlagBuilder[T],sinkFlagOptionalParser[T,Action]>`_
  runnableExamples:
    proc parser(opt: string, val: string, str: var string): Action =
      if val == "help":
        Action.ShowHelp
      else:
        str = val
        Action.Continue

    var cli = commandBuilder(string)
      .initCli()
    cli.flagBuilder()
      .name("string")
      .parser(parser)
      .addTo(cli)
    doAssertRaises(MissingValueError):
      discard cli.parse(@["--string"])

    doAssert cli.parse(@["--string", "--string"]) == "--string"
    doAssert cli.parse(@["--string=--string"]) == "--string"

    doAssertRaises(HelpError):
      discard cli.parse(@["--string", "help"])

  result = b
  result.flagParser = ParserAny[T](kind: ParserKind.Flag, parser: p)

func parser*[T](b: sink FlagBuilder[T], p: sink FlagParser[T, void]): FlagBuilder[T] =
  ## Sets the parser for this flag with `Action.Continue` as the default action,
  ## and marks the flag as requiring values.
  ##
  ## Flags with a required value can receive any of the following forms:
  ##
  ## - `--flag=value`
  ## - `--flag:value`
  ## - `--flag value`
  ##
  ## See also:
  ## - `optionalParser proc <#optionalParser,sinkFlagBuilder[T],sinkFlagOptionalParser[T,void]>`_
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.flagBuilder()
      .name("string")
      .parser((_, val, var str) => (str = val))
      .addTo(cli)

    doAssertRaises(MissingValueError):
      discard cli.parse(@["--string"])
    doAssert cli.parse(@["--string", "--string"]) == "--string"
    doAssert cli.parse(@["--string=--string"]) == "--string"
    doAssert cli.parse(@["--string", "help"]) == "help"

  b.parser(
    proc (name: string, value: string, accumulator: var T): Action =
      p(name, value, accumulator)
  )

func optionalParser*[T, U; R: MaybeAction](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink FlagOptionalTypedParser[T, U, R],
): FlagBuilder[T] =
  ## Sets the parser for this flag, and marks the flag as not requiring any
  ## value.
  ##
  ## Input string values from the command line will first be parsed using
  ## `parseCli` before handing off to the parser. See `parsers module`_ for
  ## more information.
  ##
  ## Flags with optional value will only receive their value when it is
  ## specified inline, for example: `--flag=value` or `--flag:value`.
  ##
  ## See also:
  ## - `parser proc <#parser,sinkFlagBuilder[T],typedesc[U],sinkFlagTypedParser[T,U,R]>`_
  ##
  ## .. _parsers module: parsers.html
  runnableExamples:
    import std/options
    import std/sugar

    var cli = commandBuilder(int)
      .initCli()
    cli.flagBuilder()
      .name("int")
      .optionalParser(int, (_, val, var i) => (i = val.get(42)))
      .addTo(cli)
    doAssert cli.parse(@["--int"]) == 42
    doAssert cli.parse(@["--int", "--int"]) == 42
    doAssertRaises(InvalidValueError):
      discard cli.parse(@["--int=--int"])
    doAssert cli.parse(@["--int=1000"]) == 1000

  mixin parseCli

  when U is string:
    result = b.optionalParser(FlagOptionalParser[T, R] parser)
  else:
    result = b.optionalParser(
      proc (name: string, value: Option[string], r: var T): Action =
        let value = value.map(proc (x: string): U = parseCli(U, x))
        parser(name, value, r)
    )

proc parser*[T, U; R: MaybeAction](
  b: sink FlagBuilder[T],
  _: typedesc[U],
  parser: sink FlagTypedParser[T, U, R],
): FlagBuilder[T] =
  ## Sets the parser for this flag, and marks the flag as requiring values.
  ##
  ## Input string values from the command line will first be parsed using
  ## `parseCli` before handing off to the parser. See `parsers module`_ for
  ## more information.
  ##
  ## When `U` is `bool`, this flag behaves like a switch and does not
  ## require a value to be passed. If values are to be passed, it must be
  ## inlined (i.e. `--flag=false`).
  ##
  ## Flags with a required value can receive any of the following forms:
  ##
  ## - `--flag=value`
  ## - `--flag:value`
  ## - `--flag value` (only when `U` is not `bool`)
  ##
  ## See also:
  ## - `optionalParser proc <#optionalParser,sinkFlagBuilder[T],typedesc[U],sinkFlagOptionalTypedParser[T,U,R]>`_
  ##
  ## .. _parsers module: parsers.html
  runnableExamples:
    import std/options
    import std/sugar

    type Args = object
      i: int
      b: bool

    var cli = commandBuilder(Args)
      .initCli()
    cli.flagBuilder()
      .name("int")
      .parser(int, (_, val, var args) => (args.i = val))
      .addTo(cli)
    cli.flagBuilder()
      .name("switch")
      .parser(bool, (_, val, var args) => (args.b = val))
      .addTo(cli)
    doAssertRaises(MissingValueError):
      discard cli.parse(@["--int"])
    doAssertRaises(InvalidValueError):
      discard cli.parse(@["--int", "string"])
    doAssert cli.parse(@["--int", "1000"]) == Args(i: 1000)
    doAssert cli.parse(@["--switch"]) == Args(b: true)
    doAssert cli.parse(@["--switch", "--switch:false"]) == Args(b: false)

  mixin parseCli

  when U is bool:
    result = b.optionalParser(
      proc (name: string, value: Option[string], r: var T): R =
        parser(name, parseCli(U, value.get("true")), r)
    )

  elif U is string:
    result = b.parser(FlagParser[T, R] parser)

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
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.positionalBuilder()
      .name("NO-FLAGS")
      .parser((_, var s) => Action.DisableFlagProcessing)
      .addTo(cli)
    cli.positionalBuilder()
      .name("STR")
      .parser((v, var s) => (s = v; Action.Continue))
      .addTo(cli)

    doAssert cli.parse(@["a", "-b"]) == "-b"

  result = b
  result.posParser = p

func parser*[T](
  b: sink PositionalBuilder[T],
  p: sink PositionalParser[T, void]
): PositionalBuilder[T] =
  ## Sets the parser for this positional parameter.
  ## On successful parse, `Action.Continue` is taken as the default action.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    cli.positionalBuilder()
      .name("STR")
      .parser((v, var s) => (s = v))
      .addTo(cli)

    doAssert cli.parse(@["a"]) == "a"

  b.parser(
    proc (value: string, accumulator: var T): Action =
      p(value, accumulator)
  )

proc parser*[T, U; R: MaybeAction](
  b: sink PositionalBuilder[T],
  _: typedesc[U],
  parser: sink TypedPositionalParser[T, U, R],
): PositionalBuilder[T] =
  ## Sets the parser for this positional parameter.
  ##
  ## Input string values from the command line will first be parsed using
  ## `parseCli` before handing off to the parser. See `parsers module`_ for
  ## more information.
  ##
  ## .. _parsers module: parsers.html
  runnableExamples:
    import std/sugar

    type Args = object
      a, b: int

    var cli = commandBuilder(Args)
      .initCli()
    cli.positionalBuilder()
      .name("A")
      .parser(int, (v, var args) => (args.a = v; Action.DisableFlagProcessing))
      .addTo(cli)
    cli.positionalBuilder()
      .name("B")
      .parser(int, (v, var args) => (args.b = v))
      .addTo(cli)

    doAssert cli.parse(@["10", "-10"]) == Args(a: 10, b: -10)

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
  ## Sets a short `usage` description for this command.
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
  ## Sets a short `usage` description and a value `placeholder` for this flag.
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
  ## Sets a short `usage` description for this positional.
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
  parent: ParameterId,
  name, usage, placeholder: sink string,
  parser: sink ParserAny[T]
): ParameterId =
  ## Adds a new parameter to `cli`.
  ##
  ## .. important::
  ##   Input parameters are assumed to meet operational constraints.
  result = cli.names.add(name)
  discard cli.usages.add(usage)
  discard cli.placeholders.add(placeholder)
  discard cli.parsers.add(parser)
  discard cli.parents.add(parent)

func initCli*[T: not void](b: sink CommandBuilder[T]): Cli[T] =
  ## Creates a new `Cli`, with `b` used to construct the root command.
  ##
  ## The root command must:
  ##
  ## - Have no `parser` set.
  ## - Have no `aliases`.
  ##
  ## Unlike subcommands, the root command may:
  ##
  ## - Have an empty (or unset) `name`, since the `name` is not used during
  ##   parsing. A `name`, if specified, will be used in documentation
  ##   generation.
  ##
  ## The root command can be referred to using the `RootCommand` constant.
  ##
  ## See also:
  ## - `commandBuilder proc <#commandBuilder,typedesc>`_
  result = Cli[T](
    commands: default(typeof result.commands),
    parsers: default(typeof result.parsers),
    names: default(typeof result.names),
    aliases: default(typeof result.aliases),
    parents: default(typeof result.parents),
    usages: default(typeof result.usages),
    placeholders: default(typeof result.placeholders),
  )

  assert b.cmdParser == nil, "Root parser cannot be non-nil"
  assert b.aliases == [], "Root parser cannot have aliases"

  discard result.addCommon(
    InvalidParameter,
    b.cmdName,
    b.usage,
    "",
    ParserAny[T](kind: ParserKind.Command)
  )
  result.commands[RootCommand] = CliCommand()

func hasSubcommand(cmd: CliCommand): bool =
  cmd.subcommands.len > 0

func hasPositional(cmd: CliCommand): bool =
  not cmd.hasSubcommand and cmd.positionals.len > 0

func hasDefaultSubcommand(cmd: CliCommand): bool =
  cmd.hasSubcommand and cmd.positionals.len > 0

func addTo*[T](
  b: sink CommandBuilder[T],
  cli: var Cli[T],
  command: CommandId,
): CommandId {.discardable.} =
  ## Adds the command specified by `b` as a subcommand of `command`.
  ##
  ## A subcommand can only be added to `command` if:
  ##
  ## - `command` has no positional parameters.
  ## - The subcommand has a non-empty name.
  ## - Neither the name nor aliases are used by previously added subcommands.
  ##
  ## `ValueError` will be raised if any of the specified constraints are
  ## violated.
  ##
  ## See also:
  ## - `commandBuilder proc <#commandBuilder,Cli[T]>`_
  if b.cmdName == "":
    raise newException(ValueError, "Command name must not be empty")
  if b.cmdName in cli.commands[command].subcommands:
    raise newException(ValueError, "Command '" & b.cmdName & "' already exists")
  for alias in b.aliases.items:
    assert alias != "", "Command alias cannot be empty"
    if alias in cli.commands[command].subcommands:
      raise newException(ValueError, "Command '" & alias & "' already exists")

  if cli.commands[command].hasPositional:
    raise newException(ValueError, "Cannot add subcommand to command with positional parameters")

  if b.isDefault and cli.commands[command].hasDefaultSubcommand:
    raise newException(ValueError, "Command already has a default subcommand registered")

  result = CommandId cli.addCommon(
    ParameterId command,
    b.cmdName,
    b.usage,
    "",
    ParserAny[T](kind: ParserKind.Command, cmdParser: b.cmdParser)
  )
  cli.commands[result] = CliCommand()
  cli.commands[command].subcommands[b.cmdName] = result
  if b.isDefault:
    cli.commands[command].positionals.add(ParameterId result)
  for alias in b.aliases.items:
    cli.commands[command].subcommands[alias] = result
  if b.aliases.len > 0:
    cli.aliases[ParameterId result] = b.aliases

func addTo*[T](
  b: sink FlagBuilder[T],
  cli: var Cli[T],
  command: CommandId = RootCommand,
): FlagId {.discardable.} =
  ## Adds the flag specified by `b` to `command`.
  ##
  ## A flag can only be added to `command` if:
  ##
  ## - The flag has a non-empty name.
  ## - Neither the name nor aliases are used by previously added flags.
  ## - A parser or optional parser as set for the flag.
  ##
  ## `ValueError` will be raised if any of the specified constraints are
  ## violated.
  ##
  ## See also:
  ## - `flagBuilder proc <#flagBuilder,Cli[T]>`_
  if b.flagName == "":
    raise newException(ValueError, "Flag name must not be empty")
  if b.flagName in cli.commands[command].flags:
    raise newException(ValueError, "Flag '" & b.flagName & "' already exists")
  for alias in b.aliases.items:
    assert alias != "", "Flag alias cannot be empty"
    if alias in cli.commands[command].flags:
      raise newException(ValueError, "Flag '" & alias & "' already exists")

  if b.flagParser.isNil():
    raise newException(ValueError, "Parser must be non-nil")
  assert b.flagParser.kind in {ParserKind.Flag, FlagOptionalValue}

  result = FlagId cli.addCommon(
    ParameterId command,
    b.flagName,
    b.usage,
    b.placeholder,
    b.flagParser
  )
  cli.commands[command].flags[b.flagName] = ParameterId result
  for alias in b.aliases.items:
    cli.commands[command].flags[alias] = ParameterId result
  if b.aliases.len > 0:
    cli.aliases[ParameterId result] = b.aliases

func addTo*[T](
  b: sink PositionalBuilder[T],
  cli: var Cli[T],
  command: CommandId = RootCommand,
): PositionalId {.discardable.} =
  ## Adds the positional parameter specified by `b` to `command`.
  ##
  ## A positional parameter can only be added to `command` if:
  ##
  ## - `command` does not contain any subcommands.
  ## - The parameter has a non-empty name.
  ## - The name is not used by any previously added positional parameters.
  ## - A parser was set for the parameter.
  ## - No catch-all parameter have been added yet.
  ## - For non-optional positionals, no optional parameters have been added yet.
  ##
  ## `ValueError` will be raised if any of the specified constraints are
  ## violated.
  ##
  ## See also:
  ## - `positionalBuilder proc <#positionalBuilder,Cli[T]>`_
  if b.posName == "":
    raise newException(ValueError, "Positional name should not be empty")
  for param in cli.commands[command].positionals.items:
    if b.posName == cli.names[param]:
      raise newException(ValueError):
        "Positional with name '" & cli.names[param] & "' already exists"

  if b.posParser == nil:
    raise newException(ValueError, "Parser must be non-nil")

  if cli.commands[command].hasSubcommand:
    raise newException(ValueError, "Cannot add positional parameters: command has subcommands")

  if cli.commands[command].hasPositional:
    let lastPos = cli.commands[command].positionals[^1]
    case cli.parsers[lastPos].kind
    of CatchAll, OptionalCatchAll:
      raise newException(ValueError):
        "Cannot add positional after catch all positional: " & cli.names[lastPos]
    of OptionalPositional:
      if not b.isOptional:
        raise newException(ValueError):
          "Cannot add non-optional positional after optional positional: " & cli.names[lastPos]
    else:
      discard "No constraints"

  let parser =
    if b.isOptional and b.isCatchAll:
      ParserAny[T](kind: OptionalCatchAll, posParser: b.posParser)
    elif b.isOptional:
      ParserAny[T](kind: OptionalPositional, posParser: b.posParser)
    elif b.isCatchAll:
      ParserAny[T](kind: CatchAll, posParser: b.posParser)
    else:
      ParserAny[T](kind: ParserKind.Positional, posParser: b.posParser)

  result = PositionalId cli.addCommon(
    ParameterId command,
    b.posName,
    b.usage,
    "",
    parser
  )
  cli.commands[command].positionals.add ParameterId(result)

func flagWithName*(cli: Cli, command: CommandId, name: string): Option[FlagId] =
  ## Returns the `FlagId` handle for the flag identifiable by `name` registered
  ## for `command`.
  runnableExamples:
    import std/options
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("string")
      .alias("str", "s")
      .parser(string, (_, val, var str) => (str = val))
      .addTo(cli)
    doAssert cli.flagWithName(RootCommand, "string") == some(strFlag)
    doAssert cli.flagWithName(RootCommand, "str") == some(strFlag)
    doAssert cli.flagWithName(RootCommand, "not-found") == none(FlagId)

  assert command in cli.commands, "Invalid command"
  try: some(FlagId cli.commands[command].flags[name])
  except KeyError: none FlagId

func commandWithName*(cli: Cli, command: CommandId, name: string): Option[CommandId] =
  ## Returns the `CommandId` handle for the subcommand identifiable by `name`
  ## registered for `command`.
  runnableExamples:
    import std/options

    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .alias("a", "do")
      .addTo(cli, RootCommand)

    doAssert cli.commandWithName(RootCommand, "a") == some(actCmd)
    doAssert cli.commandWithName(RootCommand, "act") == some(actCmd)
    doAssert cli.commandWithName(RootCommand, "not-found") == none(CommandId)

  assert command in cli.commands, "Invalid command"
  try: some(CommandId cli.commands[command].subcommands[name])
  except KeyError: none CommandId

func nameOf*(cli: Cli, flag: FlagId): lent string =
  ## Returns the canonical name for `flag`.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("string")
      .parser(string, (_, val, var str) => (str = val))
      .addTo(cli)
    doAssert cli.nameOf(strFlag) == "string"

  cli.names[ParameterId flag]

func nameOf*(cli: Cli, positional: PositionalId): lent string =
  ## Returns the canonical name for `positional`.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strPos = cli.positionalBuilder()
      .name("STR")
      .parser((v, var s) => (s = v))
      .addTo(cli)

    doAssert cli.nameOf(strPos) == "STR"

  cli.names[ParameterId positional]

func nameOf*(cli: Cli, command: CommandId): lent string =
  ## Returns the canonical name for `command`.
  runnableExamples:
    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)
    doAssert cli.nameOf(actCmd) == "act"

  cli.names[ParameterId command]

iterator namesOf*(cli: Cli, flag: FlagId): lent string =
  ## Returns all names that can be used to refer to `flag`. The canonical name
  ## is always returned first.
  runnableExamples:
    import std/sequtils
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("string")
      .alias("str", "s")
      .parser(string, (_, val, var str) => (str = val))
      .addTo(cli)
    doAssert toSeq(cli.namesOf(strFlag)) == ["string", "str", "s"]

  try:
    yield cli.names[ParameterId flag]
    for name in cli.aliases[ParameterId flag].items:
      yield name
  except KeyError:
    discard "Flag has no aliases"

iterator namesOf*(cli: Cli, command: CommandId): lent string =
  ## Returns all names that can be used to refer to `command`. The canonical
  ## name is always returned first.
  runnableExamples:
    import std/sequtils

    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .alias("a", "do")
      .addTo(cli, RootCommand)
    doAssert toSeq(cli.namesOf(actCmd)) == ["act", "a", "do"]

  try:
    yield cli.names[ParameterId command]
    for name in cli.aliases[ParameterId command].items:
      yield name
  except KeyError:
    discard "Command has no aliases"

func parentOf*(cli: Cli, command: CommandId): Option[CommandId] =
  ## Returns the parent command of `command`.
  ##
  ## `none(CommandId)` is only returned for `RootCommand`.
  runnableExamples:
    import std/options

    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)
    doAssert cli.parentOf(actCmd) == some(RootCommand)
    doAssert cli.parentOf(RootCommand) == none(CommandId)

  let parent = CommandId cli.parents[ParameterId command]
  if ParameterId(parent) == InvalidParameter:
    none CommandId
  else:
    some parent

func pathOf*(cli: Cli, command: CommandId): seq[CommandId] =
  ## Returns the path leading to `command`.
  runnableExamples:
    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)
    doAssert cli.pathOf(actCmd) == [RootCommand, actCmd]
    doAssert cli.pathOf(RootCommand) == [RootCommand]

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

func longNameOf*(cli: Cli, flag: FlagId): Option[string] =
  ## Returns the first long name of `flag`, if one exists.
  ##
  ## A long name is defined as a name longer than one character.
  runnableExamples:
    import std/options
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("s")
      .alias("str", "string")
      .parser(string, (_, val, var str) => (str = val))
      .addTo(cli)
    doAssert cli.longNameOf(strFlag) == some("str")

  result = none string
  for name in cli.namesOf(flag):
    if name.len > 1:
      return some name

func shortNameOf*(cli: Cli, flag: FlagId): Option[string] =
  ## Returns the first short name of `flag`, if one exists.
  ##
  ## A short name is defined as a one-character long name.
  runnableExamples:
    import std/options
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("string")
      .alias("str", "s")
      .parser(string, (_, val, var str) => (str = val))
      .addTo(cli)
    doAssert cli.shortNameOf(strFlag) == some("s")

  result = none string
  for name in cli.namesOf(flag):
    if name.len == 1:
      return some name

func usageOf*(cli: Cli, command: CommandId): lent string =
  ## Returns the `usage` for `command`, as described with `describe`.
  runnableExamples:
    import std/sugar

    let cli = commandBuilder(string)
      .describe("some usage")
      .initCli()
    doAssert cli.usageOf(RootCommand) == "some usage"

  cli.usages[ParameterId command]

func usageOf*(cli: Cli, flag: FlagId): lent string =
  ## Returns the `usage` for `flag`, as described with `describe`.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("string")
      .describe("a string")
      .parser((_, v, var s) => (s = v))
      .addTo(cli)
    doAssert cli.usageOf(strFlag) == "a string"

  cli.usages[ParameterId flag]

func usageOf*(cli: Cli, positional: PositionalId): lent string =
  ## Returns the `usage` for `positional`, as described with `describe`.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strPos = cli.positionalBuilder()
      .name("STR")
      .describe("a string")
      .parser((v, var s) => (s = v))
      .addTo(cli)
    doAssert cli.usageOf(strPos) == "a string"

  cli.usages[ParameterId positional]

func placeholderOf*(cli: Cli, flag: FlagId): string =
  ## Returns the `placeholder` for `flag`, as described with `describe`.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("string")
      .describe("a string", "STR")
      .parser((_, v, var s) => (s = v))
      .addTo(cli)
    let str2Flag = cli.flagBuilder()
      .name("string2")
      .describe("a string")
      .parser((_, v, var s) => (s = v))
      .addTo(cli)
    doAssert cli.placeholderOf(strFlag) == "STR"
    doAssert cli.placeholderOf(str2Flag) == ""

  cli.placeholders[ParameterId flag]

func hasSubcommand*(cli: Cli, command: CommandId): bool =
  ## Returns whether `command` contains subcommands.
  runnableExamples:
    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)

    doAssert cli.hasSubcommand(RootCommand)
    doAssert not cli.hasSubcommand(actCmd)

  cli.commands[command].hasSubcommand()

func hasDefaultSubcommand*(cli: Cli, command: CommandId): bool =
  ## Returns whether `command` has a default subcommand.
  runnableExamples:
    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)
    cli.commandBuilder()
      .name("def")
      .default()
      .addTo(cli, actCmd)

    doAssert not cli.hasDefaultSubcommand(RootCommand)
    doAssert cli.hasDefaultSubcommand(actCmd)

  cli.commands[command].hasDefaultSubcommand()

func defaultCommandOf*(cli: Cli, command: CommandId): Option[CommandId] =
  ## Returns the default subcommand of `command`.
  runnableExamples:
    import std/options

    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)
    let defCmd = cli.commandBuilder()
      .name("def")
      .default()
      .addTo(cli, actCmd)

    doAssert cli.defaultCommandOf(RootCommand) == none(CommandId)
    doAssert cli.defaultCommandOf(actCmd) == some(defCmd)

  if cli.commands[command].hasDefaultSubcommand():
    some(CommandId cli.commands[command].positionals[0])
  else:
    none(CommandId)

func classify*(cli: Cli, param: ParameterId): ParameterKind =
  ## Returns the type of `param`.
  runnableExamples:
    import std/sugar
    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("str")
      .parser((_, v, var s) => (s = v))
      .addTo(cli)
    let strPos = cli.positionalBuilder()
      .name("STR")
      .parser((v, var s) => (s = v))
      .addTo(cli)

    doAssert cli.classify(ParameterId RootCommand) == ParameterKind.Command
    doAssert cli.classify(ParameterId strFlag) == ParameterKind.Flag
    doAssert cli.classify(ParameterId strPos) == ParameterKind.Positional

  case cli.parsers[param].kind
  of ParserKind.Command:
    ParameterKind.Command
  of ParserKind.Flag, FlagOptionalValue:
    ParameterKind.Flag
  of ParserKind.Positional..OptionalCatchAll:
    ParameterKind.Positional

func isDefault*(cli: Cli, command: CommandId): bool =
  ## Returns whether `command` is the default subcommand of its parent.
  ##
  ## .. note::
  ##   This is always false for `RootCommand`.
  runnableExamples:
    var cli = commandBuilder(string)
      .initCli()
    let actCmd = cli.commandBuilder()
      .name("act")
      .addTo(cli, RootCommand)
    let defCmd = cli.commandBuilder()
      .name("def")
      .default()
      .addTo(cli, actCmd)

    doAssert not cli.isDefault(actCmd)
    doAssert cli.isDefault(defCmd)

  let parent = cli.parentOf(command)
  let default =
    if parent.isSome: cli.defaultCommandOf(parent.get)
    else: none(CommandId)

  default == some(command)

func isValueOptional*(cli: Cli, flag: FlagId): bool =
  ## Returns whether a value for `flag` is optional on the command line.
  runnableExamples:
    import std/sugar
    var cli = commandBuilder(string)
      .initCli()
    let strFlag = cli.flagBuilder()
      .name("str")
      .optionalParser((_, v, var s) => Action.Continue)
      .addTo(cli)
    let strReqFlag = cli.flagBuilder()
      .name("str-req")
      .parser((_, v, var s) => Action.Continue)
      .addTo(cli)
    let switchFlag = cli.flagBuilder()
      .name("switch")
      .parser(bool, (_, v, var s) => Action.Continue)
      .addTo(cli)

    doAssert cli.isValueOptional(strFlag)
    doAssert not cli.isValueOptional(strReqFlag)
    doAssert cli.isValueOptional(switchFlag)

  cli.parsers[ParameterId flag].kind == FlagOptionalValue

func isOptional*(cli: Cli, positional: PositionalId): bool =
  ## Returns whether a value for `positional` is optional on the command line.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let reqPos = cli.positionalBuilder()
      .name("REQ")
      .parser((v, var s) => (s = v))
      .addTo(cli)
    let optPos = cli.positionalBuilder()
      .name("OPT")
      .optional()
      .parser((v, var s) => (s = v))
      .addTo(cli)

    doAssert not cli.isOptional(reqPos)
    doAssert cli.isOptional(optPos)

  cli.parsers[ParameterId positional].kind in {OptionalPositional, OptionalCatchAll}

func isCatchAll*(cli: Cli, positional: PositionalId): bool =
  ## Returns whether `positional` is a catch all parameter.
  runnableExamples:
    import std/sugar

    var cli = commandBuilder(string)
      .initCli()
    let reqPos = cli.positionalBuilder()
      .name("REQ")
      .parser((v, var s) => (s = v))
      .addTo(cli)
    let anyPos = cli.positionalBuilder()
      .name("ANY")
      .catchAll()
      .parser((v, var s) => Action.Continue)
      .addTo(cli)

    doAssert not cli.isCatchAll(reqPos)
    doAssert cli.isCatchAll(anyPos)

  cli.parsers[ParameterId positional].kind in {CatchAll, OptionalCatchAll}

proc helpFlagBuilder*[T](
  cli: Cli[T],
  name: sink string = "help"
): FlagBuilder[T] =
  ## Builds a simple flag that triggers `Action.ShowHelp` when specified on the
  ## command line.
  ##
  ## The flag can be added directly to a `cli` or further customized.
  ##
  ## See also:
  ## - `addHelpFlag proc <#addHelpFlag,Cli[T],CommandId,sinkstring,varargs[string]>`_
  cli.flagBuilder
    .name(name)
    .optionalParser(
      proc (_: string, v: Option[string], r: var T): Action = ShowHelp
    )
    .describe("display help message")

proc addHelpFlag*[T](
  cli: var Cli[T],
  command: CommandId = RootCommand,
  name: sink string = "help",
  aliases: varargs[string] = []
): FlagId {.discardable.} =
  ## Adds a flag triggering `Action.ShowHelp` with the given `name` and
  ## `aliases`.
  ##
  ## See also:
  ## - `helpFlagBuilder proc <#helpFlagBuilder,Cli[T],sinkstring>`_
  cli.helpFlagBuilder
    .name(name)
    .alias(aliases)
    .addTo(cli, command)

func collectRemaining(lexer: var CmdLexer): seq[string] =
  for arg in lexer.remaining:
    result.add arg

func newHelpError(
  command: CommandId,
  paramName: sink string,
  param: ParameterId,
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
  command: CommandId,
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
  command: CommandId,
  flagName: sink string,
  flag: FlagId,
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
  command: CommandId,
  parent: ref ValueError,
  flagName: sink string,
  flag: FlagId,
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
  command: CommandId,
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
  command: CommandId,
  parent: ref ValueError,
  value: sink string,
  positional: PositionalId,
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
  command: CommandId,
  positional: PositionalId
): ref MissingPositionalError {.raises: [].} =
  (ref MissingPositionalError)(
    msg: "missing required value for positional parameter",
    command: command,
    positional: positional,
  )

func newUnknownCommandError(
  command: CommandId,
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
  command: CommandId,
  parent: ref ValueError,
  value: sink string,
  target: CommandId,
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
  command: CommandId,
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
  parameter: ParameterId,
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

func switchCommand(ctx: var ParseContext, command: CommandId) =
  ctx.command = command
  ctx.nextPositional = 0
  ctx.positionalCount = 0

func parsePositional[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  value: sink string,
) {.tailcall.} =
  let posLen = block: cli.commands[ctx.command].positionals.len
  if ctx.nextPositional < posLen:
    let
      posId = block: cli.commands[ctx.command].positionals[ctx.nextPositional]
      parser = cli.parsers[posId]
      action =
        try:
          parser.posParser(value, accumulator)
        except ValueError as e:
          raise newInvalidPositionalError(
            ctx.command,
            e,
            value,
            PositionalId posId,
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
  command: CommandId,
  input: sink string,
) {.tailcall.} =
  let
    parser = cli.parsers[ParameterId command]
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

  performAction(ctx, cli, accumulator, input, ParameterId command, action)

func parseCommand[T](
  ctx: var ParseContext,
  cli: Cli[T],
  accumulator: var T,
  value: sink string,
) {.tailcall.} =
  let cmd =
    try: cli.commands[ctx.command].subcommands[value]
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
  name: sink string,
) {.tailcall.} =
  let flagId =
    try: cli.commands[ctx.command].flags[name]
    except KeyError:
      raise newUnknownFlagError(
        ctx.command,
        name,
        ctx.lexer.value(delimitedOnly = true),
        collectRemaining ctx.lexer
      )

  let parser = cli.parsers[flagId]
  let action = block:
    let optValue = ctx.lexer.value(delimitedOnly = parser.kind == FlagOptionalValue)
    try:
      case parser.kind
      of FlagOptionalValue:
        parser.optParser(name, optValue, accumulator)
      of ParserKind.Flag:
        if optValue.isNone:
          raise newMissingValueError(
            ctx.command,
            name,
            FlagId flagId,
            collectRemaining ctx.lexer
          )

        parser.parser(name, optValue.unsafeGet(), accumulator)
      else:
        unreachable()
    except ValueError as e:
      raise newInvalidValueError(
        ctx.command,
        e,
        name,
        FlagId flagId,
        optValue,
        collectRemaining ctx.lexer
      )

  performAction(ctx, cli, accumulator, name, flagId, action)

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
        .map(proc (value: auto): auto = (cmdValue, value))
        .get(otherwise = (cmdEnd, ""))

  case kind
  of cmdLong, cmdShort:
    parseFlag(ctx, cli, accumulator, kind, option)
  of cmdValue:
    if not ctx.isValueOnly and option == "--":
      ctx.isValueOnly = true
      drop option
      parseNext(ctx, cli, accumulator)
    elif (block: cli.commands[ctx.command].hasSubcommand):
      parseCommand(ctx, cli, accumulator, option)
    else:
      parsePositional(ctx, cli, accumulator, option)
  of cmdEnd:
    # Verify that we collected all required parameters
    # TODO: remove this copy once tables return lent T
    let currentCommand = cli.commands[ctx.command]
    if currentCommand.hasDefaultSubcommand:
      let cmd = CommandId currentCommand.positionals[0]

      drop currentCommand
      drop option
      parseCommand(ctx, cli, accumulator, cmd, "")
    elif currentCommand.hasSubcommand:
      raise newMissingCommandError(ctx.command)
    elif ctx.nextPositional < currentCommand.positionals.len:
      let posId = currentCommand.positionals[ctx.nextPositional]
      case cli.parsers[posId].kind
      of ParserKind.Positional:
        raise newMissingPositionalError(ctx.command, PositionalId posId)
      of CatchAll:
        # Catch all hasn't collected any parameters
        if ctx.positionalCount <= ctx.nextPositional:
          raise newMissingPositionalError(ctx.command, PositionalId posId)
      of OptionalPositional, OptionalCatchAll:
        discard "nothing to do"
      else:
        unreachable()

func parse*[T](
  cli: Cli[T],
  accumulator: var T,
  args: sink seq[string],
) {.raises: [ParseError].} =
  ## Parses the given `args` list based on the description in `cli`, with
  ## configured parsers updating values in the `accumulator`.
  ##
  ## See also:
  ## - `run proc <#run,Cli[T],T,sinkseq[string],File>`_
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
  defaults: sink T = default(T),
): T {.inline, raises: [ParseError].} =
  ## Parses the given `args` list based on the description in `cli`, returning
  ## accumulated changes from configured parsers.
  ##
  ## An initial value for the internal accumulator can be specified using
  ## `defaults`.
  ##
  ## See also:
  ## - `run proc <#run,Cli[T],sinkseq[string],File,sinkT>`_
  result = defaults
  parse(cli, result, args)

iterator flags*(cli: Cli, command: CommandId): FlagId =
  ## Returns all flags for `command`.
  # Not the most efficient, but CLIs shouldn't be big enough for this to be an
  # issue
  for i in 0 ..< cli.parsers.nextId.int:
    if cli.parents[ParameterId i] == ParameterId(command) and
      cli.parsers[ParameterId i].kind in {ParserKind.Flag, FlagOptionalValue}:
      yield FlagId(i)

iterator positionals*(cli: Cli, command: CommandId): PositionalId =
  ## Returns all positional parameters for `command`.
  if not cli.commands[command].hasSubcommand:
    for i in cli.commands[command].positionals.items:
      yield PositionalId(i)

iterator subcommands*(cli: Cli, command: CommandId): CommandId =
  ## Returns all subcommands for `command`.
  for (param, parent) in cli.parents.pairs:
    if parent == ParameterId(command) and
      cli.parsers[param].kind == ParserKind.Command:
      yield CommandId(param)

func flagsUsage*(cli: Cli, command: CommandId): string =
  ## Produces a usage message for all flags registered for `command`.
  ##
  ## For each flag, the rendered message contains the first short and long
  ## name, a placeholder and the described `usage`.
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

func displayOf(cli: Cli, positional: PositionalId): string =
  ## Renders the display form of a positional parameter.
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

func positionalsUsage*(cli: Cli, command: CommandId): string =
  ## Produces a usage message for all positionals registered for `command`.
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
  command: CommandId,
  rootName: string = cli.nameOf(RootCommand)
): string =
  ## Produces a usage message for the given command. The message is meant to
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

  if cli.hasDefaultSubcommand(command):
    result.add " [COMMAND]"
  elif cli.hasSubcommand(command):
    result.add " <COMMAND>"

  for positional in cli.positionals(command):
    if result.len > 0:
      result.add ' '
    result.add cli.displayOf(positional)

func subcommandsUsage*(cli: Cli, command: CommandId): string =
  ## Produces a usage message for all subcommands registered for `command`.
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
    commands: seq[CommandId]
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

func help*(cli: Cli, command: CommandId, rootName: string = cli.nameOf(RootCommand)): string =
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
  ## Produces a pretty error message for `error`. The provided `error` must have
  ## been raised by `parse(cli)`.
  func prefixedFlag(name: string): string =
    if name.len > 1:
      "--" & name
    elif name == "-":
      "---" # The only unambiguous form of this flag
    else:
      "-" & name

  func formatFlag(cli: Cli[T], flag: FlagId, name: string): string =
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

when declared(os.commandLineParams) and declared(stdmsg):
  proc run*[T](
    cli: Cli[T],
    accumulator: var T,
    args: sink seq[string] = os.commandLineParams(),
    messageOutput: File = stdmsg,
  ) =
    ## Parses the command line `args` based on the description in `cli`, with
    ## configured parsers updating values in the `accumulator`.
    ##
    ## If an error occurs during parsing, the error message will be printed
    ## to `messageOutput` alongside helpful information and the command will
    ## terminate with a failure exit code automatically. The only exception
    ## to this is when `HelpError` occurs, of which the command will terminate
    ## with a successful exit code.
    ##
    ## See also:
    ## - `parse proc <#parse,Cli[T],T,sinkseq[string]>`_
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
    args: sink seq[string] = os.commandLineParams(),
    messageOutput: File = stdmsg,
    defaults: sink T = default(T),
  ): T =
    ## Parses the command line `args` based on the description in `cli`, returning
    ## accumulated changes from configured parsers.
    ##
    ## An initial value for the internal accumulator can be specified using
    ## `defaults`.
    ##
    ## See `run proc <#run,Cli[T],T,sinkseq[string],File>`_ for more information.
    ##
    ## See also:
    ## - `parse proc <#parse,Cli[T],sinkseq[string],sinkT>`_
    result = defaults
    run(cli, result, args, messageOutput)
