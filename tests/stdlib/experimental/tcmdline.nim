discard """
description: "Tests for the cmdline module"
"""

import std/setutils
import std/options
import std/sugar
import experimental/cmdline

type
  Custom = object
    ## Custom type to test custom typed parsers
    i: int

proc parseCli(T: typedesc[Custom], value: string): T =
  result.i = parseCli(int, value)

proc noop(k, v: auto, r: var auto): Action = discard
proc noop(v: auto, r: var auto): Action = discard

block typicalUsage:
  ## Samples of typical command lines
  block catLike:
    ## Unix's cat clone
    type
      Opt = enum
        ShowNonPrinting
        SqueezeBlank

      Config = object
        opts: set[Opt]
        files: seq[string]

    var cli = commandBuilder(Config)
      .name("cat")
      .initCli()
    cli.flagBuilder
      .name("show-nonprinting")
      .alias("v")
      .parser(bool, (k, v, var c) => (c.opts[ShowNonPrinting] = v))
      .describe("display non-printing characters")
      .addTo(cli)
    cli.flagBuilder
      .name("squeeze-blank")
      .alias("s")
      .parser(bool, (k, v, var c) => (c.opts[SqueezeBlank] = v))
      .describe("remove repeated empty lines")
      .addTo(cli)
    cli.positionalBuilder
      .name("FILE")
      .parser(string, (v, var c) => c.files.add v)
      .optional()
      .catchAll()
      .describe("file(s) to concatenate")
      .addTo(cli)

    block dashdash:
      let parsed = cli.parse @["--show-nonprinting", "--", "--squeeze-blank"]
      doAssert parsed.opts == {ShowNonPrinting}
      doAssert parsed.files == ["--squeeze-blank"]

    block basic:
      let parsed = cli.parse @["--show-nonprinting", "--squeeze-blank", "-"]
      doAssert parsed.opts == {ShowNonPrinting, SqueezeBlank}
      doAssert parsed.files == ["-"]

    block bool:
      let parsed = cli.parse @["--show-nonprinting=no", "-"]
      doAssert parsed.opts == {}
      doAssert parsed.files == ["-"]

    block short:
      let parsed = cli.parse @["-sv"]
      doAssert parsed.opts == {ShowNonPrinting, SqueezeBlank}

    block doc:
      doAssert cli.flagsUsage(RootCommand) == """
  -v, --show-nonprinting  display non-printing characters
  -s, --squeeze-blank     remove repeated empty lines"""

      doAssert cli.commandUsage(RootCommand) == "cat [OPTIONS] [FILE]..."
      doAssert cli.positionalsUsage(RootCommand) == """
  [FILE]...  file(s) to concatenate"""

      doAssert cli.help(RootCommand) == """
Usage: cat [OPTIONS] [FILE]...

Arguments:
  [FILE]...  file(s) to concatenate

Options:
  -v, --show-nonprinting  display non-printing characters
  -s, --squeeze-blank     remove repeated empty lines"""

  block seqLike:
    ## Unix's seq clone
    type
      Config = object
        format: Option[string]
        separator: string
        equalizeWidth: bool
        numbers: seq[int]

    var cli = commandBuilder(Config)
      .name("seq")
      .initCli()
    cli.flagBuilder
      .name("format")
      .parser(string, (_, v, var c) => (c.format = some(v)))
      .describe("use printf style FORMAT", "FORMAT")
      .addTo(cli)
    cli.flagBuilder
      .name("separator")
      .parser(string, (_, v, var c) => (c.separator = v))
      .describe("use STRING to separate numbers", "STRING")
      .addTo(cli)
    cli.flagBuilder
      .name("equal-width")
      .parser(bool, (_, v, var c) => (c.equalizeWidth = v))
      .describe("equalize width by padding with leading zeroes")
      .addTo(cli)
    cli.positionalBuilder
      .name("NUMBER")
      .parser(int, (v, var c) => c.numbers.add v)
      .catchAll()
      .addTo(cli)

    const baseConfig = Config(separator: "\\n")

    block basic:
      var parsed = baseConfig
      cli.parse(parsed, @["10", "--format:%02d"])
      doAssert parsed.format == some("%02d")
      doAssert parsed.separator == "\\n"
      doAssert not parsed.equalizeWidth
      doAssert parsed.numbers == [10]

    block:
      var parsed = baseConfig
      cli.parse(parsed, @["--equal-width", "10", "--format:"])
      doAssert parsed.format == some("")
      doAssert parsed.separator == "\\n"
      doAssert parsed.equalizeWidth
      doAssert parsed.numbers == [10]

    block separated:
      var parsed = baseConfig
      cli.parse(parsed, @["10", "--format", "--separator=;"])
      doAssert parsed.format == some("--separator=;")
      doAssert parsed.separator == "\\n"
      doAssert not parsed.equalizeWidth
      doAssert parsed.numbers == [10]

    block multiple:
      var parsed = baseConfig
      cli.parse(parsed, @["10", "--separator", ";", "100"])
      doAssert parsed.format == none(string)
      doAssert parsed.separator == ";"
      doAssert not parsed.equalizeWidth
      doAssert parsed.numbers == [10, 100]

    block doc:
      doAssert cli.flagsUsage(RootCommand) == """
  --format <FORMAT>     use printf style FORMAT
  --separator <STRING>  use STRING to separate numbers
  --equal-width         equalize width by padding with leading zeroes"""

      doAssert cli.commandUsage(RootCommand) == "seq [OPTIONS] <NUMBER>..."
      doAssert cli.positionalsUsage(RootCommand) == "  <NUMBER>..."

      doAssert cli.help(RootCommand) == """
Usage: seq [OPTIONS] <NUMBER>...

Arguments:
  <NUMBER>...

Options:
  --format <FORMAT>     use printf style FORMAT
  --separator <STRING>  use STRING to separate numbers
  --equal-width         equalize width by padding with leading zeroes"""

  block gitLike:
    ## git-style cli
    type
      Operation = enum
        NoOperation
        Clone
        RemoteList
        RemoteAdd
        RemoteShow

      CloneConfig = object
        url: string
        dest: string
        branch: string

      RemoteConfig = object
        name: string
        url: string
        branch: string

      SharedConfig = object
        verbose: bool

      Config = object
        shared: SharedConfig
        case op: Operation
        of Clone:
          cloneConfig: CloneConfig
        of RemoteAdd, RemoteShow:
          remoteConfig: RemoteConfig
        of RemoteList, NoOperation:
          discard

    var cli = commandBuilder(Config)
      .name("git")
      .initCli()
    cli.helpFlagBuilder()
      .addTo(cli)

    let cloneCmd = cli.commandBuilder()
      .name("clone")
      .alias("c")
      .describe("clone a repository")
      .parser((_, var c) => (c = Config(shared: c.shared, op: Clone)))
      .addTo(cli, RootCommand)
    cli.flagBuilder()
      .name("branch")
      .alias("b")
      .describe("checkout BRANCH instead of HEAD", "BRANCH")
      .parser(string, (_, v, var c) => (c.cloneConfig.branch = v))
      .addTo(cli, cloneCmd)
    cli.positionalBuilder()
      .name("REPO")
      .describe("repository to clone")
      .parser(string, (v, var c) => (c.cloneConfig.url = v))
      .addTo(cli, cloneCmd)
    cli.positionalBuilder()
      .name("DIR")
      .describe("output directory")
      .optional()
      .parser(string, (v, var c) => (c.cloneConfig.dest = v))
      .addTo(cli, cloneCmd)

    let remoteCmd = cli.commandBuilder()
      .name("remote")
      .alias("r")
      .describe("manage repository remote(s)")
      .addTo(cli, RootCommand)
    cli.flagBuilder()
      .name("verbose")
      .alias("v")
      .describe("print full URLs")
      .parser(bool, (_, v, var c) => (c.shared.verbose = v))
      .addTo(cli, remoteCmd)
    cli.commandBuilder()
      .name("list")
      .describe("list all remotes")
      .default()
      .parser((_, var c) => (c = Config(shared: c.shared, op: RemoteList)))
      .addTo(cli, remoteCmd)

    let remoteAddCmd = cli.commandBuilder()
      .name("add")
      .describe("add a new remote")
      .parser((_, var c) => (c = Config(shared: c.shared, op: RemoteAdd)))
      .addTo(cli, remoteCmd)
    cli.flagBuilder()
      .name("t")
      .describe("track only BRANCH", "BRANCH")
      .parser(string, (_, v, var c) => (c.remoteConfig.branch = v))
      .addTo(cli, remoteAddCmd)
    cli.positionalBuilder()
      .name("NAME")
      .describe("name of the new remote")
      .parser(string, (v, var c) => (c.remoteConfig.name = v))
      .addTo(cli, remoteAddCmd)
    cli.positionalBuilder()
      .name("URL")
      .describe("url of the new remote")
      .parser(string, (v, var c) => (c.remoteConfig.url = v))
      .addTo(cli, remoteAddCmd)

    let remoteShowCmd = cli.commandBuilder()
      .name("show")
      .describe("show information about remote")
      .parser((_, var c) => (c = Config(shared: c.shared, op: RemoteShow)))
      .addTo(cli, remoteCmd)
    cli.positionalBuilder()
      .name("NAME")
      .parser(string, (v, var c) => (c.remoteConfig.name = v))
      .addTo(cli, remoteShowCmd)

    block basic:
      var parsed = cli.parse(@["clone", "some-repo"])
      doAssert not parsed.shared.verbose
      doAssert parsed.op == Clone
      doAssert parsed.cloneConfig == CloneConfig(url: "some-repo")

      parsed = cli.parse(@["clone", "some-repo", "-b", "some-branch"])
      doAssert not parsed.shared.verbose
      doAssert parsed.op == Clone
      doAssert parsed.cloneConfig == CloneConfig(url: "some-repo", branch: "some-branch")

      parsed = cli.parse(@["remote"])
      doAssert not parsed.shared.verbose
      doAssert parsed.op == RemoteList

      parsed = cli.parse(@["remote", "-v", "list"])
      doAssert parsed.shared.verbose
      doAssert parsed.op == RemoteList

      parsed = cli.parse(@["remote", "add", "origin", "some-repo"])
      doAssert not parsed.shared.verbose
      doAssert parsed.op == RemoteAdd
      doAssert parsed.remoteConfig == RemoteConfig(name: "origin", url: "some-repo")

    block doc:
      doAssert cli.help(RootCommand) == """
Usage: git [OPTIONS] <COMMAND>

Commands:
  clone   clone a repository
  remote  manage repository remote(s)

Options:
  --help  display help message"""
      doAssert cli.help(remoteCmd) == """
manage repository remote(s)

Usage: git remote [OPTIONS] [COMMAND]

Commands:
  list  list all remotes [default]
  add   add a new remote
  show  show information about remote

Options:
  -v, --verbose  print full URLs"""
      doAssert cli.help(remoteAddCmd) == """
add a new remote

Usage: git remote add [OPTIONS] <NAME> <URL>

Arguments:
  <NAME>  name of the new remote
  <URL>   url of the new remote

Options:
  -t <BRANCH>  track only BRANCH"""

block flags:
  ## Tests for flags
  block:
    ## It is not possible to add duplicated flags
    var cli = commandBuilder(int).initCli()

    cli.flagBuilder
      .name("flag")
      .parser(noop)
      .addTo(cli)
    doAssertRaises(ValueError):
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .addTo(cli)

  block:
    ## Aliases also block duplicated flags
    var cli = commandBuilder(int).initCli()

    cli.flagBuilder
      .name("flag")
      .alias("f", "some-other-alias", "a")
      .parser(noop)
      .addTo(cli)
    doAssertRaises(ValueError):
      cli.flagBuilder
        .name("f")
        .parser(noop)
        .addTo(cli)
    doAssertRaises(ValueError):
      cli.flagBuilder
        .name("some-other-alias")
        .parser(noop)
        .addTo(cli)
    doAssertRaises(ValueError):
      cli.flagBuilder
        .name("unrelated")
        .alias("some-other-alias")
        .parser(noop)
        .addTo(cli)
    doAssertRaises(ValueError):
      cli.flagBuilder
        .name("unrelated-2")
        .alias("still-ok", "a")
        .parser(noop)
        .addTo(cli)

  block:
    ## `--` blocks stops all flag processing
    var cli = commandBuilder(bool).initCli()

    cli.flagBuilder
      .name("flag")
      .parser(bool, proc (_, v: auto, r: var auto): Action = r = v)
      .addTo(cli)
    cli.positionalBuilder
      .name("ANY")
      .parser(noop)
      .optional()
      .catchAll()
      .addTo(cli)

    let parsed = cli.parse @["--", "--flag"]
    doAssert not parsed

  block:
    ## Custom flag parser
    type
      Config = object
        o: Custom

    var cli = commandBuilder(Config).initCli()
    cli.flagBuilder
      .name("o")
      .parser(Custom, proc (_, v: auto, c: var auto): Action = c.o = v)
      .addTo(cli)
    let parsed = cli.parse @["-o", "10"]
    doAssert parsed.o.i == 10

  block:
    ## Flag behaviour tests
    type
      Config = object
        i: int
        n: Natural
        f: seq[float]
        s: string
        b: bool
        remaining: seq[string]

    var cli = commandBuilder(Config).initCli()
    cli.flagBuilder
      .name("nat")
      .optionalParser(Natural, proc (_, v: auto, c: var auto): Action = c.n = v.get(0xdead))
      .addTo(cli)
    let intFlag = cli.flagBuilder
      .name("int")
      .parser(int, proc (_, v: auto, c: var auto): Action = c.i = v)
      .addTo(cli)
    cli.flagBuilder
      .name("f")
      .parser(float, proc (_, v: auto, c: var auto): Action = c.f.add v)
      .addTo(cli)
    cli.flagBuilder
      .name("string")
      .parser(string, proc (_, v: auto, c: var auto): Action = c.s = v)
      .addTo(cli)
    cli.flagBuilder
      .name("bool")
      .parser(bool, proc (_, v: auto, c: var auto): Action = c.b = v)
      .addTo(cli)
    cli.addHelpFlag(name = "help")
    cli.positionalBuilder
      .name("ANY")
      .parser(string, proc(v: auto, c: var auto): Action = c.remaining.add v)
      .optional()
      .catchAll()
      .addTo(cli)

    const baseConfig = Config(f: @[42.0], s: "default")

    block simple:
      var parsed = baseConfig
      cli.parse(parsed, @["--f", "0", "--int", "42"])
      doAssert parsed.i == 42
      doAssert parsed.f == [42.0, 0.0]
      doAssert parsed.s == "default"
      doAssert not parsed.b

    block multiple:
      var parsed = baseConfig
      cli.parse(
        parsed,
        @["--int", "42", "-f", "420", "--int=100", "-f", "10.0"],
      )
      doAssert parsed.i == 100
      doAssert parsed.f == [42.0, 420, 10.0]

    block boolSpace:
      ## Boolean does not take non-delimited value
      let parsed = cli.parse @["--bool", "false"]
      doAssert parsed.b
      doAssert parsed.remaining == ["false"]

    block boolDelim:
      ## Boolean only take delimited values
      var parsed = Config(b: true)
      cli.parse(parsed, @["--bool:false"])
      doAssert not parsed.b

    block optionalSpace:
      ## Optional flag does not take non-delimited value
      let parsed = cli.parse @["--nat", "42"]
      doAssert parsed.n == 0xdead
      doAssert parsed.remaining == ["42"]

    block optionalDelim:
      ## Optional flag only take delimited value
      let parsed = cli.parse @["--nat=42"]
      doAssert parsed.n == 42

    block help:
      doAssertRaises(HelpError):
        discard cli.parse @["--help"]

    block unknown:
      ## Unregistered flags will trigger an error and stop processing
      var parsed: Config
      try:
        cli.parse(parsed, @["--unknown", "--int=10"])
        doAssert false, "expected UnknownFlagError"
      except UnknownFlagError as e:
        doAssert e.flagName == "unknown"
        doAssert e.flagValue == none string
        doAssert parsed.i == 0

      try:
        cli.parse(parsed, @["--unknown=something"])
        doAssert false, "expected UnknownFlagError"
      except UnknownFlagError as e:
        doAssert e.flagName == "unknown"
        doAssert e.flagValue == some("something")

    block novalue:
      ## Missing value will trigger an error
      var parsed: Config
      try:
        cli.parse(parsed, @["--bool", "--int"])
        doAssert false, "expected MissingValueError"
      except MissingValueError as e:
        doAssert e.flagName == "int"
        doAssert e.flag == intFlag
        doAssert parsed.b

    block wrongvalue:
      ## Invalid value also errors and stop processing
      var parsed: Config
      try:
        cli.parse(parsed, @["-f", "0", "--int=notint", "--string", "somestring"])
        doAssert false, "expected InvalidValueError"
      except InvalidValueError as e:
        doAssert e.parent of ValueError
        doAssert e.flagName == "int"
        doAssert e.flag == intFlag
        doAssert e.flagValue == some("notint")
        doAssert parsed.f == [0.0]
        doAssert parsed.s == ""

block positionals:
  ## Tests for positionals
  block:
    ## Disallow duplicate positional names
    var cli = commandBuilder(int).initCli()
    cli.positionalBuilder
      .name("ARG")
      .parser(noop)
      .addTo(cli)
    doAssertRaises(ValueError):
      cli.positionalBuilder
        .name("ARG")
        .parser(noop)
        .addTo(cli)

  block:
    ## No required positional might be added after an optional one
    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("OPT0")
        .parser(noop)
        .optional()
        .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("REQ")
          .parser(noop)
          .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("REQ")
          .catchAll()
          .parser(noop)
          .addTo(cli)
    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("OPT0")
        .parser(noop)
        .optional()
        .catchAll()
        .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("REQ")
          .parser(noop)
          .addTo(cli)
    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQ0")
        .parser(noop)
        .addTo(cli)
      cli.positionalBuilder
        .name("OPT0")
        .parser(noop)
        .optional()
        .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("REQ1")
          .parser(noop)
          .addTo(cli)

  block:
    ## No positional can be added after a catch all
    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("OPT0")
        .parser(noop)
        .optional()
        .catchAll()
        .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("OPT1")
          .parser(noop)
          .optional()
          .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("REQ")
          .parser(noop)
          .addTo(cli)
    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("OPT0")
        .parser(noop)
        .catchAll()
        .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("OPT1")
          .parser(noop)
          .optional()
          .addTo(cli)
      doAssertRaises(ValueError):
        cli.positionalBuilder
          .name("REQ")
          .parser(noop)
          .addTo(cli)

  block:
    ## No positional can be added to a dispatcher
    var cli = commandBuilder(bool).initCli()
    cli.commandBuilder()
      .name("x")
      .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.positionalBuilder()
        .name("REQ")
        .parser(noop)
        .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.positionalBuilder()
        .name("ANY")
        .parser(noop)
        .catchAll()
        .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.positionalBuilder()
        .name("OPT")
        .parser(noop)
        .optional()
        .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.positionalBuilder()
        .name("ANYOPT")
        .parser(noop)
        .optional()
        .catchAll()
        .addTo(cli, RootCommand)

  block:
    ## First `--` is never captured
    var cli = commandBuilder(seq[string]).initCli()

    cli.positionalBuilder
      .name("ANY")
      .parser(proc (v: auto, r: var auto): Action = r.add v)
      .optional()
      .catchAll()
      .addTo(cli)

    doAssert cli.parse(@["--"]) == []
    doAssert cli.parse(@["--", "--"]) == ["--"]
    doAssert cli.parse(@["--", "--", "--"]) == ["--", "--"]

  block:
    ## Custom positional parser
    type
      Config = object
        o: Custom

    var cli = commandBuilder(Config).initCli()
    cli.positionalBuilder
      .name("O")
      .parser(Custom, proc (v: auto, c: var auto): Action = c.o = v)
      .addTo(cli)
    let parsed = cli.parse @["10"]
    doAssert parsed.o.i == 10

  block:
    ## Action tests
    block:
      ## Help
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("HELP")
        .parser(proc (v: auto, c: var auto): Action = ShowHelp)
        .addTo(cli)
      doAssertRaises(HelpError):
        discard cli.parse @["help"]
    block:
      ## Stop flag processing
      var cli = commandBuilder(seq[string]).initCli()
      cli.positionalBuilder
        .name("STOP")
        .parser(proc (v: auto, c: var auto): Action = DisableFlagProcessing)
        .addTo(cli)
      cli.positionalBuilder
        .name("ANY")
        .catchAll()
        .parser(proc (v: auto, c: var auto): Action = c.add v)
        .addTo(cli)

      doAssert cli.parse(@["stop", "-n", "--not-a-flag", "data"]) == ["-n", "--not-a-flag", "data"]

  block:
    ## Optional is optional
    block:
      var cli = commandBuilder(string).initCli()
      cli.positionalBuilder
        .name("OPT")
        .optional()
        .parser(proc (v: auto, c: var auto): Action = c = v)
        .addTo(cli)

      doAssert cli.parse(@[]) == ""

    block:
      var cli = commandBuilder(seq[string]).initCli()
      cli.positionalBuilder
        .name("OPT")
        .catchAll()
        .optional()
        .parser(proc (v: auto, c: var auto): Action = c.add v)
        .addTo(cli)

      doAssert cli.parse(@[]) == []

  block:
    ## Required catch all is required
    block:
      var cli = commandBuilder(seq[string]).initCli()
      let any = cli.positionalBuilder
        .name("ANY")
        .catchAll()
        .parser(proc (v: auto, c: var auto): Action = c.add v)
        .addTo(cli)

      try:
        discard cli.parse(@[])
        doAssert false, "expected MissingPositionalError"
      except MissingPositionalError as e:
        doAssert e.positionalValue == ""
        doAssert e.positional == any

      doAssert cli.parse(@["one"]) == ["one"]
      doAssert cli.parse(@["one", "two"]) == ["one", "two"]

  block:
    ## Behaviour tests
    type Config = object
      flag: int
      req0: bool
      req1: float
      opt: Natural
      rest: seq[string]

    var cli = commandBuilder(Config).initCli()
    cli.flagBuilder
      .name("flag")
      .alias("f")
      .parser(int, proc (_, v: auto, c: var auto): Action = c.flag = v)
      .addTo(cli)
    let req0 = cli.positionalBuilder
      .name("REQ0")
      .parser(bool, proc (v: auto, c: var auto): Action = c.req0 = v)
      .addTo(cli)
    let req1 = cli.positionalBuilder
      .name("REQ1")
      .parser(float, proc (v: auto, c: var auto): Action = c.req1 = v)
      .addTo(cli)
    let opt = cli.positionalBuilder
      .name("OPT")
      .optional()
      .parser(Natural, proc (v: auto, c: var auto): Action = c.opt = v)
      .addTo(cli)
    cli.positionalBuilder
      .name("ANY")
      .parser(proc (v: auto, c: var auto): Action = c.rest.add v)
      .optional()
      .catchAll()
      .addTo(cli)

    block:
      ## Basic positional-only cases
      doAssert cli.parse(@["1", "42e10"]) == Config(req0: true, req1: 42e10)
      doAssert cli.parse(@["1", "42e10", "42"]) == Config(
        req0: true,
        req1: 42e10,
        opt: 42,
      )
      doAssert cli.parse(@["1", "42e10", "42", "one", "two"]) == Config(
        req0: true,
        req1: 42e10,
        opt: 42,
        rest: @["one", "two"],
      )

    block:
      ## Test interactions with flags
      doAssert cli.parse(@["1", "-f", "10", "42e10"]) == Config(flag: 10, req0: true, req1: 42e10)
      doAssert cli.parse(@["1", "42e10", "--flag=11", "12"]) == Config(
        flag: 11,
        req0: true,
        req1: 42e10,
        opt: 12,
      )
      doAssert cli.parse(@["1", "42e10", "--flag=11", "12", "one", "-f", "20", "two"]) == Config(
        flag: 20,
        req0: true,
        req1: 42e10,
        opt: 12,
        rest: @["one", "two"],
      )

    block:
      ## `--` should stop flag processing
      doAssert cli.parse(@["1", "-f", "10", "42e10", "--", "11", "--flag", "ordering"]) == Config(
        flag: 10,
        req0: true,
        req1: 42e10,
        opt: 11,
        rest: @["--flag", "ordering"],
      )

    block:
      ## Required parameters are required
      try:
        discard cli.parse(@[])
        doAssert false, "expected MissingPositionalError"
      except MissingPositionalError as e:
        doAssert e.positionalValue == ""
        doAssert e.positional == req0

      block:
        ## Verify that data is accumulated on failure
        var parsed: Config
        try:
          cli.parse(parsed, @["on", "-f10"])
          doAssert false, "expected MissingPositionalError"
        except MissingPositionalError as e:
          doAssert e.positionalValue == ""
          doAssert e.positional == req1

        doAssert parsed == Config(req0: true, flag: 10)

    block:
      ## Parse errors are reported
      var parsed: Config
      try:
        cli.parse(parsed, @["off", "-f10", "3", "notnumber", "--", "--wont-get-here"])
        doAssert false, "expected InvalidPositionalError"
      except InvalidPositionalError as e:
        doAssert e.parent of ValueError
        doAssert e.positionalValue == "notnumber"
        doAssert e.positional == opt

      doAssert parsed == Config(flag: 10, req1: 3)

      block:
        ## `--` does not alter position count
        try:
          discard cli.parse(@["off", "--", "-f10"])
          doAssert false, "expected InvalidPositionalError"
        except InvalidPositionalError as e:
          doAssert e.parent of ValueError
          doAssert e.positionalValue == "-f10"
          doAssert e.positional == req1

block commands:
  ## Tests for commands
  block:
    ## Disallow duplicate commands
    var cli = commandBuilder(bool).initCli()
    cli.commandBuilder()
      .name("foo")
      .parser(noop)
      .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.commandBuilder()
        .name("foo")
        .parser(noop)
        .addTo(cli, RootCommand)

  block:
    ## Aliases cannot be duplicated
    var cli = commandBuilder(bool).initCli()
    cli.commandBuilder()
      .name("foo")
      .addTo(cli, RootCommand)
    cli.commandBuilder()
      .name("foobar")
      .alias("fb", "f-b")
      .addTo(cli, RootCommand)

    doAssertRaises(ValueError):
      cli.commandBuilder()
        .name("bar")
        .alias("foo")
        .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.commandBuilder()
        .name("fb")
        .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.commandBuilder()
        .name("unrelated")
        .alias("still-unrelated", "f-b")
        .addTo(cli, RootCommand)

  block:
    ## Command with positionals cannot have subcommands
    block:
      ## Required positional
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder()
        .name("X")
        .parser(noop)
        .addTo(cli, RootCommand)
      doAssertRaises(ValueError):
        cli.commandBuilder()
          .name("foo")
          .addTo(cli, RootCommand)

    block:
      ## Optional positional
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder()
        .name("X")
        .parser(noop)
        .optional()
        .addTo(cli, RootCommand)
      doAssertRaises(ValueError):
        cli.commandBuilder()
          .name("foo")
          .addTo(cli, RootCommand)

  block:
    ## Only one default subcommand can be registered
    var cli = commandBuilder(bool).initCli()
    cli.commandBuilder()
      .name("foo")
      .default()
      .addTo(cli, RootCommand)
    doAssertRaises(ValueError):
      cli.commandBuilder()
        .name("bar")
        .default()
        .addTo(cli, RootCommand)

  block:
    ## Action tests
    block:
      ## Help
      var cli = commandBuilder(bool).initCli()
      let notHelpCmd = cli.commandBuilder()
        .name("not-help")
        .parser(proc (_: auto, b: var auto): Action = ShowHelp)
        .addTo(cli, RootCommand)
      try:
        discard cli.parse @["not-help"]
        unreachable("HelpError should be raised")
      except HelpError as e:
        doAssert e.command == notHelpCmd
        doAssert e.param == Parameter(notHelpCmd)
        doAssert e.paramName == "not-help"

    block:
      ## Disable flags
      var cli = commandBuilder(seq[string]).initCli()
      let eatCmd = cli.commandBuilder()
        .name("eat")
        .parser(proc (_: auto, s: var auto): Action = DisableFlagProcessing)
        .addTo(cli, RootCommand)
      cli.positionalBuilder()
        .name("ANY")
        .optional()
        .catchAll()
        .parser(proc (v: auto, s: var auto): Action = s.add v)
        .addTo(cli, eatCmd)
      doAssert cli.parse(@["eat", "--foo", "--bar"]) == ["--foo", "--bar"]

  block:
    ## Default command parsers propel forward automatically
    var count = 0
    template expectAndInc(expected: int) =
      bind count
      doAssert count == expected:
        "Expected " & $expected & " but got: " & $count
      inc count

    var cli = commandBuilder(bool).initCli()
    let fooCmd = cli.commandBuilder()
      .name("foo")
      .default()
      .parser(proc (_: auto, b: var auto): Action = expectAndInc(0))
      .addTo(cli, RootCommand)
    let barCmd = cli.commandBuilder()
      .name("bar")
      .default()
      .parser(proc (_: auto, b: var auto): Action = expectAndInc(1))
      .addTo(cli, fooCmd)
    cli.commandBuilder()
      .name("foobar")
      .default()
      .parser(proc (_: auto, b: var auto): Action = expectAndInc(2))
      .addTo(cli, barCmd)

    discard cli.parse(@[])
    expectAndInc(3)

  block:
    ## Default respects action
    var cli = commandBuilder(bool).initCli()
    let fooCmd = cli.commandBuilder()
      .name("foo")
      .default()
      .addTo(cli, RootCommand)
    let barCmd = cli.commandBuilder()
      .name("bar")
      .default()
      .parser(proc (_: auto, b: var auto): Action = ShowHelp)
      .addTo(cli, fooCmd)
    try:
      discard cli.parse @[]
      unreachable("HelpError should be raised")
    except HelpError as e:
      doAssert e.command == barCmd
      doAssert e.param == Parameter(barCmd)
      doAssert e.paramName == ""

  block:
    ## Dispatcher without default requires a command
    var cli = commandBuilder(bool).initCli()
    let fooCmd = cli.commandBuilder()
      .name("foo")
      .addTo(cli, RootCommand)
    cli.commandBuilder()
      .name("bar")
      .addTo(cli, fooCmd)

    try:
      discard cli.parse(@[])
      unreachable("MissingCommandError should be raised")
    except MissingCommandError as e:
      doAssert e.command == RootCommand
      doAssert e.commandName == ""

    try:
      discard cli.parse(@["foo"])
      unreachable("MissingCommandError should be raised")
    except MissingCommandError as e:
      doAssert e.command == fooCmd
      doAssert e.commandName == ""

  block:
    ## Behaviour tests
    type
      RootConfig = object
        b: bool
        i: int
      FooConfig = object
        b: bool
        s: string
      BarConfig = object
        s: string
        x: Custom
      OtherConfig = object
        a: int
        s: string
      Cmd = enum
        cmdRoot
        cmdFoo
        cmdBar
        cmdTmp
        cmdOther
      Config = object
        cmd: Cmd
        root: RootConfig
        foo: FooConfig
        bar: BarConfig
        other: OtherConfig

    var cli = commandBuilder(Config)
      .initCli()
    cli.flagBuilder()
      .name("b")
      .parser(bool, proc (_, v: auto, c: var auto): Action = c.root.b = v)
      .addTo(cli, RootCommand)
    cli.flagBuilder()
      .name("i")
      .parser(int, proc (_, v: auto, c: var auto): Action = c.root.i = v)
      .addTo(cli, RootCommand)
    cli.addHelpFlag(RootCommand)
    let fooCmd = cli.commandBuilder()
      .name("foo")
      .alias("f")
      .default()
      .parser(proc (_: auto, c: var auto): Action = c.cmd = cmdFoo)
      .addTo(cli, RootCommand)
    cli.flagBuilder()
      .name("b")
      .parser(bool, proc (_, v: auto, c: var auto): Action = c.foo.b = v)
      .addTo(cli, fooCmd)
    cli.flagBuilder()
      .name("s")
      .parser(string, proc (_, v: auto, c: var auto): Action = c.foo.s = v)
      .addTo(cli, fooCmd)
    let barCmd = cli.commandBuilder()
      .name("bar")
      .alias("b")
      .parser(proc (_: auto, c: var auto): Action = c.cmd = cmdBar)
      .addTo(cli, fooCmd)
    cli.positionalBuilder()
      .name("S")
      .parser(string, proc (v: auto, c: var auto): Action = c.bar.s = v)
      .addTo(cli, barCmd)
    cli.positionalBuilder()
      .name("X")
      .parser(Custom, proc (v: auto, c: var auto): Action = c.bar.x = v)
      .addTo(cli, barCmd)
    cli.addHelpFlag(barCmd)
    let tmpCmd = cli.commandBuilder()
      .name("tmp")
      .parser(proc (_: auto, c: var auto): Action = c.cmd = cmdTmp)
      .addTo(cli, RootCommand)
    cli.addHelpFlag(tmpCmd)
    let otherCmd = cli.commandBuilder()
      .name("other")
      .default()
      .parser(proc (_: auto, c: var auto): Action = c.cmd = cmdOther)
      .addTo(cli, tmpCmd)
    cli.flagBuilder()
      .name("str")
      .parser(string, proc (_, v: auto, c: var auto): Action = c.other.s = v)
      .addTo(cli, otherCmd)
    cli.positionalBuilder()
      .name("A")
      .parser(int, proc (v: auto, c: var auto): Action = c.other.a = v)
      .addTo(cli, otherCmd)
    let rejectCmd = cli.commandBuilder()
      .name("reject")
      .alias("r")
      .parser(proc (_: auto, c: var auto): Action = raise newException(ValueError, "rejected"))
      .addTo(cli, RootCommand)
    let rejectDefCmd = cli.commandBuilder()
      .name("reject-default")
      .addTo(cli, RootCommand)
    let rejectDefImplCmd = cli.commandBuilder()
      .name("default")
      .parser(proc (_: auto, c: var auto): Action = raise newException(ValueError, "rejected"))
      .default()
      .addTo(cli, rejectDefCmd)

    block:
      ## Basic happy cases
      doAssert cli.parse(@["foo", "bar", "x", "1"]) == Config(
        cmd: cmdBar,
        bar: BarConfig(s: "x", x: Custom(i: 1)),
      )
      doAssert cli.parse(@["-b", "foo", "-sstuff", "bar", "x", "1"]) == Config(
        cmd: cmdBar,
        root: RootConfig(b: true),
        foo: FooConfig(s: "stuff"),
        bar: BarConfig(s: "x", x: Custom(i: 1)),
      )
      doAssert cli.parse(@["tmp", "other", "10"]) == Config(
        cmd: cmdOther,
        other: OtherConfig(a: 10)
      )

    block:
      ## `--` stop flag processing
      doAssert cli.parse(@["--", "foo", "bar", "-b", "10"]) == Config(
        cmd: cmdBar,
        bar: BarConfig(s: "-b", x: Custom(i: 10)),
      )
      doAssert cli.parse(@["foo", "-b", "--", "bar", "--", "42"]) == Config(
        cmd: cmdBar,
        foo: FooConfig(b: true),
        bar: BarConfig(s: "--", x: Custom(i: 42)),
      )

    block:
      ## Unknown command
      try:
        discard cli.parse(@["x"])
        unreachable("UnknownCommandError should be raised")
      except UnknownCommandError as e:
        doAssert e.command == RootCommand
        doAssert e.commandName == "x"

      try:
        discard cli.parse(@["foo", "x"])
        unreachable("UnknownCommandError should be raised")
      except UnknownCommandError as e:
        doAssert e.command == fooCmd
        doAssert e.commandName == "x"

    block:
      ## Rejected command
      try:
        discard cli.parse(@["reject"])
        unreachable("InvalidCommandError should be raised")
      except InvalidCommandError as e:
        doAssert e.parent.msg == "rejected"
        doAssert e.command == RootCommand
        doAssert e.commandName == "reject"
        doAssert e.targetCommand == rejectCmd

      try:
        discard cli.parse(@["r"])
        unreachable("InvalidCommandError should be raised")
      except InvalidCommandError as e:
        doAssert e.parent.msg == "rejected"
        doAssert e.command == RootCommand
        doAssert e.commandName == "r"
        doAssert e.targetCommand == rejectCmd

    block:
      ## Flag scoping
      block:
        ## Expected
        doAssert cli.parse(@["foo", "-b", "bar", "x", "1"]) == Config(
          cmd: cmdBar,
          foo: FooConfig(b: true),
          bar: BarConfig(s: "x", x: Custom(i: 1)),
        )
        doAssert cli.parse(@["-b", "foo", "-b=false", "b", "x", "1"]) == Config(
          cmd: cmdBar,
          root: RootConfig(b: true),
          bar: BarConfig(s: "x", x: Custom(i: 1)),
        )

      block:
        ## Help stops at first occurrance
        try:
          discard cli.parse(@["--help", "foo"])
          unreachable("HelpError should be raised")
        except HelpError as e:
          doAssert e.command == RootCommand
          doAssert e.remaining == ["foo"]

        try:
          discard cli.parse(@["f", "b", "--help"])
          unreachable("HelpError should be raised")
        except HelpError as e:
          doAssert e.command == barCmd
          doAssert e.remaining == []

        try:
          discard cli.parse(@["tmp", "--help"])
          unreachable("HelpError should be raised")
        except HelpError as e:
          doAssert e.command == tmpCmd
          doAssert e.remaining == []

      block:
        ## Subcommand cannot access upper level flags
        try:
          discard cli.parse(@["f", "-i=10", "bar", "x", "1"])
          unreachable("UnknownFlagError should be raised")
        except UnknownFlagError as e:
          doAssert e.command == fooCmd
          doAssert e.flagName == "i"
          doAssert e.flagValue == some("10")
          doAssert e.remaining == ["bar", "x", "1"]

        try:
          discard cli.parse(@["foo", "bar", "-b", "x", "1"])
          unreachable("UnknownFlagError should be raised")
        except UnknownFlagError as e:
          doAssert e.command == barCmd
          doAssert e.flagName == "b"
          doAssert e.remaining == ["x", "1"]

    block:
      ## Default interactions
      block:
        ## Default into dispatcher without default errors
        try:
          discard cli.parse(@[])
          unreachable("MissingCommandError should be raised")
        except MissingCommandError as e:
          doAssert e.command == fooCmd

      block:
        ## Default into required positional errors
        try:
          discard cli.parse(@["tmp"])
          unreachable("MissingPositionalError should be raised")
        except MissingPositionalError as e:
          doAssert e.command == otherCmd

      block:
        ## Default into rejected error
        try:
          discard cli.parse(@["reject-default"])
          unreachable("InvalidCommandError should be raised")
        except InvalidCommandError as e:
          doAssert e.parent.msg == "rejected"
          doAssert e.command == rejectDefCmd
          doAssert e.commandName == ""
          doAssert e.targetCommand == rejectDefImplCmd

      block:
        ## It is not possible to specify parameters for default command
        try:
          discard cli.parse(@["tmp", "--str", "value"])
          unreachable("UnknownFlagError should be raised")
        except UnknownFlagError as e:
          doAssert e.flagName == "str"
          doAssert e.flagValue == none string

        try:
          discard cli.parse(@["tmp", "10"])
          unreachable("UnknownCommandError should be raised")
        except UnknownCommandError as e:
          doAssert e.command == tmpCmd
          doAssert e.commandName == "10"

block docgen:
  block:
    ## Just one flag
    block:
      ## Short flag
      var cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .describe("", "INT")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v[=<INT>]"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .describe("some description")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v  some description"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .describe("some description", "INT")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v[=<INT>]  some description"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .parser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v <VALUE>"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .parser(noop)
        .describe("some description")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v <VALUE>  some description"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("v")
        .parser(noop)
        .describe("some description", "INT")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  -v <INT>  some description"

    block:
      ## Long flag
      var cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .describe("", "VALUE")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag[=<VALUE>]"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .describe("flag something", "VALUE")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag[=<VALUE>]  flag something"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag <VALUE>"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .describe("flag something")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag <VALUE>  flag something"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .describe("flag something", "FLAG,...")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag <FLAG,...>  flag something"

      cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .describe("flag something", "VALUE")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == "  --flag[=<VALUE>]  flag something"

    block:
      ## Aliases
      block:
        ## Check render ordering
        # Use the first short and first long flag, in that exact order
        var cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("flag")
          .alias("other-long", "c")
          .optionalParser(noop)
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag"

        cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .optionalParser(noop)
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag"

      block description:
        ## Check multi-flag description render
        var cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .parser(noop)
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag <VALUE>"

        cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .optionalParser(noop)
          .describe("", "VALUE")
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag[=<VALUE>]"

        cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .optionalParser(noop)
          .describe("flag something", "VALUE")
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag[=<VALUE>]  flag something"

        cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .parser(noop)
          .describe("flag something", "")
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag <VALUE>  flag something"

        cli = commandBuilder(int).initCli()
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .parser(noop)
          .describe("flag something", "FLAG,...")
          .addTo(cli)

        doAssert cli.flagsUsage(RootCommand) == "  -c, --flag <FLAG,...>  flag something"

  block:
    ## Multiple flags
    block:
      ## Order by addition time
      var cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("c")
        .alias("flag", "f")
        .parser(noop)
        .describe("flag something", "")
        .addTo(cli)

      cli.flagBuilder
        .name("a")
        .optionalParser(noop)
        .describe("show all")
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == """
  -c, --flag <VALUE>  flag something
  -a                  show all"""

    block:
      ## Column alignment
      var cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("c")
        .alias("flag", "f")
        .parser(noop)
        .describe("flag something", "")
        .addTo(cli)

      cli.flagBuilder
        .name("a")
        .optionalParser(noop)
        .describe("show all")
        .addTo(cli)

      cli.flagBuilder
        .name("yes-i-know-what-i-am-doing")
        .parser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage(RootCommand) == """
  -c, --flag <VALUE>                    flag something
  -a                                    show all
  --yes-i-know-what-i-am-doing <VALUE>"""

    block:
      ## Flag-only help
      var cli = commandBuilder(int).initCli()
      cli.flagBuilder
        .name("c")
        .alias("flag", "f")
        .parser(noop)
        .describe("flag something", "")
        .addTo(cli)

      cli.flagBuilder
        .name("a")
        .optionalParser(noop)
        .describe("show all")
        .addTo(cli)

      cli.flagBuilder
        .name("yes-i-know-what-i-am-doing")
        .parser(noop)
        .addTo(cli)

      doAssert cli.help(RootCommand, "cmd") == """
Usage: cmd [OPTIONS]

Options:
  -c, --flag <VALUE>                    flag something
  -a                                    show all
  --yes-i-know-what-i-am-doing <VALUE>"""

  block:
    ## Empty
    let cli = commandBuilder(bool).initCli()
    doAssert cli.flagsUsage(RootCommand) == ""
    doAssert cli.positionalsUsage(RootCommand) == ""
    doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS]"
    doAssert cli.commandUsage(RootCommand, "something") == "something [OPTIONS]"
    doAssert cli.subcommandsUsage(RootCommand) == ""
    doAssert cli.help(RootCommand, "") == "Usage: [OPTIONS]"
    doAssert cli.help(RootCommand, "something") == "Usage: something [OPTIONS]"

  block:
    ## Just one positional
    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQUIRED")
        .parser(noop)
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  <REQUIRED>"
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] <REQUIRED>"

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQUIRED")
        .parser(noop)
        .describe("a required parameter")
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  <REQUIRED>  a required parameter"
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] <REQUIRED>"

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQANY")
        .catchAll()
        .parser(noop)
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  <REQANY>..."
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] <REQANY>..."

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQANY")
        .catchAll()
        .parser(noop)
        .describe("many required parameter(s)")
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  <REQANY>...  many required parameter(s)"
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] <REQANY>..."

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("NOT-REQUIRED")
        .parser(noop)
        .optional()
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  [NOT-REQUIRED]"
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] [NOT-REQUIRED]"

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("NOT-REQUIRED")
        .parser(noop)
        .optional()
        .describe("not at all required")
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  [NOT-REQUIRED]  not at all required"
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] [NOT-REQUIRED]"

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("NOT-REQUIRED-MANY")
        .parser(noop)
        .optional()
        .catchAll()
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  [NOT-REQUIRED-MANY]..."
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] [NOT-REQUIRED-MANY]..."

    block:
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("NOT-REQUIRED-MANY")
        .parser(noop)
        .optional()
        .catchAll()
        .describe("many param(s)")
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == "  [NOT-REQUIRED-MANY]...  many param(s)"
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] [NOT-REQUIRED-MANY]..."

  block:
    ## Many positionals
    block:
      ## Ordered by addition
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQ")
        .parser(noop)
        .addTo(cli)
      cli.positionalBuilder
        .name("NOT-REQ")
        .parser(noop)
        .optional()
        .addTo(cli)
      cli.positionalBuilder
        .name("MANY-NOT-REQ")
        .parser(noop)
        .optional()
        .catchAll()
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == """
  <REQ>
  [NOT-REQ]
  [MANY-NOT-REQ]..."""
      doAssert cli.commandUsage(RootCommand, "") == "[OPTIONS] <REQ> [NOT-REQ] [MANY-NOT-REQ]..."

    block:
      ## Aligned into columns
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQ")
        .parser(noop)
        .describe("a required parameter")
        .addTo(cli)
      cli.positionalBuilder
        .name("NOT-REQ-AT-ALL")
        .parser(noop)
        .optional()
        .describe("an optional parameter")
        .addTo(cli)
      cli.positionalBuilder
        .name("MANY-NOT-REQ")
        .parser(noop)
        .optional()
        .catchAll()
        .describe("many optional parameter(s)")
        .addTo(cli)

      doAssert cli.positionalsUsage(RootCommand) == """
  <REQ>              a required parameter
  [NOT-REQ-AT-ALL]   an optional parameter
  [MANY-NOT-REQ]...  many optional parameter(s)"""

    block:
      ## Positional-only help
      var cli = commandBuilder(bool).initCli()
      cli.positionalBuilder
        .name("REQ")
        .parser(noop)
        .describe("a required parameter")
        .addTo(cli)
      cli.positionalBuilder
        .name("NOT-REQ-AT-ALL")
        .parser(noop)
        .optional()
        .describe("an optional parameter")
        .addTo(cli)
      cli.positionalBuilder
        .name("MANY-NOT-REQ")
        .parser(noop)
        .optional()
        .catchAll()
        .describe("many optional parameter(s)")
        .addTo(cli)

      doAssert cli.help(RootCommand, "cmd") == """
Usage: cmd [OPTIONS] <REQ> [NOT-REQ-AT-ALL] [MANY-NOT-REQ]...

Arguments:
  <REQ>              a required parameter
  [NOT-REQ-AT-ALL]   an optional parameter
  [MANY-NOT-REQ]...  many optional parameter(s)"""

  block:
    ## Just one subcommand
    block:
      ## No usage or default
      var cli = commandBuilder(bool).initCli()
      let fooCmd = cli.commandBuilder()
        .name("foo")
        .addTo(cli, RootCommand)
      doAssert cli.commandUsage(RootCommand) == "[OPTIONS] <COMMAND>"
      doAssert cli.commandUsage(fooCmd) == "foo [OPTIONS]"
      doAssert cli.help(fooCmd) == "Usage: foo [OPTIONS]"
      doAssert cli.subcommandsUsage(RootCommand) == "  foo"

    block:
      ## No usage but is default
      var cli = commandBuilder(bool).initCli()
      let fooCmd = cli.commandBuilder()
        .name("foo")
        .default()
        .addTo(cli, RootCommand)
      doAssert cli.commandUsage(RootCommand) == "[OPTIONS] [COMMAND]"
      doAssert cli.commandUsage(fooCmd) == "foo [OPTIONS]"
      doAssert cli.help(fooCmd) == "Usage: foo [OPTIONS]"
      doAssert cli.subcommandsUsage(RootCommand) == "  foo  [default]"

    block:
      ## Usage no default
      var cli = commandBuilder(bool).initCli()
      let fooCmd = cli.commandBuilder()
        .name("foo")
        .describe("do things")
        .addTo(cli, RootCommand)
      doAssert cli.commandUsage(RootCommand) == "[OPTIONS] <COMMAND>"
      doAssert cli.commandUsage(fooCmd) == "foo [OPTIONS]"
      doAssert cli.help(fooCmd) == """
do things

Usage: foo [OPTIONS]"""
      doAssert cli.subcommandsUsage(RootCommand) == "  foo  do things"

    block:
      ## Usage and default
      var cli = commandBuilder(bool).initCli()
      let fooCmd = cli.commandBuilder()
        .name("foo")
        .describe("do things")
        .default()
        .addTo(cli, RootCommand)
      doAssert cli.commandUsage(RootCommand) == "[OPTIONS] [COMMAND]"
      doAssert cli.commandUsage(fooCmd) == "foo [OPTIONS]"
      doAssert cli.help(fooCmd) == """
do things

Usage: foo [OPTIONS]"""
      doAssert cli.subcommandsUsage(RootCommand) == "  foo  do things [default]"

    block:
      ## Deep path
      var cli = commandBuilder(bool).initCli()
      let fooCmd = cli.commandBuilder()
        .name("foo")
        .describe("do things")
        .addTo(cli, RootCommand)
      let barCmd = cli.commandBuilder()
        .name("bar")
        .describe("do real things")
        .addTo(cli, fooCmd)
      doAssert cli.commandUsage(fooCmd) == "foo [OPTIONS] <COMMAND>"
      doAssert cli.commandUsage(fooCmd, rootName = "root") == "root foo [OPTIONS] <COMMAND>"
      doAssert cli.commandUsage(barCmd) == "foo bar [OPTIONS]"
      doAssert cli.commandUsage(barCmd, rootName = "root") == "root foo bar [OPTIONS]"
      doAssert cli.subcommandsUsage(fooCmd) == "  bar  do real things"

  block:
    ## Many subcommands
    block:
      ## Ordered by addition
      var cli = commandBuilder(bool).initCli()
      cli.commandBuilder()
        .name("x")
        .addTo(cli, RootCommand)
      cli.commandBuilder()
        .name("a")
        .addTo(cli, RootCommand)
      cli.commandBuilder()
        .name("c")
        .addTo(cli, RootCommand)
      doAssert cli.subcommandsUsage(RootCommand) == """
  x
  a
  c"""

    block:
      ## Aligned into columns
      var cli = commandBuilder(bool).initCli()
      cli.commandBuilder()
        .name("x")
        .describe("set x")
        .addTo(cli, RootCommand)
      cli.commandBuilder()
        .name("very-long")
        .describe("do some long thing")
        .addTo(cli, RootCommand)
      cli.commandBuilder()
        .name("mid")
        .describe("kinda mid")
        .addTo(cli, RootCommand)
      doAssert cli.subcommandsUsage(RootCommand) == """
  x          set x
  very-long  do some long thing
  mid        kinda mid"""

    block:
      ## Subcommand-only help
      var cli = commandBuilder(bool)
        .name("cmd")
        .initCli()
      cli.commandBuilder()
        .name("x")
        .describe("set x")
        .addTo(cli, RootCommand)
      cli.commandBuilder()
        .name("very-long")
        .describe("do some long thing")
        .default()
        .addTo(cli, RootCommand)
      cli.commandBuilder()
        .name("mid")
        .describe("kinda mid")
        .addTo(cli, RootCommand)
      doAssert cli.help(RootCommand) == """
Usage: cmd [OPTIONS] [COMMAND]

Commands:
  x          set x
  very-long  do some long thing [default]
  mid        kinda mid"""
