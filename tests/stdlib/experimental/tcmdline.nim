discard """
description: "Tests for the cmdline module"
"""

import std/setutils
import std/options
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

    var cli = initCli Config
    cli.flagBuilder
      .name("show-nonprinting")
      .alias("v")
      .parser(bool, proc(k, v: auto, c: var auto): Action = c.opts[ShowNonPrinting] = v)
      .describe("display non-printing characters")
      .addTo(cli)
    cli.flagBuilder
      .name("squeeze-blank")
      .alias("s")
      .parser(bool, proc(k, v: auto, c: var auto): Action = c.opts[SqueezeBlank] = v)
      .describe("remove repeated empty lines")
      .addTo(cli)
    cli.positionalBuilder
      .name("FILE")
      .parser(string, proc(v: auto, c: var auto): Action = c.files.add v)
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
      doAssert cli.flagsUsage == """
  -v, --show-nonprinting  display non-printing characters
  -s, --squeeze-blank     remove repeated empty lines"""

      doAssert cli.commandUsage("cat") == "cat [OPTIONS] [FILE]..."
      doAssert cli.positionalsUsage == """
  [FILE]...  file(s) to concatenate"""

      doAssert cli.help("cat") == """
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

    var cli = initCli Config
    cli.flagBuilder
      .name("format")
      .parser(string, proc (_, v: auto, c: var auto): Action = c.format = some(v))
      .describe("use printf style FORMAT", "FORMAT")
      .addTo(cli)
    cli.flagBuilder
      .name("separator")
      .parser(string, proc (_, v: auto, c: var auto): Action = c.separator = v)
      .describe("use STRING to separate numbers", "STRING")
      .addTo(cli)
    cli.flagBuilder
      .name("equal-width")
      .parser(bool, proc (_, v: auto, c: var auto): Action = c.equalizeWidth = v)
      .describe("equalize width by padding with leading zeroes")
      .addTo(cli)
    cli.positionalBuilder
      .name("NUMBER")
      .parser(int, proc(v: auto, c: var auto): Action = c.numbers.add v)
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
      doAssert cli.flagsUsage == """
  --format <FORMAT>     use printf style FORMAT
  --separator <STRING>  use STRING to separate numbers
  --equal-width         equalize width by padding with leading zeroes"""

      doAssert cli.commandUsage("seq") == "seq [OPTIONS] <NUMBER>..."
      doAssert cli.positionalsUsage == "  <NUMBER>..."

      doAssert cli.help("seq") == """
Usage: seq [OPTIONS] <NUMBER>...

Arguments:
  <NUMBER>...

Options:
  --format <FORMAT>     use printf style FORMAT
  --separator <STRING>  use STRING to separate numbers
  --equal-width         equalize width by padding with leading zeroes"""

block flags:
  ## Tests for flags
  block:
    ## It is not possible to add duplicated flags
    var cli = initCli int

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
    var cli = initCli int

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
    var cli = initCli bool

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

    var cli = initCli Config
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

    var cli = initCli Config
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
    cli.addHelpFlag("help")
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
        doAssert e.flagName == "--unknown"
        doAssert parsed.i == 0

    block novalue:
      ## Missing value will trigger an error
      var parsed: Config
      try:
        cli.parse(parsed, @["--bool", "--int"])
        doAssert false, "expected MissingValueError"
      except MissingValueError as e:
        doAssert e.flagName == "--int"
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
        doAssert e.flagName == "--int"
        doAssert e.flag == intFlag
        doAssert e.flagValue == some("notint")
        doAssert parsed.f == [0.0]
        doAssert parsed.s == ""

block positionals:
  ## Tests for positionals
  block:
    ## Disallow duplicate positional names
    var cli = initCli int
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
      var cli = initCli bool
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
      var cli = initCli bool
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
      var cli = initCli bool
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
      var cli = initCli bool
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
      var cli = initCli bool
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
    ## First `--` is never captured
    var cli = initCli seq[string]

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

    var cli = initCli Config
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
      var cli = initCli bool
      cli.positionalBuilder
        .name("HELP")
        .parser(proc (v: auto, c: var auto): Action = ShowHelp)
        .addTo(cli)
      doAssertRaises(HelpError):
        discard cli.parse @["help"]
    block:
      ## Stop flag processing
      var cli = initCli seq[string]
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
      var cli = initCli string
      cli.positionalBuilder
        .name("OPT")
        .optional()
        .parser(proc (v: auto, c: var auto): Action = c = v)
        .addTo(cli)

      doAssert cli.parse(@[]) == ""

    block:
      var cli = initCli seq[string]
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
      var cli = initCli seq[string]
      let any = cli.positionalBuilder
        .name("ANY")
        .catchAll()
        .parser(proc (v: auto, c: var auto): Action = c.add v)
        .addTo(cli)

      try:
        discard cli.parse(@[])
        doAssert false, "expected MissingPositionalError"
      except MissingPositionalError as e:
        doAssert e.position == 0
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

    var cli = initCli Config
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
        doAssert e.position == 0
        doAssert e.positionalValue == ""
        doAssert e.positional == req0

      block:
        ## Verify that data is accumulated on failure
        var parsed: Config
        try:
          cli.parse(parsed, @["on", "-f10"])
          doAssert false, "expected MissingPositionalError"
        except MissingPositionalError as e:
          doAssert e.position == 1
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
        doAssert e.position == 2
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
          doAssert e.position == 1
          doAssert e.positionalValue == "-f10"
          doAssert e.positional == req1

block docgen:
  block:
    ## Just one flag
    block:
      ## Short flag
      var cli = initCli int
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v"

      cli = initCli int
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .describe("", "INT")
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v[=<INT>]"

      cli = initCli int
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .describe("some description")
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v  some description"

      cli = initCli int
      cli.flagBuilder
        .name("v")
        .optionalParser(noop)
        .describe("some description", "INT")
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v[=<INT>]  some description"

      cli = initCli int
      cli.flagBuilder
        .name("v")
        .parser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v <VALUE>"

      cli = initCli int
      cli.flagBuilder
        .name("v")
        .parser(noop)
        .describe("some description")
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v <VALUE>  some description"

      cli = initCli int
      cli.flagBuilder
        .name("v")
        .parser(noop)
        .describe("some description", "INT")
        .addTo(cli)

      doAssert cli.flagsUsage == "  -v <INT>  some description"

    block:
      ## Long flag
      var cli = initCli int
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag"

      cli = initCli int
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .describe("", "VALUE")
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag[=<VALUE>]"

      cli = initCli int
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .describe("flag something", "VALUE")
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag[=<VALUE>]  flag something"

      cli = initCli int
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag <VALUE>"

      cli = initCli int
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .describe("flag something")
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag <VALUE>  flag something"

      cli = initCli int
      cli.flagBuilder
        .name("flag")
        .parser(noop)
        .describe("flag something", "FLAG,...")
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag <FLAG,...>  flag something"

      cli = initCli int
      cli.flagBuilder
        .name("flag")
        .optionalParser(noop)
        .describe("flag something", "VALUE")
        .addTo(cli)

      doAssert cli.flagsUsage == "  --flag[=<VALUE>]  flag something"

    block:
      ## Aliases
      block:
        ## Check render ordering
        # Use the first short and first long flag, in that exact order
        var cli = initCli int
        cli.flagBuilder
          .name("flag")
          .alias("other-long", "c")
          .optionalParser(noop)
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag"

        cli = initCli int
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .optionalParser(noop)
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag"

      block description:
        ## Check multi-flag description render
        var cli = initCli int
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .parser(noop)
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag <VALUE>"

        cli = initCli int
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .optionalParser(noop)
          .describe("", "VALUE")
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag[=<VALUE>]"

        cli = initCli int
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .optionalParser(noop)
          .describe("flag something", "VALUE")
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag[=<VALUE>]  flag something"

        cli = initCli int
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .parser(noop)
          .describe("flag something", "")
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag <VALUE>  flag something"

        cli = initCli int
        cli.flagBuilder
          .name("c")
          .alias("flag", "f")
          .parser(noop)
          .describe("flag something", "FLAG,...")
          .addTo(cli)

        doAssert cli.flagsUsage == "  -c, --flag <FLAG,...>  flag something"

  block:
    ## Multiple flags
    block:
      ## Order by addition time
      var cli = initCli int
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

      doAssert cli.flagsUsage == """
  -c, --flag <VALUE>  flag something
  -a                  show all"""

    block:
      ## Column alignment
      var cli = initCli int
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

      doAssert cli.flagsUsage == """
  -c, --flag <VALUE>                    flag something
  -a                                    show all
  --yes-i-know-what-i-am-doing <VALUE>"""

    block:
      ## Flag-only help
      var cli = initCli int
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

      doAssert cli.help("cmd") == """
Usage: cmd [OPTIONS]

Options:
  -c, --flag <VALUE>                    flag something
  -a                                    show all
  --yes-i-know-what-i-am-doing <VALUE>"""

  block:
    ## Empty
    let cli = initCli bool
    doAssert cli.flagsUsage == ""
    doAssert cli.positionalsUsage == ""
    doAssert cli.commandUsage("") == "[OPTIONS]"
    doAssert cli.commandUsage("something") == "something [OPTIONS]"
    doAssert cli.help("") == "Usage: [OPTIONS]"
    doAssert cli.help("something") == "Usage: something [OPTIONS]"

  block:
    ## Just one positional
    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("REQUIRED")
        .parser(noop)
        .addTo(cli)

      doAssert cli.positionalsUsage == "  <REQUIRED>"
      doAssert cli.commandUsage("") == "[OPTIONS] <REQUIRED>"

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("REQUIRED")
        .parser(noop)
        .describe("a required parameter")
        .addTo(cli)

      doAssert cli.positionalsUsage == "  <REQUIRED>  a required parameter"
      doAssert cli.commandUsage("") == "[OPTIONS] <REQUIRED>"

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("REQANY")
        .catchAll()
        .parser(noop)
        .addTo(cli)

      doAssert cli.positionalsUsage == "  <REQANY>..."
      doAssert cli.commandUsage("") == "[OPTIONS] <REQANY>..."

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("REQANY")
        .catchAll()
        .parser(noop)
        .describe("many required parameter(s)")
        .addTo(cli)

      doAssert cli.positionalsUsage == "  <REQANY>...  many required parameter(s)"
      doAssert cli.commandUsage("") == "[OPTIONS] <REQANY>..."

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("NOT-REQUIRED")
        .parser(noop)
        .optional()
        .addTo(cli)

      doAssert cli.positionalsUsage == "  [NOT-REQUIRED]"
      doAssert cli.commandUsage("") == "[OPTIONS] [NOT-REQUIRED]"

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("NOT-REQUIRED")
        .parser(noop)
        .optional()
        .describe("not at all required")
        .addTo(cli)

      doAssert cli.positionalsUsage == "  [NOT-REQUIRED]  not at all required"
      doAssert cli.commandUsage("") == "[OPTIONS] [NOT-REQUIRED]"

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("NOT-REQUIRED-MANY")
        .parser(noop)
        .optional()
        .catchAll()
        .addTo(cli)

      doAssert cli.positionalsUsage == "  [NOT-REQUIRED-MANY]..."
      doAssert cli.commandUsage("") == "[OPTIONS] [NOT-REQUIRED-MANY]..."

    block:
      var cli = initCli bool
      cli.positionalBuilder
        .name("NOT-REQUIRED-MANY")
        .parser(noop)
        .optional()
        .catchAll()
        .describe("many param(s)")
        .addTo(cli)

      doAssert cli.positionalsUsage == "  [NOT-REQUIRED-MANY]...  many param(s)"
      doAssert cli.commandUsage("") == "[OPTIONS] [NOT-REQUIRED-MANY]..."

  block:
    ## Many positionals
    block:
      ## Ordered by addition
      var cli = initCli bool
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

      doAssert cli.positionalsUsage == """
  <REQ>
  [NOT-REQ]
  [MANY-NOT-REQ]..."""
      doAssert cli.commandUsage("") == "[OPTIONS] <REQ> [NOT-REQ] [MANY-NOT-REQ]..."

    block:
      ## Aligned into columns
      var cli = initCli bool
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

      doAssert cli.positionalsUsage == """
  <REQ>              a required parameter
  [NOT-REQ-AT-ALL]   an optional parameter
  [MANY-NOT-REQ]...  many optional parameter(s)"""

    block:
      ## Positional-only help
      var cli = initCli bool
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

      doAssert cli.help("cmd") == """
Usage: cmd [OPTIONS] <REQ> [NOT-REQ-AT-ALL] [MANY-NOT-REQ]...

Arguments:
  <REQ>              a required parameter
  [NOT-REQ-AT-ALL]   an optional parameter
  [MANY-NOT-REQ]...  many optional parameter(s)"""
