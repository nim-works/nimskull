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

block typicalUsage:
  ## Samples of typical command lines

  block catLike:
    ## Unix's cat clone
    type Opt = enum
      ShowNonPrinting
      SqueezeBlank

    var cli = initCli set[Opt]
    cli.flagBuilder
      .name("show-nonprinting")
      .alias("v")
      .parser(bool, proc(k, v: auto, s: var auto): Action = s[ShowNonPrinting] = v)
      .describe("display non-printing characters")
      .addTo(cli)
    cli.flagBuilder
      .name("squeeze-blank")
      .alias("s")
      .parser(bool, proc(k, v: auto, s: var auto): Action = s[SqueezeBlank] = v)
      .describe("remove repeated empty lines")
      .addTo(cli)

    block dashdash:
      let parsed = cli.parse ["--show-nonprinting", "--", "--squeeze-blank"]
      doAssert parsed.result == {ShowNonPrinting}
      doAssert parsed.remaining == ["--squeeze-blank"]

    block basic:
      let parsed = cli.parse ["--show-nonprinting", "--squeeze-blank", "-"]
      doAssert parsed.result == {ShowNonPrinting, SqueezeBlank}
      doAssert parsed.remaining == ["-"]

    block bool:
      let parsed = cli.parse ["--show-nonprinting=no", "-"]
      doAssert parsed.result == {}
      doAssert parsed.remaining == ["-"]

    block short:
      let parsed = cli.parse ["-sv"]
      doAssert parsed.result == {ShowNonPrinting, SqueezeBlank}

    block doc:
      doAssert cli.flagsUsage == """
  -v, --show-nonprinting  display non-printing characters
  -s, --squeeze-blank     remove repeated empty lines"""

  block seqLike:
    ## Unix's seq clone
    type
      Config = object
        format: Option[string]
        separator: string
        equalizeWidth: bool

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

    const baseConfig = Config(separator: "\\n")

    block basic:
      let parsed = cli.parse(["10", "--format:%02d"], baseConfig)
      doAssert parsed.result.format == some("%02d")
      doAssert parsed.result.separator == "\\n"
      doAssert not parsed.result.equalizeWidth
      doAssert parsed.remaining == ["10"]

    block:
      let parsed = cli.parse(["--equal-width", "10", "--format:"], baseConfig)
      doAssert parsed.result.format == some("")
      doAssert parsed.result.separator == "\\n"
      doAssert parsed.result.equalizeWidth
      doAssert parsed.remaining == ["10"]

    block separated:
      let parsed = cli.parse(["10", "--format", "--separator=;"], baseConfig)
      doAssert parsed.result.format == some("--separator=;")
      doAssert parsed.result.separator == "\\n"
      doAssert not parsed.result.equalizeWidth
      doAssert parsed.remaining == ["10"]

    block multiple:
      let parsed = cli.parse(["10", "--separator", ";", "100"], baseConfig)
      doAssert parsed.result.format == none(string)
      doAssert parsed.result.separator == ";"
      doAssert not parsed.result.equalizeWidth
      doAssert parsed.remaining == ["10", "100"]

    block doc:
      doAssert cli.flagsUsage == """
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

    let parsed = cli.parse ["--", "--flag"]
    doAssert not parsed.result
    doAssert parsed.remaining == ["--flag"]

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
    let parsed = cli.parse ["-o", "10"]
    doAssert parsed.result.o.i == 10

  block:
    ## Flag behaviour tests
    type
      Config = object
        i: int
        n: Natural
        f: seq[float]
        s: string
        b: bool

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

    const baseConfig = Config(f: @[42.0], s: "default")

    block simple:
      let parsed = cli.parse(["--f", "0", "--int", "42"], baseConfig)
      doAssert parsed.result.i == 42
      doAssert parsed.result.f == [42.0, 0.0]
      doAssert parsed.result.s == "default"
      doAssert not parsed.result.b

    block multiple:
      let parsed = cli.parse(
        ["--int", "42", "-f", "420", "--int=100", "-f", "10.0"],
        baseConfig
      )
      doAssert parsed.result.i == 100
      doAssert parsed.result.f == [42.0, 420, 10.0]

    block boolSpace:
      ## Boolean does not take non-delimited value
      let parsed = cli.parse ["--bool", "false"]
      doAssert parsed.result.b
      doAssert parsed.remaining == ["false"]

    block boolDelim:
      ## Boolean only take delimited values
      let parsed = cli.parse(["--bool:false"], Config(b: true))
      doAssert not parsed.result.b

    block optionalSpace:
      ## Optional flag does not take non-delimited value
      let parsed = cli.parse ["--nat", "42"]
      doAssert parsed.result.n == 0xdead
      doAssert parsed.remaining == ["42"]

    block optionalDelim:
      ## Optional flag only take delimited value
      let parsed = cli.parse ["--nat=42"]
      doAssert parsed.result.n == 42

    block help:
      doAssertRaises(HelpError[Config]):
        discard cli.parse ["--help"]

    block unknown:
      ## Unregistered flags will trigger an error and stop processing
      try:
        discard cli.parse ["--unknown", "--int=10"]
        doAssert false, "expected UnknownFlagError"
      except UnknownFlagError[Config] as e:
        doAssert e.flagName == "--unknown"
        doAssert e.result.result.i == 0

    block novalue:
      ## Missing value will trigger an error
      try:
        discard cli.parse ["--bool", "--int"]
        doAssert false, "expected MissingValueError"
      except MissingValueError[Config] as e:
        doAssert e.flagName == "--int"
        doAssert e.flag == intFlag
        doAssert e.result.result.b

    block wrongvalue:
      ## Invalid value also errors and stop processing
      try:
        discard cli.parse ["-f", "0", "--int=notint", "--string", "somestring"]
        doAssert false, "expected InvalidValueError"
      except InvalidValueError[Config] as e:
        doAssert e.parent of ValueError
        doAssert e.flagName == "--int"
        doAssert e.flag == intFlag
        doAssert e.flagValue == some("notint")
        doAssert e.result.result.f == [0.0]
        doAssert e.result.result.s == ""

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
