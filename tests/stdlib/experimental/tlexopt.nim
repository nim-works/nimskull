discard """
  description: "Tests for the lexopt module"
  targets: "c js vm"
"""

import experimental/lexopt
import std/[options, sequtils, strutils]

block typicalUsage:
  ## Samples of typical command lines

  block catLike:
    ## Unix's cat clone
    type
      Option = enum
        optSqueeze
        optNonPrinting

    proc parseCmd(cmd: openArray[string]): (set[Option], seq[string]) =
      var files: seq[string]
      var opts: set[Option]
      var lexer = initCmdLexer(cmd)
      var (kind, option) = lexer.next()
      while kind != cmdEnd:
        if (cmdShort, "v") == (kind, option) or (cmdLong, "show-nonprinting") == (kind, option):
          opts.incl optNonPrinting
        elif (cmdShort, "s") == (kind, option) or (cmdLong, "squeeze-blank") == (kind, option):
          opts.incl optSqueeze
        elif (cmdValue, "--") == (kind, option):
          for file in lexer.remaining():
            files.add file
        elif kind == cmdValue:
          files.add option
        else:
          raise newException(ValueError):
            "unknown option '" & kind.prefix & option & "'"

        (kind, option) = lexer.next()

      result = (opts, files)

    var (opts, files) = parseCmd(["-v", "-", "--", "--squeeze-blank"])
    doAssert opts == {optNonPrinting}:
      "Unexpected value: " & $opts
    doAssert files == ["-", "--squeeze-blank"]:
      "Unexpected value: " & $files

    (opts, files) = parseCmd(["-sv", "-"])
    doAssert opts == {optNonPrinting, optSqueeze}:
      "Unexpected value: " & $opts
    doAssert files == ["-"]:
      "Unexpected value: " & $files

    doAssertRaises(ValueError):
      discard parseCmd(["-c", "-"])

    doAssertRaises(UnexpectedValueError):
      discard parseCmd(["--squeeze-blank=no"])

  block seqLike:
    ## Unix's seq clone
    type
      Opts = object
        format: Option[string]
        separator: Option[string]
        sequence: seq[int]

    proc parseCmd(cmd: openArray[string]): Opts =
      var lexer = initCmdLexer(cmd)
      var (kind, option) = lexer.next()
      while kind != cmdEnd:
        if (kind, option) == (cmdLong, "format") or (kind, option) == (cmdShort, "f"):
          let value = lexer.value()
          if value.isNone:
            raise newException(ValueError):
              "option requires an argument: " & kind.prefix & option
          result.format = value
        elif (kind, option) == (cmdLong, "separator") or (kind, option) == (cmdShort, "s"):
          let value = lexer.value()
          if value.isNone:
            raise newException(ValueError):
              "option requires an argument: " & kind.prefix & option
          result.separator = value
        elif kind == cmdValue:
          if result.sequence.len < 3:
            result.sequence.add parseInt(option)
          else:
            raise newException(ValueError):
              "extra operand: " & option
        else:
          raise newException(ValueError):
            "invalid option: " & kind.prefix & option

        (kind, option) = lexer.next()

      if result.sequence.len == 0:
        raise newException(ValueError):
          "missing operand"

    var opts = parseCmd(["1"])
    doAssert opts.format.isNone
    doAssert opts.separator.isNone
    doAssert opts.sequence == [1]

    opts = parseCmd(["10", "-f:%02d"])
    doAssert opts.format == some("%02d"):
      "Unexpected value: " & $opts.format
    doAssert opts.separator.isNone
    doAssert opts.sequence == [10]

    opts = parseCmd(["10", "-f", "-s;"])
    doAssert opts.format == some("-s;"):
      "Unexpected value: " & $opts.format
    doAssert opts.separator.isNone
    doAssert opts.sequence == [10]

    opts = parseCmd(["10", "-s;", "100"])
    doAssert opts.format.isNone
    doAssert opts.separator == some(";"):
      "Unexpected value: " & $opts.separator
    doAssert opts.sequence == [10, 100]

block opts:
  ## Unit tests for the option parser
  block long:
    ## Long options
    block dashdash:
      ## `--` special case
      var lexer = initCmdLexer(["--"])
      let (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdValue, "--")
      doAssert kind.prefix & option == "--"

    block basic:
      ## Long option of varying sizes and characters
      var lexer = initCmdLexer(["--foo", "--yes-i-am-sure", "--d;", "--plus+"])
      var (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "foo"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--foo"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "yes-i-am-sure"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--yes-i-am-sure"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "d;"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--d;"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "plus+"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--plus+"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdEnd, ""):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == ""

    block values:
      ## Long options with values and some edge cases
      var lexer = initCmdLexer([
        "--foo=bar", "--foo:bar", "--d", "--this-is-value",
        "--bar=:foobar", "--bar::barfoo", "--=smt", "--:bar"
      ])

      var (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "foo"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--foo"
      # inline values are unskippable
      doAssertRaises(UnexpectedValueError):
        discard lexer.next()
      var value = lexer.value()
      doAssert value == some("bar"):
        "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "foo"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--foo"
      # inline values are unskippable
      doAssertRaises(UnexpectedValueError):
        discard lexer.next()
      value = lexer.value(delimitedOnly = true)
      doAssert value == some("bar"):
        "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "d"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--d"
      # This option does not have any inline values
      value = lexer.value(delimitedOnly = true)
      doAssert value.isNone:
        "Unexpected values: " & $value
      value = lexer.value()
      doAssert value == some("--this-is-value")

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "bar"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--bar"
      value = lexer.value(delimitedOnly = true)
      doAssert value == some(":foobar"):
        "Unexpected values: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "bar"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "--bar"
      value = lexer.value()
      doAssert value == some(":barfoo"):
        "Unexpected values: " & $value

      block edgecases:
        ## Edge cases, might be subjected to changes in interpretation
        (kind, option) = lexer.next()
        doAssert (kind, option) == (cmdLong, "=smt"):
          "Unexpected values: " & $(kind, option)
        doAssert kind.prefix & option == "--=smt"
        value = lexer.value(delimitedOnly = true)
        doAssert value.isNone

        (kind, option) = lexer.next()
        doAssert (kind, option) == (cmdLong, ":bar"):
          "Unexpected values: " & $(kind, option)
        doAssert kind.prefix & option == "--:bar"
        value = lexer.value()
        doAssert value.isNone

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdEnd, ""):
        "Unexpected values: " & $(kind, option)
      value = lexer.value()
      doAssert value.isNone

  block short:
    # Short options
    block dash:
      ## `-` special case
      var lexer = initCmdLexer(["-"])
      let (kind, option) = lexer.next()
      doAssert kind == cmdValue
      doAssert option == "-"
      doAssert kind.prefix & option == "-"

    block basic:
      ## Short options without values
      var lexer = initCmdLexer(["-a", "-Acd", "-0@-"])

      var (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "a"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "-a"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "A"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "-A"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "c"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "-c"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "d"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "-d"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "0"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "-0"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "@"):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == "-@"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "-"):
        "Unexpected values: " & $(kind, option)
      # A bit of an edge case because `--` does not parse as (cmdShort, "-").
      doAssert kind.prefix & option == "--"

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdEnd, ""):
        "Unexpected values: " & $(kind, option)
      doAssert kind.prefix & option == ""

    block values:
      ## Short options with values
      var lexer = initCmdLexer([
        "-o", "out", "-O-", "-d:val", "-abcdef", "-f=false", "-:o", "-="
      ])

      var (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "o")
      doAssert kind.prefix & option == "-o"
      # No inline value for this one
      var value = lexer.value(delimitedOnly = true)
      doAssert value.isNone:
        "Unexpected value: " & $value
      value = lexer.value()
      doAssert value == some("out"):
        "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "O")
      doAssert kind.prefix & option == "-O"
      # Inline values are not considered delimited
      value = lexer.value(delimitedOnly = true)
      doAssert value.isNone:
        "Unexpected value: " & $value

      value = lexer.value()
      doAssert value == some("-"):
        "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "d")
      doAssert kind.prefix & option == "-d"
      # Delimited values are unskippable
      doAssertRaises(UnexpectedValueError):
        discard lexer.next()

      value = lexer.value()
      doAssert value == some("val"):
        "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "a")
      doAssert kind.prefix & option == "-a"
      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "b")
      doAssert kind.prefix & option == "-b"
      # No delimited value here
      value = lexer.value(delimitedOnly = true)
      doAssert value.isNone:
        "Unexpected value: " & $value
      # Value collects the remaining short ops
      value = lexer.value()
      doAssert value == some("cdef"):
        "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "f")
      doAssert kind.prefix & option == "-f"
      # Delimited values are unskippable
      doAssertRaises(UnexpectedValueError):
        discard lexer.next()

      value = lexer.value()
      doAssert value == some("false"):
        "Unexpected value: " & $value

      block edgecases:
        ## Edge cases, interpretation might change
        (kind, option) = lexer.next()
        doAssert (kind, option) == (cmdShort, ":")
        doAssert kind.prefix & option == "-:"
        # What comes after is not considered to be delimited
        value = lexer.value(delimitedOnly = true)
        doAssert value.isNone:
          "Unexpected value: " & $value

        value = lexer.value()
        doAssert value == some("o"):
          "Unexpected value: " & $value

        (kind, option) = lexer.next()
        doAssert (kind, option) == (cmdShort, "=")
        doAssert kind.prefix & option == "-="
        value = lexer.value()
        doAssert value.isNone:
          "Unexpected value: " & $value

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdEnd, "")
      doAssert kind.prefix & option == ""
      value = lexer.value()
      doAssert value.isNone:
        "Unexpected value: " & $value

  block unexpected:
    ## Tests for UnexpectedValueError exception
    var lexer = initCmdLexer(["--foo=bar", "-d:foo", "-ab=cdef"])
    var (kind, option) = lexer.next()
    doAssert (kind, option) == (cmdLong, "foo")

    try:
      discard lexer.next()
      doAssert false, "the previous statement should've raised"
    except UnexpectedValueError as e:
      doAssert e.opt == "--foo"
      doAssert e.value == "bar"

    discard lexer.value()

    (kind, option) = lexer.next()
    doAssert (kind, option) == (cmdShort, "d")
    try:
      discard lexer.next()
      doAssert false, "the previous statement should've raised"
    except UnexpectedValueError as e:
      doAssert e.opt == "-d"
      doAssert e.value == "foo"

    discard lexer.value()

    # Test that the option value is correct even for opts within a bundle
    (kind, option) = lexer.next()
    doAssert (kind, option) == (cmdShort, "a")
    (kind, option) = lexer.next()
    doAssert (kind, option) == (cmdShort, "b")
    try:
      discard lexer.next()
      doAssert false, "the previous statement should've raised"
    except UnexpectedValueError as e:
      doAssert e.opt == "-b"
      doAssert e.value == "cdef"

    discard lexer.value()
    (kind, option) = lexer.next()
    doAssert (kind, option) == (cmdEnd, "")

  block remaining:
    ## Tests for the `remaining()` iterator
    block allRemaining:
      ## Use `remaining()` immediately
      const values = ["-FAl", "--define:x=y", "fun", "--not", "fun", "-cool"]
      var lexer = initCmdLexer(@values)
      let collected = toSeq(lexer.remaining())
      doAssert collected == values:
        "Unexpected value: " & $collected

      let (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdEnd, "")

    block afterOpts:
      ## Use `remaining()` after stepping
      const values = ["-undle", "-it-s", "free", "--real", "estate"]
      var lexer = initCmdLexer(
        @["-1", "--second", "-bundle", "-it-s", "free", "--real", "estate"]
      )

      var (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "1"):
        "Unexpected value: " & $(kind, option)
      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "second"):
        "Unexpected value: " & $(kind, option)
      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "b"):
        "Unexpected value: " & $(kind, option)

      let collected = toSeq(lexer.remaining())
      doAssert collected == values:
        "Unexpected value: " & $collected

    block unexpected:
      ## UnexpectedValueError when remaining() is called where inline values exist
      var lexer = initCmdLexer(@["-d:useMalloc", "--run:false"])

      var (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdShort, "d"):
        "Unexpected value: " & $(kind, option)
      try:
        for _ in lexer.remaining():
          doAssert false, "unreachable"
        doAssert false, "the previous call should've failed"
      except UnexpectedValueError as e:
        doAssert e.opt == "-d"
        doAssert e.value == "useMalloc"

      discard lexer.value()

      (kind, option) = lexer.next()
      doAssert (kind, option) == (cmdLong, "run"):
        "Unexpected value: " & $(kind, option)
      try:
        for _ in lexer.remaining():
          doAssert false, "unreachable"
        doAssert false, "the previous call should've failed"
      except UnexpectedValueError as e:
        doAssert e.opt == "--run"
        doAssert e.value == "false"
