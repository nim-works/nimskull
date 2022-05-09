discard """
targets: "!js"
joinable: false
"""

import std/unittest
import experimental/shellrunner
import std/[strutils, os]

let params = commandLineParams()

if params.len() != 0:
  case params[0]:
    of "print-back-1":
      echo params[1]

    of "exit-code":
      quit parseInt(params[1])

    of "stdout":
      stdout.writeLine(params[1])

    of "stderr":
      stderr.writeLine(params[1])

    of "stdin":
      stdout.writeLine(stdin.readLine())

  quit 0

suite "Builder":
  test "basic":
    check shell("gcc").toStr() == @["gcc"]

  test "with arguments":
    check shell("gcc", ["--version"]).toStr() == @["gcc", "--version"]

  test "Interpolation":
    check shell("gcc", [shSub("options")]).
      interpolate({"options": @["test"]}).
      toStr() == @["gcc", "test"]

    check shell("gcc", [shSub("options")]).
      interpolate({"options": @["A", "B", "C"]}).
      toStr() == @["gcc", "A", "B", "C"]

    check shell("gcc", [shSub("random")]).
      interpolate({"options": @["test"]}).
      toStr() == @["gcc"]

let now = getAppFilename()

suite "Execution":
  test "standalone":
    check shell(now, ["print-back-1", "4 ? 4"]).
      exec().
      stdout.
      strip() == "4 ? 4"

    var nim = shell(now, ["print-back-1"])
    nim.arg("[ $# ]", "2 + 2")
    check nim.exec().stdout.strip() == "[ 2 + 2 ]"

  test "parallel":
    let cmds = @[
      shell(now, ["print-back-1", "AAA"]),
      shell(now, ["print-back-1", "BBB"])
    ]

    let res = cmds.exec(maxParallel = 1)
    check res[0].cmd.toStr()[1..^1] == @["print-back-1", "AAA"]
    check res[1].cmd.toStr()[1..^1] == @["print-back-1", "BBB"]


suite "std(in|out|err) processing":
  test "stdin":
    check shell(now, ["stdin"]).exec(stdin = "test").stdout.strip() == "test"
    check shell(now, ["stdin"]).exec().stdout.strip() == ""

  test "stderr":
    check shell(now, ["stderr", "err"]).exec().stderr.strip() == "err"
    check shell(now, ["stdout", "out"]).exec().stdout.strip() == "out"

  test "code":
    check shell(now, ["exit-code", "1"]).exec().retcode == 1
