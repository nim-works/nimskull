import experimental/cmdline
from std/os import walkDir, PathComponent
from std/osproc import execCmdEx
from std/strutils import endsWith
import dust

proc main(testsDir: string = "./tools/dust/tests") =
  var
    success = 0
    error = 0
    failed = 0

  for pc, file in walkDir(testsDir):
    if pc == PathComponent.pcFile and file.endsWith(".nim"):
      echo "testing: " & file
      let
        cmd = "./bin/dust " & file
        (output, exitCode) = execCmdEx(cmd)
        code = ErrorCode(exitCode)
      echo "command (cmd): ", cmd
      case code
      of ErrorCode.success:
        echo "success: " & file
        inc success
      of ErrorCode.fileNotProvided, ErrorCode.setupError:
        echo "error: " & file & " with code " & $code
        inc error
      of ErrorCode.noError:
        echo "failed: " & file & " with code " & $code
        inc failed

  let total = success + error + failed

  if total == 0:
    echo "no files to minimize"
    quit 1
  elif success == total:
    echo "all " & $total & " files were successfully minimized"
    quit 0
  else:
    echo "success: " & $success & " of " & $total
    echo "failed: " & $failed & " of " & $total
    echo "error: " & $error & " of " & $total
    quit 1

when isMainModule:
  from std/sugar import `=>`
  type Args = object
    testsDir: string

  var cli = commandBuilder(Args)
    .name("tester")
    .describe("Test the dust tool")
    .initCli()

  cli.addHelpFlag()

  cli.positionalBuilder()
    .name("testsDir")
    .parser(string, (val, var args) => (args.testsDir = val))
    .describe("The directory containing the tests")
    .optional()
    .addTo(cli)

  let args = cli.run(defaults = Args(testsDir: "./tools/dust/tests"))

  main(args.testsDir)