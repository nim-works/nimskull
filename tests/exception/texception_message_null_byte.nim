discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/13115
    newException terminates msg early from NULL byte, other IO does not
  . Error output produced by raise newException is inconsistent with other
    IO such as echo or stdout.write.
'''
"""
const msg = "This char is `" & '\0' & "` and works fine!"

when defined nim_t13115:
  # bug #13115
  template fn =
    raise newException(Exception, msg)
  when defined nim_t13115_static:
    static: fn()
  fn()
else:
  import std/[osproc, os]
  from std/strformat import fmt
  from std/strutils import contains

  type TBackend = enum
    backendC = "c"
    backendJs = "js"

  const
    nim = getCurrentCompilerExe()
    file = currentSourcePath
    opts = [
      backendC:  @["", "-d:nim_t13115_static", "-d:danger", "-d:debug"],
      backendJs: @["", "-d:nim_t13115_static"]
    ] ## save CI time by avoiding mostly redundant combinations as far as
      ## this bug is concerned

  proc main =
    for b in backendC..backendJs:
      for opt in opts[b]:
        let
          cmd = fmt"{nim} r -b:{b} -d:nim_t13115 {opt} --hints:off {file}"
          (outp, exitCode) = execCmdEx(cmd)
        when defined windows:
          # `\0` not preserved on windows
          doAssert "` and works fine!" in outp, cmd & "\n" & msg
        else:
          doAssert msg in outp, cmd & "\n" & msg
        doAssert exitCode == 1

  main()