#
#
#            Nim's Runtime Library
#        (c) Copyright 2019 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

{.push profiler: off.}

when hostOS == "standalone":
  include "$projectpath/panicoverride"

  proc sysFatal(exceptn: typedesc, message: string) {.inline.} =
    panic(message)

  proc sysFatal(exceptn: typedesc, message, arg: string) {.inline.} =
    rawoutput(message)
    panic(arg)

elif defined(nimPanics) and not defined(nimscript) and not defined(vm):
  import ansi_c

  proc name(t: typedesc): string {.magic: "TypeTrait".}

  proc sysFatal(exceptn: typedesc, message, arg: string) {.inline, noreturn.} =
    when nimvm:
      # TODO when doAssertRaises works in CT, add a test for it
      raise (ref exceptn)(msg: message & arg)
    else:
      {.cast(noSideEffect).}:
        writeStackTrace()
      var buf = newStringOfCap(200)
      add(buf, "Error: unhandled exception: ")
      add(buf, message)
      add(buf, arg)
      add(buf, " [")
      add(buf, name exceptn)
      add(buf, "]\n")
      {.cast(noSideEffect).}:
        cstderr.rawWrite buf
      quit 1

  proc sysFatal(exceptn: typedesc, message: string) {.inline, noreturn.} =
    sysFatal(exceptn, message, "")

else:
  proc sysFatal(exceptn: typedesc, message: string) {.inline, noreturn.} =
    raise (ref exceptn)(msg: message)

  proc sysFatal(exceptn: typedesc, message, arg: string) {.inline, noreturn.} =
    raise (ref exceptn)(msg: message & arg)

{.pop.}
