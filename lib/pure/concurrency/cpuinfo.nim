#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements a proc to determine the number of CPUs / cores.

runnableExamples:
  doAssert countProcessors() > 0


include "system/inclrtl"

when defined(posix) and not (defined(macosx) or defined(bsd)):
  import std/posix

when defined(windows):
  import std/private/win_getsysteminfo

when defined(freebsd) or defined(macosx):
  {.emit: "#include <sys/types.h>".}

when defined(openbsd) or defined(netbsd):
  {.emit: "#include <sys/param.h>".}

when defined(macosx) or defined(bsd):
  # we HAVE to emit param.h before sysctl.h so we cannot use .header here
  # either. The amount of archaic bullshit in Poonix based OSes is just insane.
  {.emit: "#include <sys/sysctl.h>".}
  const
    CTL_HW = 6
    HW_AVAILCPU = 25
    HW_NCPU = 3
  proc sysctl(x: ptr array[0..3, cint], y: cint, z: pointer,
              a: var csize_t, b: pointer, c: csize_t): cint {.
              importc: "sysctl", nodecl.}

when defined(genode):
  include genode/env

  proc affinitySpaceTotal(env: GenodeEnvPtr): cuint {.
    importcpp: "@->cpu().affinity_space().total()".}

when defined(haiku):
  type
    SystemInfo {.importc: "system_info", header: "<OS.h>".} = object
      cpuCount {.importc: "cpu_count".}: uint32

  proc getSystemInfo(info: ptr SystemInfo): int32 {.importc: "get_system_info",
                                                    header: "<OS.h>".}

proc countProcessors*(): int {.rtl, extern: "ncpi$1".} =
  ## Returns the number of the processors/cores the machine has.
  ## Returns 0 if it cannot be detected.
  when defined(windows):
    var
      si: SystemInfo
    getSystemInfo(addr si)
    result = int(si.dwNumberOfProcessors)
  elif defined(macosx) or defined(bsd):
    var
      mib: array[0..3, cint]
      numCPU: int
    mib[0] = CTL_HW
    mib[1] = HW_AVAILCPU
    var len = sizeof(numCPU).csize_t
    discard sysctl(addr(mib), 2, addr(numCPU), len, nil, 0)
    if numCPU < 1:
      mib[1] = HW_NCPU
      discard sysctl(addr(mib), 2, addr(numCPU), len, nil, 0)
    result = numCPU
  elif defined(hpux):
    result = mpctl(MPC_GETNUMSPUS, nil, nil)
  elif defined(irix):
    var SC_NPROC_ONLN {.importc: "_SC_NPROC_ONLN", header: "<unistd.h>".}: cint
    result = sysconf(SC_NPROC_ONLN)
  elif defined(genode):
    result = runtimeEnv.affinitySpaceTotal().int
  elif defined(haiku):
    var sysinfo: SystemInfo
    if getSystemInfo(addr sysinfo) == 0:
      result = sysinfo.cpuCount.int
  else:
    result = sysconf(SC_NPROCESSORS_ONLN)
  if result <= 0: result = 0
