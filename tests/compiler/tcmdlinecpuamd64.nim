discard """
  targets: "c cpp"
  matrix: "--cpu:amd64"
  disabled: "32bit"
"""

import strutils

static:
  #cpu is set to "i386" in tcpuamd64.nim.cfg, but --cpu:amd64 in command line should override it.
  doAssert cmpIgnoreCase(hostCPU, "amd64") == 0
