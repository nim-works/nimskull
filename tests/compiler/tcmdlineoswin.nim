discard """
  matrix: "--os:windows"
  disabled: "linux"
  disabled: "bsd"
  disabled: "osx"
  disabled: "unix"
  disabled: "posix"
"""

import strutils

static:
  #os is set to "linux" in toswin.nim.cfg, but --os:windows in command line should override it.
  doAssert cmpIgnoreCase(hostOS, "windows") == 0
