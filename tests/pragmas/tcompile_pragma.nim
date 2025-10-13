discard """
  description: "Tests for the `compile` pragma"
  targets: "c"
"""

{.compile("cfunction.c", "-DNUMBER_HERE=34").}
# ensure that compiling a second C file with the same name works:
{.compile("sub/cfunction.c", "-DNUMBER_HERE=1").}

proc cfunction(): cint {.importc.}
proc cfunction2(): cint {.importc.}

doAssert cfunction() == 34
doAssert cfunction2() == 1
