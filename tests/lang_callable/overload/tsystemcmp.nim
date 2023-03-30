discard """
  matrix: "--threads:on"
  output: '''@["", "a", "ha", "hi", "ho", "huu"]'''
"""

import std/algorithm

# bug #1657
var modules = @["hi", "ho", "", "a", "ha", "huu"]
sort(modules, system.cmp)
echo modules

type
  MyType = object
    x: string

proc cmp(a, b: MyType): int = cmp(a.x, b.x)

var modulesB = @[MyType(x: "ho"), MyType(x: "ha")]
sort(modulesB, cmp)

# bug #2397

proc f(x: (proc(a,b: string): int) = system.cmp) = discard
