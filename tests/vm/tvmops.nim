discard """
  description: "Tests for VM Operations (`vmops.nim`)"
  matrix: "--experimental:vmopsDanger"
  targets: "c js"
  joinable: false
"""

# marked as not joinable as this test executes the compiler (error prone)

import std/os
import std/math
import std/strutils

static:
  # TODO: add more tests
  block: #getAppFilename, gorgeEx, gorge
    const nim = getCurrentCompilerExe()
    let ret = gorgeEx(nim & " --version")
    doAssert ret.exitCode == 0
    doAssert ret.output.contains "Nimskull Compiler"
    let ret2 = gorgeEx(nim & " --nonxistent")
    doAssert ret2.exitCode != 0
    let output3 = gorge(nim & " --version")
    doAssert output3.contains "Nimskull Compiler"

  block:
    const key = "D20181210T175037"
    const val = "foo"
    putEnv(key, val)
    doAssert existsEnv(key)
    doAssert getEnv(key) == val

  block:
    # sanity check (we probably don't need to test for all ops)
    const a1 = arcsin 0.3
    let a2 = arcsin 0.3
    doAssert a1 == a2

  block bitxor:
    let x = -1'i32
    let y = 1'i32
    doAssert (x xor y) == -2

block:
  # Check against bugs like #9176
  doAssert getCurrentCompilerExe() == getCurrentCompilerExe().static
  if false: #pending #9176
    doAssert gorgeEx("nonxistent") == gorgeEx("nonxistent").static
