discard """
  targets: "c js !vm"
  matrix: "--gc:arc"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/17812
      repr fails to compile with ARC/ORC on DateTime object field
  '''
"""

import std/times

block:
  block:
    type
      Task = object
        cb: proc ()

    proc hello() = discard


    let t = Task(cb: hello)

    doAssert t.repr.len > 0


  block:
    type MyObj = object
      field: DateTime


    proc `$`(o: MyObj): string = o.repr

    doAssert ($MyObj()).len > 0