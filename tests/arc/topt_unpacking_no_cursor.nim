discard """
  matrix: "--expandArc:test --hints:off"
  nimout: '''--expandArc: test

scope:
  def x: (Obj, Obj) = init() -> [Unwind]
  def _2: (Obj, Obj) = move x
  def a: Obj
  a := move _2.0
  def b: Obj
  b := move _2.1
  =destroy(name b)
  =destroy(name a)

-- end of expandArc ------------------------'''
"""

type Obj = object

proc `=copy`(a: var Obj, b: Obj) =
  discard

proc init(): (Obj, Obj) =
  discard

proc test() =
  let x = init()
  let (a, b) = x
  # ^^ the rhs is not something that *requires* taking ownership of

test()
