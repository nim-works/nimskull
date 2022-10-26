discard """
  targets: "js"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/6612
    Issue with javascript compiler, var parameters and case fields
  . These are the interesting parts in the generated JS:
    Apparently fillWith expects, in place of the single var parameter,
    the parent object plus the name of the field of that object,
    but apparently when that field depends on a discriminant, the compiler
    is just evaluating the field instead like it wasn't var.
'''
"""

proc fillWith(sq: var seq[int], n: int, unused: string) =
  sq = @[n]

type
  Object = object of RootObj
    case hasNums: bool
    of true:
      numbers: seq[int]
    of false:
      discard
    always: seq[int]

var obj = Object(hasNums: true)

obj.always.fillWith(5, "unused")
doAssert obj.always == @[5]

obj.numbers.fillWith(3, "unused")
doAssert obj.numbers == @[3]
doAssert obj.always == @[5]

