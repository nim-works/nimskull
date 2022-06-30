discard """
labels: "codegen gensym union"
description: '''
  . From https://github.com/nim-lang/Nim/issues/8781
    Duplicate member error for union types
  . Not entirely sure if this is a bug, or if its just assumed
    that the programmer should not have variable names that end
    in a capital U.
  . Fixed by replacing 'U' suffix by  "_U" prefix, which is not
    allowed as an identifier: https://github.com/nim-lang/Nim/pull/8787
'''
"""

type
  Drawable = object of RootObj
    discard

  TypeOne = ref object of Drawable
    animatedU: bool
    case animated: bool
    of true:
        frames: seq[int]
    of false:
        region: float

when true:
  let r = 1.5
  let a = TypeOne(animatedU: true,
                  animated: false,
                  region: r)