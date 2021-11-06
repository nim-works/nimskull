discard """
description: '''
'''
errormsg: "cannot prove that it's safe to initialize 'fTrue' with the runtime value for the discriminator 'isSelected'"
"""

type
  BoolVariant = object
    case isSelected: bool
      of true:
        fTrue: int

      of false:
        fFalse: int

var it = BoolVariant(fTrue: 12)