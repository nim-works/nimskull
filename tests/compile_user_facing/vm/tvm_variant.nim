discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/17039
    VM crash with object variant, adding seq to seq field
  . Encountered in nimscript. The issue seems to be that fields in
    object variant branches break when converting to var T.
  . turns out the access of field is wrapped in an nkCheckedFieldExpr
    in one and not the other. the analyseIfAddressTaken proc in semexprs.nim
    handles that so I added an unwrap case (doesn't feel a 100% right) but
    seems less terrible than putting an if guard for poor code gen
  '''
"""
type
  Obj1 = object
    case kind: bool
    of false:
      field: seq[int]
    else: discard

static:
  var obj1 = Obj1()
  obj1.field.add(@[])

