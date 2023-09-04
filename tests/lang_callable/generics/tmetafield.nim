discard """
  cmd: "nim check $options --hints:off $file"
  action: "reject"
  nimoutfull: true
  nimout: '''
tmetafield.nim(26, 13) Error: 'proc' is not a concrete type; for a callback without parameters use 'proc()'
tmetafield.nim(27, 14) Error: 'Foo' is not a concrete type
'''
"""

# bug #188








# line 20
type
  Foo[T] = object
    x: T

  TBaseMed =  object
    doSmth: proc
    data: seq[Foo]

var a: TBaseMed

