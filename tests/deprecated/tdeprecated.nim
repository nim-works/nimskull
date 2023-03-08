discard """
  nimout: '''
tdeprecated.nim(21, 3) Warning: a is deprecated [Deprecated]
tdeprecated.nim(28, 11) Warning: asdf; enum 'Foo' which contains field 'a' is deprecated [Deprecated]
'''
"""






## line 15



block:
  var
    a {.deprecated.}: array[0..11, int]

  a[8] = 1

block t10111:
  type
    Foo {.deprecated: "asdf" .} = enum
      a 
  
  var _ = a
