discard """
description: "Test out of range int literal conversion for enums"
cmd: "nim check --filenames:canonical --backend:$target $options $file"
action: reject
"""

block out_of_range:
  type
    Foo = enum
      thingy
  echo 1.Foo #[tt.Error
        ^ 1 can't be converted to Foo]#

block out_of_range_singleton_holey_enum:
  type
    Foo = enum
      thingy = 2
  echo 1.Foo #[tt.Error
        ^ 1 can't be converted to Foo]#

block out_of_range_within_holey_enum:
  type
    Foo = enum
      thingy
      thangy = 10
  echo 1.Foo #[tt.Error
        ^ 1 can't be converted to Foo]#