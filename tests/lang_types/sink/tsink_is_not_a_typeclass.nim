discard """
  description: '''
    Ensure that `sink` cannot be used as a type-class or constraint
  '''
  cmd: "nim check --filenames=canonical --hints:off $options $file"
  action: reject
"""

var x: int
discard x is sink #[tt.Error
             ^ 'sink' cannot be used as a type class]#

proc p[T: sink]() = #[tt.Error
          ^ 'sink' cannot be used as a type class]#
  discard