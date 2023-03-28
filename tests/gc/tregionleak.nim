discard """
  cmd: '''nim c --gc:regions $file'''
  output: '''
finalized
finalized
'''
"""

type Obj = object

proc `=destroy`(o: var Obj) =
  echo "finalized"

withScratchRegion:
  var test: ref Obj
  new(test)

var
  mr: MemRegion
  test: ref Obj

withRegion(mr):
  new(test)

deallocAll(mr)
