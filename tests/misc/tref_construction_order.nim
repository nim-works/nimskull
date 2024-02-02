discard """
  targets: c js vm
  matrix: "-d:nimAllocStats"
  description: '''
    No heap cell must be allocated when control-flow exits the construction
    expression
  '''
  knownIssue.js vm: "``getAllocStats`` is not supported by the backends"
"""

type Ref = ref object
  a: int
  b: int

let start = getAllocStats()

proc get(): bool {.noinline.} =
  # prevent the optimizer from eliminating the branch
  result = true

block exit:
  # break out of the block before the construction is finished
  let r = Ref(a: 1, b: (if get(): break exit; 2))
  # no allocation must have took place

let stats = getAllocStats() - start

doAssert $stats == "(allocCount: 0, deallocCount: 0)"