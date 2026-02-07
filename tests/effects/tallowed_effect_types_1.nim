discard """
  description: '''
    Specification-like test for covering what types are allowed in .raises and
    .tags lists
  '''
  target: native
"""

type
  ProperTag = object of RootEffect
  ProperException = object of CatchableError

# proper exception types are usable in a .raises effect list, including with
# a single ref or ptr indirection
type Test1 = proc() {.raises: [ProperException].}
type Test2 = proc() {.raises: [ref ProperException].}
type Test3 = proc() {.raises: [ptr ProperException].}

# proper tag types are usable in a .tags effect list, including with a single
# ref or ptr indirection
type Test4 = proc() {.tags: [ProperTag].}
type Test5 = proc() {.tags: [ref ProperTag].}
type Test6 = proc() {.tags: [ptr ProperTag].}
