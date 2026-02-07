discard """
  description: '''
    Specification-like test for covering what types are disallowed in .raises
    and .tags lists
  '''
  matrix: "--errorMax:0"
  action: reject
  target: native
"""

type
  ProperTag = object of RootEffect
  ProperException = object of CatchableError

# multiple ref or ptr indirections are not allowed
discard proc() {.raises: [ref ref ProperException].} #[tt.Error
                          ^ invalid type for raises/tags list ]#
discard proc() {.raises: [ptr ptr ProperException].} #[tt.Error
                          ^ invalid type for raises/tags list ]#
discard proc() {.raises: [ptr ref ProperException].} #[tt.Error
                          ^ invalid type for raises/tags list ]#

# # non-object types are not allowed in .raises effect lists
discard proc() {.raises: [int].} #[tt.Error
                          ^ invalid type for raises/tags list ]#
discard proc() {.raises: [ref int].} #[tt.Error
                          ^ invalid type for raises/tags list ]#
discard proc() {.raises: [ptr int].} #[tt.Error
                          ^ invalid type for raises/tags list ]#

# # multiple ref or ptr indirections are not allowed
# type
discard proc() {.tags: [ref ref ProperTag].} #[tt.Error
                        ^ invalid type for raises/tags list]#
discard proc() {.tags: [ptr ptr ProperTag].} #[tt.Error
                        ^ invalid type for raises/tags list]#
discard proc() {.tags: [ptr ref ProperTag].} #[tt.Error
                        ^ invalid type for raises/tags list]#

# # non-object types are not allowed in .tags effect lists
# type
discard proc() {.tags: [int].} #[tt.Error
                        ^ invalid type for raises/tags list]#
discard proc() {.tags: [ref int].} #[tt.Error
                        ^ invalid type for raises/tags list]#
discard proc() {.tags: [ptr int].} #[tt.Error
                        ^ invalid type for raises/tags list]#
