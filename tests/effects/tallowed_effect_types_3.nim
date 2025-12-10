discard """
  description: '''
    Specification-like test for making sure only proper exception and tag
    types are allowed in .raises and .tags lists
  '''
  matrix: "--errorMax:0"
  action: reject
  target: native
  knownIssue: '''
    Arbitrary object types are currently allowed in both raises and
    tags effect lists
  '''
"""

type
  ImproperException = object
  ImproperTag = object

# multiple ref or ptr indirections are not allowed
discard proc() {.raises: [ImproperException].} #[tt.Error
                          ^ invalid type for raises/tags list ]#
discard proc() {.raises: [ref ImproperException].} #[tt.Error
                          ^ invalid type for raises/tags list ]#
discard proc() {.raises: [ptr ImproperException].} #[tt.Error
                          ^ invalid type for raises/tags list ]#

# multiple ref or ptr indirections are not allowed
discard proc() {.tags: [ImproperTag].} #[tt.Error
                        ^ invalid type for raises/tags list]#
discard proc() {.tags: [ref ImproperTag].} #[tt.Error
                        ^ invalid type for raises/tags list]#
discard proc() {.tags: [ptr ImproperTag].} #[tt.Error
                        ^ invalid type for raises/tags list]#
