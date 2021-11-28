discard """
description: '''
Creating and accessing views for the array types
'''

"""

{.experimental: "views".}

## feature-documentation

proc `sealedBorrow`() =
  var simpleArray = [0, 1, 2, 3, 4, 5]

  var view: openarray[int] = toOpenArray(simpleArray, 1, 3)

  ## Openarray can only access a section of the original data, so indexing
  ## miggh be offset.
  doAssert view[0] == 1

  ## Underlying elements might be modified
  simpleArray[1] = 12

  ## And as a result, view into this section is also affected.
  doAssert view[0] == 12

  view[2] = 24

  doAssert simpleArray[3] == 24

`sealedBorrow`()


