discard """
description: '''
Cannot mutate sealed location
'''
errormsg: "cannot borrow view; what it borrows from is potentially mutated"


"""

{.experimental: "views".}

proc `sealedBorrow`() =
  var simpleArray = [0, 1, 2, 3, 4, 5]

  var view: openarray[int] = toOpenArray(simpleArray, 1, 3)

  ## Openarray can only access a section of the original data, so indexing
  ## miggh be offset.
  doAssert view[0] == 1

  ## Because `view` is accessed later, it is not possible to mutate original data
  ## as it would result the change in the borrowed view.
  simpleArray[1] = 12

  doAssert view[0] == 12


`sealedBorrow`()