discard """
  description: '''
    Regression test for an internal type and symbol loading bug, where packed
    item IDs were decoded from context of the wrong module
  '''
"""

import mdeferred_items_1, mdeferred_items_2

# first, create an unrelated instance. This forces the entries in the
# instantiation cache to be resolved, but without ``Type[int]`` being
# explicitly used prior (<-- this is important)
doAssert test[int]() == 1

# invoke the instantiation created by ``mdeferred_items_2``
doAssert call() == 1
# create the same instantiation as ``mdeferred_items_2`` does. If the generic
# cache worked and it's really the same, this is the second call to it
doAssert test[Type[int]]() == 2

#!EDIT!#
discard """
"""

import mdeferred_items_1, mdeferred_items_2

# logically the same as above, just without comments in order to force a
# recompilation
doAssert test[int]() == 1
doAssert call() == 1
doAssert test[Type[int]]() == 2