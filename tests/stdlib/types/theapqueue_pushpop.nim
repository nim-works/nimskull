discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/14139
    heapqueue pushpop() proc doesn't compile
  . swap() proc, which is used in pushpop() requires inputs of type var T,
    but in pushpop() the item variable is immutable.
  '''
"""
import heapqueue

var test_queue : HeapQueue[int]

test_queue.push(7)
test_queue.push(3)
test_queue.push(9)
let i = test_queue.pushpop(10)
doAssert i == 3

