discard """
  description: "Ensure that the internal thread management-data is freed"
  joinable: false
"""

import std/os

let startMem = getOccupiedSharedMem()

proc run(signal: ptr int) =
  # busy-wait until the signal is signaled
  while atomicLoadN(signal, ATOMIC_SEQ_CST) == 0:
    discard

block detached_thread:
  # case 1: detached thread (the spawned thread does the cleanup)
  var
    signal = 0
    thread: Thread[ptr int]

  thread.createThread(run, addr signal)
  # don't wake the thread up
  thread.reset()
  # now wake the thread up
  atomicStoreN(addr signal, 1, ATOMIC_SEQ_CST)

  # without the handle there's no way to know when the thread is finished,
  # so we sleep for some time
  sleep(100)
  doAssert getOccupiedSharedMem() == startMem

block joined_thread:
  # case 2: joined thread (the current thread does the cleanup)
  var
    signal = 0
    thread: Thread[ptr int]

  thread.createThread(run, addr signal)
  atomicStoreN(addr signal, 1, ATOMIC_SEQ_CST)
  thread.joinThread() # wait for the thread to finish
  reset thread # destroy the thread

  doAssert getOccupiedSharedMem() == startMem
