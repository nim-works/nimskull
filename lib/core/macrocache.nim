#
#
#            Nim's Runtime Library
#        (c) Copyright 2018 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module provides an API for macros to collect compile-time information
## across module boundaries. It should be used instead of global `{.compileTime.}`
## variables as those break incremental compilation.
##
## The main feature of this module is that if you create `CacheTable`s or
## any other `Cache` types with the same name in different modules, their
## content will be shared, meaning that you can fill a `CacheTable` in
## one module, and iterate over its contents in another.

runnableExamples:
  import std/macros

  const mcTable = CacheTable"myTable"
  const mcSeq = CacheSeq"mySeq"
  const mcCounter = CacheCounter"myCounter"

  static:
    # add new key "val" with the value `myval`
    let myval = newLit("hello ic")
    mcTable["val"] = myval
    assert mcTable["val"].kind == nnkStrLit

  # Can access the same cache from different static contexts
  # All the information is retained
  static:
    # get value from `mcTable` and add it to `mcSeq`
    mcSeq.add(mcTable["val"])
    assert mcSeq.len == 1

  static:
    assert mcSeq[0].strVal == "hello ic"

    # increase `mcCounter` by 3
    mcCounter.inc(3)
    assert mcCounter.value == 3

import system/magics

export magics.CacheSeq, magics.CacheTable, magics.CacheCounter
export magics.value, magics.len
export magics.inc, magics.add, magics.incl, magics.`[]`, magics.`[]=`

iterator items*(s: CacheSeq): NimNode =
  ## Iterates over each item in `s`.
  runnableExamples:
    import std/macros
    const myseq = CacheSeq"itemsTest"

    static:
      myseq.add(newLit(5))
      myseq.add(newLit(42))

      for val in myseq:
        # check that all values in `myseq` are int literals
        assert val.kind == nnkIntLit

  for i in 0 ..< len(s): yield s[i]

iterator pairs*(t: CacheTable): (string, NimNode) =
  ## Iterates over all `(key, value)` pairs in `t`.
  runnableExamples:
    import std/macros
    const mytabl = CacheTable"values"

    static:
      mytabl["intVal"] = newLit(5)
      mytabl["otherVal"] = newLit(6)
      for key, val in mytabl:
        # make sure that we actually get the same keys
        assert key in ["intVal", "otherVal"]

        # all vals are int literals
        assert val.kind == nnkIntLit

  var h = 0
  while hasNext(t, h):
    let (a, b, h2) = next(t, h)
    yield (a, b)
    h = h2
