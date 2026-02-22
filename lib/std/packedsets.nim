#
#
#                 NimSkull's runtime library
#              (c) Copyright 2012 Andreas Rumpf
#     (c) Copyright 2024 Leorize <leorize+oss@disroot.org>
#
# See the file "copying.txt", included in this distribution, for
# details about copyright.

import std/tables

## The `packedsets` module implements an efficient `Ordinal` set implemented as a
## simple sparse bitmap:idx:.
##
## This bitmap is optimized for relatively low number of values (less than 1
## million entries) and trade memory usage for time.
##
## See also
## ========
## * `sets module <sets.html>`_ for more general hash sets

type
  Word = set[uint8]
    ## A bitmap "word".

  PackedSet*[A: Ordinal] = object
    ## An efficient set of `Ordinal` implemented as a sparse bitmap.
    # Bootstrap is tripped by the sizeof
    when defined(nimKochBootstrap):
      bitmap: Table[uint64, Word]
    else:
      when sizeof(A) > sizeof(uint32):
        bitmap: Table[uint64, Word]
      else:
        bitmap: Table[uint32, Word]

template internalType(A: typedesc[Ordinal]): untyped =
  ## Return the internal type used for representation of `A`.
  when defined(nimKochBootstrap):
    uint64
  elif sizeof(A) > sizeof(uint32):
    uint64
  else:
    uint32

proc initPackedSet*[A]: PackedSet[A] {.inline.} =
  ## Returns an empty `PackedSet[A]`.
  ## `A` must be `Ordinal`.
  ##
  ## **See also:**
  ## * `toPackedSet proc <#toPackedSet,openArray[A]>`_
  runnableExamples:
    let a = initPackedSet[int]()
    assert len(a) == 0

    type Id = distinct int
    var ids = initPackedSet[Id]()
    ids.incl(3.Id)

  PackedSet[A]()

proc initPackedSet[A](cap: Natural): PackedSet[A] {.inline.} =
  ## Creates a PackedSet with `cap` buckets preallocated.
  result.bitmap = initTable[internalType(A), Word](cap)

proc len*[A](s: PackedSet[A]): int {.inline.} =
  ## Returns the number of elements in `s`.
  runnableExamples:
    let a = [1, 3, 5].toPackedSet
    assert len(a) == 3

  for word in s.bitmap.values:
    result.inc len(word)

proc clear*[A](result: var PackedSet[A]) {.inline.} =
  ## Clears the `PackedSet[A]` back to an empty state.
  runnableExamples:
    var a = [5, 7].toPackedSet
    clear(a)
    assert len(a) == 0

  clear result.bitmap

proc toInternal[A](x: A): uint16 or uint32 or uint64 {.inline.} =
  ## Convert `x` into the internal representation.
  cast[internalType(A)](x)

proc split[T: uint32 or uint64](key: T): tuple[high: T, low: uint8] {.inline.} =
  ## Split `key` into low and high portions
  (key shr 8, uint8(key))

proc join[T: uint32 or uint64](high: T, low: uint8): T {.inline.} =
  ## Join high and low portions into the original value
  (high shl 8) or T(low)

proc contains*[A](s: PackedSet[A], key: A): bool {.inline.} =
  ## Returns true if `key` is in `s`.
  ##
  ## This allows the usage of the `in` operator.
  runnableExamples:
    type ABCD = enum A, B, C, D

    let a = [1, 3, 5].toPackedSet
    assert a.contains(3)
    assert 3 in a
    assert not a.contains(8)
    assert 8 notin a

    let letters = [A, C].toPackedSet
    assert A in letters
    assert C in letters
    assert B notin letters

  let (high, low) = split(key.toInternal)
  s.bitmap.getOrDefault(high, {}).contains(low)

proc containsOrIncl*[A](s: var PackedSet[A], key: A): bool {.inline.} =
  ## Includes `key` in the set `s` and tells if `key` was already in `s`.
  ##
  ## The difference with regards to the `incl proc <#incl,PackedSet[A],A>`_ is
  ## that this proc returns true if `s` already contained `key`. The
  ## proc will return false if `key` was added as a new value to `s` during
  ## this call.
  ##
  ## **See also:**
  ## * `incl proc <#incl,PackedSet[A],A>`_ for including an element
  ## * `missingOrExcl proc <#missingOrExcl,PackedSet[A],A>`_
  runnableExamples:
    var a = initPackedSet[int]()
    assert a.containsOrIncl(3) == false
    assert a.containsOrIncl(3) == true
    assert a.containsOrIncl(4) == false

  let (high, low) = split(key.toInternal)
  if high notin s.bitmap:
    s.bitmap[high] = {low}
    result = false
  else:
    result = s.bitmap[high].contains(low)
    s.bitmap[high].incl(low)

proc incl*[A](s: var PackedSet[A], key: A) {.inline.} =
  ## Includes an element `key` in `s`.
  ##
  ## This doesn't do anything if `key` is already in `s`.
  ##
  ## **See also:**
  ## * `excl proc <#excl,PackedSet[A],A>`_ for excluding an element
  ## * `incl proc <#incl,PackedSet[A],PackedSet[A]>`_ for including a set
  ## * `containsOrIncl proc <#containsOrIncl,PackedSet[A],A>`_
  runnableExamples:
    var a = initPackedSet[int]()
    a.incl(3)
    a.incl(3)
    assert len(a) == 1

  let (high, low) = split(key.toInternal)
  s.bitmap.mgetOrPut(high, {low}).incl(low)

proc incl*[A](s: var PackedSet[A], other: PackedSet[A]) {.inline.} =
  ## Includes all elements from `other` into `s`.
  ##
  ## This is the in-place version of `s + other <#+,PackedSet[A],PackedSet[A]>`_.
  ##
  ## **See also:**
  ## * `excl proc <#excl,PackedSet[A],PackedSet[A]>`_ for excluding a set
  ## * `incl proc <#incl,PackedSet[A],A>`_ for including an element
  ## * `containsOrIncl proc <#containsOrIncl,PackedSet[A],A>`_
  runnableExamples:
    var a = [1].toPackedSet
    a.incl([5].toPackedSet)
    assert len(a) == 2
    assert 5 in a

  for high, word in other.bitmap.pairs:
    let merged = s.bitmap.getOrDefault(high, {}) + word
    s.bitmap[high] = merged

proc toPackedSet*[A](x: openArray[A]): PackedSet[A] =
  ## Creates a new `PackedSet[A]` that contains the elements of `x`.
  ##
  ## Duplicates are removed.
  ##
  ## **See also:**
  ## * `initPackedSet proc <#initPackedSet>`_
  runnableExamples:
    let a = [5, 6, 7, 8, 8].toPackedSet
    assert len(a) == 4
    assert $a == "{5, 6, 7, 8}"

  for value in x.items:
    result.incl(value)

proc missingOrExcl*[A](s: var PackedSet[A], key: A): bool {.inline.} =
  ## Excludes `key` from the set `s` and tells if `key` was already missing from `s`.
  ##
  ## The difference with regards to the `excl proc <#excl,PackedSet[A],A>`_ is
  ## that this proc returns true if `key` was missing from `s`.
  ## The proc will return false if `key` was in `s` and it was removed
  ## during this call.
  ##
  ## **See also:**
  ## * `excl proc <#excl,PackedSet[A],A>`_ for excluding an element
  ## * `excl proc <#excl,PackedSet[A],PackedSet[A]>`_ for excluding a set
  ## * `containsOrIncl proc <#containsOrIncl,PackedSet[A],A>`_
  runnableExamples:
    var a = [5].toPackedSet
    assert a.missingOrExcl(5) == false
    assert a.missingOrExcl(5) == true

  let (high, low) = split(key.toInternal)
  if high notin s.bitmap:
    result = true
  else:
    result = not s.bitmap[high].contains(low)
    s.bitmap[high].excl(low)
    if s.bitmap[high].len == 0:
      s.bitmap.del(high)

proc excl*[A](s: var PackedSet[A], key: A) {.inline.} =
  ## Excludes `key` from the set `s`.
  ##
  ## This doesn't do anything if `key` is not found in `s`.
  ##
  ## **See also:**
  ## * `incl proc <#incl,PackedSet[A],A>`_ for including an element
  ## * `excl proc <#excl,PackedSet[A],PackedSet[A]>`_ for excluding a set
  ## * `missingOrExcl proc <#missingOrExcl,PackedSet[A],A>`_
  runnableExamples:
    var a = [3].toPackedSet
    a.excl(3)
    a.excl(3)
    a.excl(99)
    assert len(a) == 0

  discard s.missingOrExcl(key)

proc excl*[A](s: var PackedSet[A], other: PackedSet[A]) =
  ## Excludes all elements from `other` from `s`.
  ##
  ## This is the in-place version of `s - other <#-,PackedSet[A],PackedSet[A]>`_.
  ##
  ## **See also:**
  ## * `incl proc <#incl,PackedSet[A],PackedSet[A]>`_ for including a set
  ## * `excl proc <#excl,PackedSet[A],A>`_ for excluding an element
  ## * `missingOrExcl proc <#missingOrExcl,PackedSet[A],A>`_
  runnableExamples:
    var a = [1, 5].toPackedSet
    a.excl([5].toPackedSet)
    assert len(a) == 1
    assert 5 notin a

  for high, word in other.bitmap.pairs:
    if high in s.bitmap:
      let excluded = s.bitmap[high] - word
      if excluded.len == 0:
        s.bitmap.del high
      else:
        s.bitmap[high] = excluded

proc union*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Returns the union of the sets `s1` and `s2`.
  ##
  ## The same as `s1 + s2 <#+,PackedSet[A],PackedSet[A]>`_.
  runnableExamples:
    let
      a = [1, 2, 3].toPackedSet
      b = [3, 4, 5].toPackedSet
      c = union(a, b)
    assert c.len == 5
    assert c == [1, 2, 3, 4, 5].toPackedSet

  result = s1
  result.incl s2

proc intersection*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Returns the intersection of the sets `s1` and `s2`.
  ##
  ## The same as `s1 * s2 <#*,PackedSet[A],PackedSet[A]>`_.
  runnableExamples:
    let
      a = [1, 2, 3].toPackedSet
      b = [3, 4, 5].toPackedSet
      c = intersection(a, b)
    assert c.len == 1
    assert c == [3].toPackedSet

  if s1.bitmap.len < s2.bitmap.len:
    result = intersection(s2, s1)
  else:
    result = initPackedSet[A](s1.bitmap.len)
    for high, word in s1.bitmap.pairs:
      if high in s2.bitmap:
        let intersection = s2.bitmap[high] * word
        if intersection.len > 0:
          result.bitmap[high] = intersection

proc difference*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Returns the difference of the sets `s1` and `s2`.
  ##
  ## The same as `s1 - s2 <#-,PackedSet[A],PackedSet[A]>`_.
  runnableExamples:
    let
      a = [1, 2, 3].toPackedSet
      b = [3, 4, 5].toPackedSet
      c = difference(a, b)
    assert c.len == 2
    assert c == [1, 2].toPackedSet

  result = initPackedSet[A](s1.bitmap.len)
  for high, word in s1.bitmap.pairs:
    if high in s2.bitmap:
      let difference = word - s2.bitmap[high]
      if difference.len > 0:
        result.bitmap[high] = difference
    else:
      result.bitmap[high] = word

proc symmetricDifference*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Returns the symmetric difference of the sets `s1` and `s2`.
  runnableExamples:
    let
      a = [1, 2, 3].toPackedSet
      b = [3, 4, 5].toPackedSet
      c = symmetricDifference(a, b)
    assert c.len == 4
    assert c == [1, 2, 4, 5].toPackedSet

  result = s1
  for high, word in s2.bitmap.pairs:
    let rword = result.bitmap.getOrDefault(high)
    let difference = (rword + word) - (rword * word)
    if difference.len > 0:
      result.bitmap[high] = difference
    else:
      result.bitmap.del(high)

proc disjoint*[A](s1, s2: PackedSet[A]): bool {.inline.} =
  ## Returns true if the sets `s1` and `s2` have no items in common.
  runnableExamples:
    let
      a = [1, 2].toPackedSet
      b = [2, 3].toPackedSet
      c = [3, 4].toPackedSet
    assert disjoint(a, b) == false
    assert disjoint(a, c) == true

  if s1.bitmap.len < s2.bitmap.len:
    result = disjoint(s2, s1)
  else:
    result = true
    for high, word in s1.bitmap.pairs:
      if high in s2.bitmap:
        let intersection = s2.bitmap[high] * word
        if intersection.len > 0:
          return false

proc card*[A](s: PackedSet[A]): int {.inline.} =
  ## Alias for `len() <#len,PackedSet[A]>`_.
  ##
  ## Card stands for the [cardinality](http://en.wikipedia.org/wiki/Cardinality)
  ## of a set.
  result = s.len()

proc `<=`*[A](s1, s2: PackedSet[A]): bool {.inline.} =
  ## Returns true if `s1` is a subset of `s2`.
  ##
  ## A subset `s1` has all of its elements in `s2`, but `s2` doesn't necessarily
  ## have more elements than `s1`. That is, `s1` can be equal to `s2`.
  runnableExamples:
    let
      a = [1].toPackedSet
      b = [1, 2].toPackedSet
      c = [1, 3].toPackedSet
    assert a <= b
    assert b <= b
    assert not (c <= b)

  if s1.bitmap.len > s2.bitmap.len:
    return false

  result = true
  for high, word in s1.bitmap.pairs:
    if high notin s2.bitmap or not (word <= s2.bitmap[high]):
      return false

proc `<`*[A](s1, s2: PackedSet[A]): bool {.inline.} =
  ## Returns true if `s1` is a proper subset of `s2`.
  ##
  ## A strict or proper subset `s1` has all of its elements in `s2`, but `s2` has
  ## more elements than `s1`.
  runnableExamples:
    let
      a = [1].toPackedSet
      b = [1, 2].toPackedSet
      c = [1, 3].toPackedSet
    assert a < b
    assert not (b < b)
    assert not (c < b)

  s1.len < s2.len and s1 <= s2

proc `==`*[A](s1, s2: PackedSet[A]): bool =
  ## Returns true if both `s1` and `s2` have the same elements and set size.
  runnableExamples:
    assert [1, 2].toPackedSet == [2, 1].toPackedSet
    assert [1, 2].toPackedSet == [2, 1, 2].toPackedSet

  if s1.bitmap.len != s2.bitmap.len:
    return false

  result = true
  for high, word in s1.bitmap.pairs:
    if high notin s2.bitmap or word != s2.bitmap[high]:
      return false

iterator items*[A](s: PackedSet[A]): A {.inline.} =
  ## Iterates over included elements of `s`.
  for high, word in s.bitmap.pairs:
    for low in word.items:
      yield cast[A](join(high, low))

proc `$`*[A](s: PackedSet[A]): string =
  ## Returns the string representation of `s`.
  result.add '{'
  for value in s.items:
    let value = cast[A](value)
    if result.len > 1:
      result.add ", "
    result.add $value
  result.add '}'

proc `+`*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `union(s1, s2) <#union,PackedSet[A],PackedSet[A]>`_.
  union(s1, s2)

proc `*`*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `intersection(s1, s2) <#intersection,PackedSet[A],PackedSet[A]>`_.
  intersection(s1, s2)

proc `-`*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `difference(s1, s2) <#difference,PackedSet[A],PackedSet[A]>`_.
  difference(s1, s2)

proc `-+-`*[A](s1, s2: PackedSet[A]): PackedSet[A] {.inline.} =
  ## Alias for `symmetricDifference(s1, s2) <#symmetricDifference,PackedSet[A],PackedSet[A]>`_.
  symmetricDifference(s1, s2)

proc isNil*[A](x: PackedSet[A]): bool {.inline, deprecated: "PackedSet can no longer be nil; use `x.len == 0` instead".} =
  ## Returns true if `x` is empty, false otherwise.
  x.len == 0

proc assign*[A](dest: var PackedSet[A], src: sink PackedSet[A]) {.inline, deprecated: "PackedSet is now always deep copied; use `dest = src` instead".} =
  ## Copies `src` to `dest`.
  dest = src
