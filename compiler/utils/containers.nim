## This module contains ``seq``-based containers useful in contexts that make
## use of data-oriented design

import
  std/[
    options
  ]

type
  SeqMap*[K: Ordinal, V] = object
    ## Maps a 0-based integer-like key to a value, using a ``seq`` as the
    ## underlying storage. The default value for `V` is expected to indicated
    ## "empty" and an ``isFilled`` routine that returns a ``bool`` must
    ## exist for ``V``
    data: seq[V]

  Store*[I; T] = object
    ## Stores a sequence of `T` where each item is identified by an
    ## integer-like ID. The container is append-only
    data: seq[T]

  PartialStore*[I; T] = object
    ## Used for adding items to an existing `Store <#Store>`_ without directly
    ## modifying said object nor requiring access to it.
    ##
    ## The usual usage pattern is:
    ## 1. fork a partial store from a `Store <#Store>`_ object
    ## 2. add items to the partial store
    ## 3. once done, join the partial store with the store it was forked from
    ##
    ## It's also legal to add items to a partial store that wasn't forked from
    ## a store. In this case, it can only be joined into empty stores.
    data: seq[T]
    base: I

  Checkpoint* = distinct int
    ## Represents the state of a ``Store`` at some point in time.

  OrdinalSeq*[I: Ordinal, T] = distinct seq[T]
    ## Similar to a ``seq``, but can only be accessed by values of the
    ## specified type. This is useful in situations where the type of the
    ## index value is a ``distinct`` integer-like type

# ---------- SeqMap API ------------

func contains*[K, V](m: SeqMap[K, V], key: K): bool {.inline.} =
  ## Returns whether a value with key `key` exists in the map
  mixin isFilled
  result = ord(key) < m.data.len and isFilled(m.data[ord(key)])

func `[]`*[K, V](m: SeqMap[K, V], key: K): lent V {.inline.} =
  result = m.data[ord(key)]

func `[]`*[K, V](m: var SeqMap[K, V], key: K): var V {.inline.} =
  result = m.data[ord(key)]

func `[]=`*[K, V](m: var SeqMap[K, V], key: K, val: sink V) =
  let i = ord(key)
  if m.data.len <= i:
    m.data.setLen(i + 1)

  m.data[i] = val

iterator values*[K, V](m: SeqMap[K, V]): lent V =
  ## Returns, in an unspecified order, the value for each entry in the map `m`.
  mixin isFilled
  var i = 0
  let L = m.data.len
  while i < L:
    if isFilled(m.data[i]):
      yield m.data[i]
    inc i

iterator pairs*[K, V](m: SeqMap[K, V]): (K, lent V) =
  ## Returns, in an unspecified order, the key and value for each entry in the
  ## map `m`.
  mixin isFilled
  var i = 0
  let L = m.data.len
  while i < L:
    if isFilled(m.data[i]):
      yield (K(i), m.data[i])
    inc i

iterator mpairs*[K, V](m: var SeqMap[K, V]): (K, var V) =
  ## Returns, in an unspecified order, the key and mutable value for each entry
  ## in the map `m`.
  mixin isFilled
  var i = 0
  let L = m.data.len
  while i < L:
    if isFilled(m.data[i]):
      yield (K(i), m.data[i])
    inc i

# ---------- Store API ------------

func `==`*(a, b: Checkpoint): bool {.borrow.}

template `[]`*[I; T](x: Store[I, T], i: I): untyped =
  # TODO: convert to ``distinctBase`` instead
  x.data[int(i)]

template `[]=`*[I; T](x: var Store[I, T], i: I, it: T): untyped =
  ## Overwrites the item corresponding to `i` with `it`
  # TODO: convert to ``distinctBase`` instead
  x.data[int(i)] = it

iterator items*[I, T](x: Store[I, T]): lent T =
  ## Iterates over and returns all items in `x`
  var i = 0
  let L = x.data.len
  while i < L:
    yield x.data[i]
    inc i

iterator pairs*[I, T](x: Store[I, T]): (I, lent T) =
  ## Iterates over and returns all items in `x` together with their
  ## corresponding IDs
  var i = 0
  let L = x.data.len
  while i < L:
    # there's no need to perform a range check here: ``add`` already errors
    # when trying to add items for which the index can't be represented with
    # ``I``
    yield (I(i), x.data[i])
    inc i

iterator since*[I; T](s: Store[I, T], p: Checkpoint): (I, lent T) =
  ## Returns all items together with their ID added since `p` was created.
  for i in int(p)..<s.data.len:
    yield (I(i), s.data[i])

func nextId*[I; T](x: Store[I, T]): I {.inline.} =
  ## Returns the ID that would be assigned to the next added item.
  rangeCheck x.data.len.BiggestUInt < high(I).BiggestUInt
  result = I(x.data.len)

func checkpoint*(s: Store): Checkpoint =
  s.data.len.Checkpoint

func add*[I; T](x: var Store[I, T], it: sink T): I {.inline.} =
  ## Appends a new item to the Store and returns the ID assigned to
  ## it
  rangeCheck x.data.len.BiggestUInt < high(I).BiggestUInt
  x.data.add it
  result = I(x.data.high)

iterator mitems*[I; T](x: var Store[I, T]): var T =
  ## Yields a mutable item for each entry in `x`.
  var i = 0
  let L = x.data.len
  while i < L:
    yield x.data[i]
    inc i

func merge*[I; T](dst: var Store[I, T], src: sink Store[I, T]): Option[I] =
  ## Merges `src` into `dst` and returns the ID of the first-merged item. If
  ## `src` has no items, ``none(I)`` is returned.
  let start = dst.data.len
  rangeCheck start.BiggestUInt + src.data.len.BiggestUInt <= high(I).BiggestUInt

  dst.data.setLen(start + src.data.len)
  for i, it in src.data.mpairs:
    dst.data[start + i] = move(it)

  result =
    if src.data.len > 0: some I(start)
    else:                none(I)

func rewind*(s: var Store, p: Checkpoint) =
  ## Removes all items added since `p` was created. Do note that modifications
  ## to items already existing when `p` was created are not reverted.
  assert p.int <= s.data.len, "illegal rewind"
  s.data.setLen(p.int)

# ---------- PartialStore API -----------

func fork*[I;T](s: Store[I, T]): PartialStore[I, T] =
  ## Creates a new partial table that can later be joined back (via
  ## `join <#join,Store,PartialStore>`_) into `s`.
  PartialStore[I, T](base: nextId(s))

func add*[I;T](s: var PartialStore[I, T], item: sink T): I =
  ## Adds `item` to `s`, returning the ID to later query it with.
  s.data.add(item)
  result = I(ord(s.data.high) + ord(s.base))

func `[]`*[I;T](s: PartialStore[I, T], id: I): lent T {.inline.} =
  ## Returns the item associated with `id`.
  s.data[ord(id) - ord(s.base)]

func join*[I;T](s: var Store[I, T], other: sink PartialStore[I, T]) =
  ## Adds all items from `other` to `s`. `s` has to have the same number of
  ## items it had when `other` was forked from it.
  assert s.nextId() == other.base, "containers are out of sync"
  let offset = s.data.len
  s.data.setLen(offset + other.data.len)
  for i, it in other.data.mpairs:
    s.data[offset + i] = move it

# ---------- OrdinalSeq API ------------

template base*[I; T](x: OrdinalSeq[I, T]): seq[T] =
  ## Returns underlying seq of `x`.
  seq[T](x)

template len*[I; T](x: OrdinalSeq[I, T]): int =
  base(x).len

template `[]`*[I; T](x: OrdinalSeq[I, T], i: I): untyped =
  base(x)[ord i]

template `[]=`*[I; T](x: OrdinalSeq[I, T], i: I, item: T): untyped =
  base(x)[ord i] = item

func add*[I; T](x: var OrdinalSeq[I, T], item: sink T): I {.inline.} =
  base(x).add item
  result = I(base(x).high)

func newSeq*[I; T](x: var OrdinalSeq[I, T], len: int) {.inline.} =
  newSeq(base(x), len)

func setLen*[I; T](x: var OrdinalSeq[I, T], len: int) {.inline.} =
  setLen(base(x), len)

func synchronize*[I; A; B](x: var OrdinalSeq[I, A], s: Store[I, B]) =
  ## Synchronizes the number of elements `x` has with that of `s`.
  ##
  ## `s` must be larger than or equal in size to `x`: if this is
  ## not the case, behaviour is undefined.
  assert x.len <= s.data.len
  x.setLen(s.data.len)

iterator pairs*[I; T](x: OrdinalSeq[I, T]): (I, lent T) =
  var i = 0
  while i < x.len:
    yield (I(i), x[I(i)])
    inc i
