## This module contains ``seq``-based containers useful in contexts that make
## use of data-oriented design

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

# ---------- SeqMap API ------------

func contains*[K, V](m: SeqMap[K, V], key: K): bool {.inline.} =
  ## Returns whether a value with key `key` exists in the map
  mixin isFilled
  result = ord(key) < m.data.len and isFilled(m.data[ord(key)])

func `[]`*[K, V](m: SeqMap[K, V], key: K): lent V {.inline.} =
  result = m.data[ord(key)]

func `[]=`*[K, V](m: var SeqMap[K, V], key: K, val: sink V) =
  let i = ord(key)
  if m.data.len <= i:
    m.data.setLen(i + 1)

  m.data[i] = val


# ---------- Store API ------------

template `[]`*[I; T](x: Store[I, T], i: I): untyped =
  # TODO: convert to ``distinctBase`` instead
  x.data[int(i)]

template `[]=`*[I; T](x: var Store[I, T], i: I, it: T): untyped =
  ## Overwrites the item corresponding to `i` with `it`
  # TODO: convert to ``distinctBase`` instead
  x.data[int(i)] = it

func add*[I; T](x: var Store[I, T], it: sink T): I {.inline.} =
  ## Appends a new item to the Store and returns the ID assigned to
  ## it
  rangeCheck x.data.len.BiggestUInt < high(I).BiggestUInt
  x.data.add it
  result = I(x.data.high)