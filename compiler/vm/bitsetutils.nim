## This module implements bit-sets supporting an arbitrary amount of elements.
## The implementation is a thin, type-safe wrapper around
## ``compiler/utils/bitsets``.

# TODO: apply some polish (documentation, cleanup, etc.) and move this module
#       to ``compiler/utils``

import compiler/utils/bitsets

export bitsets

type BitSet*[T: Ordinal] = object
  data: TBitSet

func newBitSet*[T](_: typedesc[T], elems: Natural): BitSet[T] =
  result.data.bitSetInit((elems + 7) div 8)

func incl*[T](x: var BitSet[T], elem: T) {.inline.} =
  x.data.bitSetIncl(elem.BiggestInt)

func excl*[T](x: var BitSet[T], elem: T) {.inline.} =
  x.data.bitSetExcl(elem.BiggestInt)

func contains*[T](x: BitSet[T], elem: T): bool {.inline.} =
  x.data.bitSetIn(elem.BiggestInt)

func containsOrIncl*[T](x: BitSet[T], elem: T): bool {.inline.} =
  result = elem in x
  if not result:
    x.incl elem

iterator items*[T](x: BitSet[T]): T =
  var i = 0
  let L = x.data.len * 8
  while i < L:
    let b = x.data[i shr 3]
    if b shr (i and 7) != 0:
      yield T(i)

    inc i
