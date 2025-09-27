
discard """
  output: '''occupied ok: true
total ok: true'''
  joinable: false
  disabled: "osx"
"""

# if the tests fails in the CI, consider disabling it again. Refer to
# https://github.com/nim-lang/Nim/issues/8509 and
# https://github.com/nim-lang/Nim/issues/9421 for more information.

import std/[strutils, strformat]
import data

var m, lastSize, largestSize, fragmentation, defragmentation = 0

const
  fsLookupTable: array[byte, int8] = [
    -1'i8, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7
  ]

proc msbit(x: uint32): int {.inline.} =
  let a = if x <= 0xff_ff'u32:
            (if x <= 0xff: 0 else: 8)
          else:
            (if x <= 0xff_ff_ff'u32: 16 else: 24)
  result = int(fsLookupTable[byte(x shr a)]) + a

proc main =
  # for i in 0..1_000_000:
  for i in 0..100:
    let
      size = sizes[i mod sizes.len]
      frag = 1 shl msbit(uint32 size) div 5 - 2
        ## s^(fli)/sli - 2
      initialTotal = getTotalMem()
      initialOccupied = getOccupiedMem()

    m += size
    lastSize = size
    largestSize = if lastSize > largestSize: lastSize else: largestSize
    fragmentation += frag
    let p = alloc(size)
    if p == nil:
      quit "could not serve request!"
    dealloc p

    let
      afterTotal = getTotalMem()
      afterOccupied = getOccupiedMem()

    defragmentation

    if afterOccupied - initialOccupied > size + frag:
      echo afterOccupied - initialOccupied
      # echo "memory greater than anticipated at i: ", i, " size: ", size, " frag: ", frag, " delta: ", total - initialTotal - size - frag

    # c_fprintf(stdout, "iteration: %ld size: %ld\n", i, size)
  when defined(cpu64):
    # see https://github.com/nim-lang/Nim/issues/8509
    # this often made appveyor (on windows) fail with out of memory
    when defined(posix):
      # bug #7120
      var x = alloc(((1 shl 29) - 4) * 8)
      dealloc x

main()

let occ = getOccupiedMem()
let total = getTotalMem()

let width = ($m).len

echo "cumuliative allocated:.", fmt"""{(($m).insertSep(',')):.>15}"""
echo "largest allocated:.....", fmt"""{(($largestSize).insertSep(',')):.>15}"""
echo "last allocated:........", fmt"""{(($lastSize).insertSep(',')):.>15}"""
echo "fragmentation:.........", fmt"""{(($fragmentation).insertSep(',')):.>15}"""
echo "occupied:..............", fmt"""{(($occ).insertSep(',')):.>15}"""
echo "total:.................", fmt"""{(($total).insertSep(',')):.>15}"""

# Current values on Win64: 824KiB / 106.191MiB

echo "occupied ok: ", occ < 2 * 1024 * 1024
echo "total ok: ", total < 120 * 1024 * 1024
