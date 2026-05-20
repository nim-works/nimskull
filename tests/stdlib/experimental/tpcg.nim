discard """
  targets: "c js"
  knownIssueknownIssue.vm: '''

  '''
"""

import std/unittest
import experimental/pcg


suite "PCG PRNG Stability":
  test "Identical sequence on all backends":
    var rng = initPcg(seed = 42'u64, seq = 123'u64)

    # We assert against hardcoded values to ensure cross-backend stability.
    # These values were obtained from the initial implementation.
    check rng.next() == 1365045034'u32
    check rng.next() == 550752490'u32
    check rng.next() == 398886483'u32
    check rng.next() == 1580311325'u32
    check rng.next() == 1309372251'u32


  test "Different seeds produce different sequences":
    var rng1 = initPcg(seed = 1'u64)
    var rng2 = initPcg(seed = 2'u64)
    check rng1.next() != rng2.next()


  test "Different sequences produce different results":
    var rng1 = initPcg(seed = 42'u64, seq = 1'u64)
    var rng2 = initPcg(seed = 42'u64, seq = 2'u64)
    check rng1.next() != rng2.next()
