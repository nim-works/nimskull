discard """
description: '''
Test structural S-expression comparison. Correctness of
this test is very important, since it directly influences
testament UX when it comes to structural data comparisons.
'''
"""

import experimental/[sexp, sexp_diff, colordiff]
import std/[tables, algorithm]

proc sortmatches(s1, s2: seq[SexpNode], dir: SortOrder = Ascending):
  tuple[
    lhsIgnore, rhsIgnore: seq[int],
    map: seq[tuple[key: (int, int), diff: seq[SexpMismatch]]],
  ] =

  var diffMap = TableRef[(int, int), seq[SexpMismatch]]()

  proc reportCmp(a, b: int): int =
    # NOTE weight function used in the test formatting - it is based on the
    # number of mismatches. Other implementations might be more
    # involved/optimized, but for the testing purposes simple version will
    # do.
    let diff = diff(s1[a], s2[b])
    diffMap[(a, b)] = diff
    return diff.len

  let (expected, given, sorted) = stableMatch(
    s1.len, s2.len, reportCmp, dir)

  for (key, val) in sorted:
    result.map.add((key, diffMap[key]))

  result.lhsIgnore = expected
  result.rhsIgnore = given

proc sortmatchesIdx(
    s1, s2: seq[string],
    dir: SortOrder = Ascending
  ): auto =

  var ss1, ss2: seq[SexpNode]
  for item in s1: ss1.add parseSexp(item)
  for item in s2: ss2.add parseSexp(item)
  let (l, r, mis) = sortmatches(ss1, ss2, dir)
  return (expected: l, given: r, idxs: mis, parsed: (ss1, ss2))

proc sortmatches(
    s1, s2: seq[string],
    dir: SortOrder = Ascending
  ): tuple[
    expected, given: seq[int],
    map: seq[tuple[key: (SexpNode, SexpNode), diff: seq[SexpMismatch]]]] =
  var (expected, given, idxs, parsed) = sortmatchesIdx(s1, s2, dir)
  for (idx, diff) in idxs:
    result.map.add(((parsed[0][idx[0]], parsed[1][idx[1]]), diff))

  result.expected = expected
  result.given = given

proc matches(s1, s2: string): seq[SexpMismatch] =
  diff(s1.parseSexp(), s2.parseSexp())

proc eq[T](a, b: T) =
  doAssert a == b, "a was " & $a & ", b was " & $b

block literal:
  let d = matches("(1)", "(2)")
  eq d.len, 1
  eq(d[0].kind, smDifferentLiteral)

block symbol:
  let d = matches("(A)", "(B)")
  eq d.len, 1
  eq d[0].kind, smDifferentSymbol

block match_all:
  let d = matches("(_)", "(Q)")
  eq d.len, 0

block match_keys:
  block ordered_unordered:
    for (m1, m2) in @[
      ("(:a b :c d)", "(:a q :c z)"),
      ("(:a b :c d)", "(:c q :a z)")
    ]:
      let d = matches(m1, m2)
      eq d.len, 2
      # Mismatches are placed in the same order as they are used in the input
      # text
      eq d[0].path[0].key, "a"
      eq d[1].path[0].key, "c"
      eq d[0].kind, smDifferentSymbol
      eq d[1].kind, smDifferentSymbol

  block elements:
    let d = matches("(User :key 12)", "(Azer :id 14 :key 3)")
    eq d.len, 2
    eq d[0].kind, smDifferentLiteral
    eq d[1].kind, smDifferentSymbol

block weighed_matching:
  let d = sortmatchesIdx(@["(User)"], @["(User2)"]).idxs
  eq d[0].key, (0, 0)
  eq d[0].diff.len, 1
  eq d[0].diff[0].kind, smDifferentSymbol

block hint_matching:
  let d = sortmatches(@[
    """(User :location ("tfile.nim" 11 7) :severity Hint :str "Another hint")""",
    """(User :location ("tfile.nim" 8 _) :str "User Hint")"""
  ], @[
    """(User :location ("tfile.nim" 8 _) :str "User Hint")""",
    """(User :location ("tfile.nim" 9 6) :severity Hint :str "User hint")"""
  ])

block direct_1_1:
  let d = sortmatchesIdx(@["(T)"], @["(T)"], Descending).idxs
  # Structural mapping between two groups with no differences, and one
  # element in each one obviously shows no differences in a single mapping pair
  eq d.len, 1
  eq d[0].diff.len,  0

  # And direct mapping between each element
  eq d[0].key, (0, 0)

block more_expected:
  block descending:
    let (expected, given, idxs, _) = sortmatchesIdx(
      @["(T1)", "(T2)"], @["(T1)"], Descending)

    # If expected data has more elements then best mapping will be put into
    # results, and all other variants will be discarded.

    eq expected, @[1] # discarding `T2` as it does not match
    eq given, @[]

    eq idxs.len, 1
    eq idxs[0].diff.len, 0
    eq idxs[0].key, (0, 0)

  block:
    # Note that when used in `Ascending` option, this mapping will be
    # revesed - worst possible (highest diff cost) options will be
    # assigned.
    let (exp, give, d, _) = sortmatchesIdx(
      @["(T1)", "(T2)"], @["(T1)"], Ascending)

    # Low-weight match was discarded, higher-valued one was selected
    # instead.
    eq exp, @[0]
    eq give, @[]

    eq d.len, 1
    eq d[0].diff.len, 1
    eq d[0].key, (1, 0)
    eq d[0].diff[0].kind, smDifferentSymbol

block more_given:
  let (e, g, d, _) = sortmatchesIdx(@["(T1)"], @["(T1)", "(T2)"], Descending)

  # If more input mappings are given then everything that was not matched
  # is discarded
  eq e, @[]
  eq g, @[1]
  eq d.len, 1
  eq d[0].diff.len, 0

block:
  # In case of multiple possible pairings the matching heavily depends on
  # the ordering option. In case of Ascending it will generate the highest
  # overall cost, and for descending it will make a lowest overall cost.
  let (s1, s2) = (@["(A B C R)", "(A Q D R)"],
                  @["(A B C E)", "(A Q D E)"])

  block ascending:
    let (e, g, d, _) = sortmatchesIdx(s1, s2, Ascending)

    eq e, @[]
    eq g, @[]

    eq d.len, 2
    # Overall cost - 6, best possible matching for Ascending ordering
    eq d[0].diff.len, 3
    eq d[1].diff.len, 3

    let (d1, d2, d3) = (d[0].diff[0], d[0].diff[1], d[0].diff[2])

    eq d[0].key, (0, 1)
    eq d[1].key, (1, 0)

    eq d1.path[0].index, 1
    eq d1.kind, smDifferentSymbol
    eq d1.expected.getSymbol(), "B"
    eq d1.found.getSymbol(), "Q"

    eq d2.path[0].index, 2
    eq d2.kind, smDifferentSymbol

    eq d3.path[0].index, 3
    eq d3.kind, smDifferentSymbol

  block descending:
    let (e, g, d, _) = sortmatchesIdx(s1, s2, Descending)

    eq e, @[]
    eq g, @[]

    eq d.len, 2
    # Overall cost - 2, best possible matching for Descending ordering
    eq d[0].diff.len, 1
    eq d[1].diff.len, 1


    eq d[0].key, (0, 0)
    eq d[1].key, (1, 1)

    let d1 = (d[0].diff[0])
    eq d1.path[0].index, 3
    eq d1.kind, smDifferentSymbol
    eq d1.expected.getSymbol(), "R"
    eq d1.found.getSymbol(), "E"

block string_roundtrip:
  # a string containing control character must roundtrip through
  # stringification
  var str = newString(128) # only test characters in the ASCII range
  for i, c in str.mpairs:
    c = char(i)

  let asText = toLine(newSString(str)).toString(false)
  doAssert str == parseSexp(asText).getStr()

if false:
  # Don't delete this section, it is used for print-debugging expected
  # formatting. And yes, 'if' is intentional as well - code needs to
  # compile, running is optional.
  for (lhs, rhs) in @[
    (@["(A B C R)", "(A Q D R)"],
     @["(A B C E)", "(A Q D E)"])
  ]:
    for dir in [Ascending, Descending]:
      echo ">>>"
      let (expected, given, map) = sortmatches(lhs, rhs, dir)
      for (pair, diff) in map:
        echo "-- ", pair[0]
        echo "++ ", pair[1]
        echo describeDiff(diff, diffFormatter())

      echo "-? ", expected
      echo "+? ", given
