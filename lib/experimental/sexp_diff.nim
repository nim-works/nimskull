import
  ./sexp,
  ./colortext,
  ./colordiff,
  std/[
    strformat,
    sequtils,
    strutils,
    tables,
    intsets,
    options,
    algorithm
  ]

type IdxCostMap* = Table[(int, int), int]

proc randomKey[K, V](table: OrderedTable[K, V]): K =
  for k, v in pairs(table):
    return k

proc stableMatch*(
    lhsLen, rhsLen: int,
    weight: proc(a, b: int): int,
    order: SortOrder = SortOrder.Ascending
  ): tuple[
    lhsIgnore, rhsIgnore: seq[int],
    map: seq[tuple[pair: (int, int), cost: int]]
  ] =
  ## Do a weighted matching of the items in lhs and rhs sequences using
  ## weight function. Return most cost-effective matching elements.
  ##
  ## - `lhsLen` and `rhsLen` lists number of the elements in each input
  ##   sequence
  ## - `weight` - comparison proc that returns match score between
  ##   two items as position `a` and `b`.
  ## - `order` - comparison ordering. If it is `Ascending` higher matching
  ##   cost is consdered better and replaces previous mappings. If `Descending`
  ##   prefer lower matching cost instead
  ##
  ## For generating mapping of two sequences make `weight` function a
  ## closure and let it retrieve values as needed.

  var canTry: OrderedTable[int, seq[int]]
  var rmap: OrderedTable[int, (int,int)]

  for l in 0 ..< lhsLen:
    canTry[l] = @[]
    for r in 0 ..< rhsLen:
      canTry[l].add r

  proc getCost(l, r: int, res: var IdxCostMap): int =
    if (l, r) notin res:
      res[(l, r)] = weight(l, r)

    res[(l, r)]

  var tmp: IdxCostMap
  while 0 < len(canTry):
    let l = canTry.randomKey()
    let r = canTry[l].pop()
    if r in rmap:
      let (oldL, _) = rmap[r]
      let tryCost = getCost(l, r, tmp)
      let otherCost = getCost(oldL, r, tmp)
      let better =
        if order == Ascending:
          otherCost < tryCost
        else:
          otherCost > tryCost

      if better:
        rmap[r] = (l, r)

    else:
      discard getCost(l, r, tmp)
      rmap[r] = (l, r)

    if canTry[l].len() == 0:
      canTry.del l

  var tmpMap: seq[((int, int), int)] = toSeq(pairs(tmp))
  sort(
    tmpMap,
    proc(a, b: ((int, int), int)): int =
      if a[1] == b[1]:
        cmp(a[0], b[0])
      elif order == Descending:
        cmp(a[1], b[1])
      else:
        -cmp(a[1], b[1])
  )

  var seenLeft: IntSet
  var seenRight: IntSet

  for (key, val) in tmpMap:
    if key[0] notin seenLeft and key[1] notin seenRight:
      result.map.add((key, val))
      seenLeft.incl key[0]
      seenRight.incl key[1]

  for idx in 0 ..< rhsLen:
    if idx notin seenRight:
      result.rhsIgnore.add idx

  for idx in 0 ..< lhsLen:
    if idx notin seenLeft:
      result.lhsIgnore.add idx

export `$`, toString

type
  SexpPathPartKind = enum
    ## Kind of the s-expression mismatch path part
    spIndex
    spKey

  SexpPathPart = object
    ## S-expression mismatch part kind
    case kind*: SexpPathPartKind
      of spIndex:
        index*: int ## Mismatch at index

      of spKey:
        key*: string ## Mismatch for a given `:key`

  SexpPath* = seq[SexpPathPart]

  SexpMismatchKind* = enum
    ## Possible kinds of the mismatches
    smMissingKey ## Input data has no `:key` that was present in expected
    smDifferentLiteral ## Target has different literal values from the expected
    smDifferentSymbol ## Target has different symbol at position
    smArrayLen ## Mismatched array len
    smKindMismatch ## Different kinds of nodes - expected string but found
                   ## int for example

  SexpMismatch* = object
    ## Single S-expression mismatch
    path*: SexpPath ## Full path for the mismatched
    case kind*: SexpMismatchKind
      of smMissingKey:
        key*: string ## Key missing in the input data

      of smDifferentLiteral, smKindMismatch, smArrayLen, smDifferentSymbol:
        expected*, found*: SexpNode ## 'expected X' but 'found Y' error messages
        arraydiff*: tuple[target, input: seq[int]] ## For comparison of the
        ## lists keys - indices of the non-field elements.

func sdiffPart*(key: string): SexpPathPart =
  ## Create single S-expression key path part
  SexpPathPart(key: key, kind: spKey)

func sdiffPart*(index: int): SexpPathPart =
  ## Create single S-expression index part
  SexpPathPart(index: index, kind: spIndex)


func mismatch*(path: SexpPath, key: string): SexpMismatch =
  ## Create missing key mismatch
  SexpMismatch(kind: smMissingKey, key: key, path: path)

proc mismatch(
    kind: SexpMismatchKind, path: SexpPath,
    expected, found: SexpNode
  ): SexpMismatch =
  ## Create expected/found mismatch

  result = SexpMismatch(kind: kind, path: path)
  result.expected = expected
  result.found = found


proc diff*(target, input: SexpNode): seq[SexpMismatch] =
  ##[

Recursively iterate over target and input trees, find all mismatches.

Comparison rules:

- `_` in expected matches to anything
- Excess fields in `input` are discarded
- Missing fields in `target` are treated as errors
- List with keys are compared in two passes - only `:key` to `:key`
  between two lists - in unordered manner. Then all remaining elements
  are processed in the order of their appearance.
- Literals and kinds are compared directly with `==`

  ]##

  proc aux(
      target, input: SexpNode,
      path: SexpPath,
      mismatches: var seq[SexpMismatch]
    ) =

    if target.kind == SSymbol and target.getSymbol() == "_":
      # `_` matches against everything and does not produce diffs
      return

    elif target.kind != input.kind:
      mismatches.add mismatch(smKindMismatch, path, target, input)

    else:
      case target.kind:
        of SInt:
          if target.getNum() != input.getNum():
            mismatches.add mismatch(smDifferentLiteral, path, target, input)

        of SFloat:
          if target.getFNum() != input.getFNum():
            mismatches.add mismatch(smDifferentLiteral, path, target, input)

        of SString:
          if target.getStr() != input.getStr():
            mismatches.add mismatch(smDifferentLiteral, path, target, input)

        of SSymbol:
          if target.getSymbol() != input.getSymbol():
            mismatches.add mismatch(smDifferentSymbol, path, target, input)

        of SList:
          var
            inputKeys: Table[string, int]
            inputNonKeys, targetNonKeys: seq[int]

          for idx, item in pairs(input):
            if item.kind == SKeyword:
              inputKeys[item.getKey()] = idx

            else:
              inputNonKeys.add idx

          for idx, item in pairs(target):
            if item.kind == SKeyword:
              let key = item.getKey()
              if key in inputKeys:
                aux(
                  item,
                  input[inputKeys[key]],
                  path & sdiffPart(key), mismatches)

              else:
                mismatches.add mismatch(path, key)

            else:
              targetNonKeys.add idx

          if inputNonKeys.len != targetNonKeys.len:
            var mis =  mismatch(smArrayLen, path, target, input)
            mis.arraydiff = (targetNonKeys, inputNonKeys)
            mismatches.add mis

          for idx in 0 ..< min(inputNonKeys.len, targetNonKeys.len):
            aux(
              target[targetNonKeys[idx]],
              input[inputNonKeys[idx]],
              path & sdiffPart(inputNonKeys[idx]),
              mismatches
            )

        of SCons:
          aux(target.car, input.car, path & sdiffPart(0), mismatches)
          aux(target.cdr, input.cdr, path & sdiffPart(1), mismatches)

        of SNil:
          discard

        of SKeyword:
          aux(target.value, input.value, path, mismatches)


  aux(target, input, @[], result)

func formatPath(path: SexpPath): string =
  ## Format S-expression path
  if path.len == 0:
    result = "<root>"

  else:
    for part in path:
      case part.kind:
        of spIndex:
          result.add "[" & $part.index & "]"

        of spKey:
          result.add ":" & part.key

proc describeDiff*(diff: seq[SexpMismatch], conf: DiffFormatConf): ColText =
  ## Generate colortext description of the S-expression mismatch diff
  coloredResult()

  for idx, mismatch in diff:
    if 0 < idx:
      add "\n"

    add formatPath(mismatch.path) + fgYellow
    case mismatch.kind:
      of smKindMismatch:
        addf(
          "expected kind '$#', but got '$#'",
          $mismatch.expected.kind + fgGreen,
          $mismatch.found.kind + fgRed
        )

      of smMissingKey:
        add " misses key ", mismatch.key + fgRed

      of smDifferentLiteral, smDifferentSymbol:
        let exp = $mismatch.expected
        let got = $mismatch.found
        addf(" expected $#, but got $#", exp + fgGreen, got + fgRed)
        if '\n' notin exp and '\n' notin got:
          addf(" ($#)", formatInlineDiff(exp, got, conf))

      of smArrayLen:
        addf(
          " len mismatch. Expected $# elements, but got $#",
          $mismatch.expected.len + fgGreen,
          $mismatch.found.len + fgRed
        )

proc toLine*(s: SexpNode, sortfield: bool = false): ColText =
  ## Generate colored formatting of the S-expression.
  ##
  ## - `sortfield` - order SKeyword entries in lists by the key name
  coloredResult()

  let dim = styleDim
  proc aux(s: SexpNode) =
    if s.isNil: return
    case s.kind:
      of SInt:    add $s.getNum() + fgCyan
      of SString: add ("\"" & s.getStr() & "\"") + fgYellow
      of SFloat:  add $s.getFNum() + fgMagenta
      of SNil:    add "nil"
      of SSymbol: add s.getSymbol() + fgCyan
      of SCons:
        add "(" + dim
        aux(s.car)
        add " . " + dim
        aux(s.cdr)
        add ")" + dim
      of SKeyword:
        add ":" + fgBlue
        add s.getKey() + fgBlue
        add " "
        aux(s.value)

      of SList:
        add "(" + dim
        var first = true
        if sortfield:
          var fieldIdx: seq[(int, string)]
          for idx, item in pairs(s):
            if item.kind == SKeyword:
              fieldIdx.add (idx, item.getKey())

          let sortedFields = fieldIdx.sortedByIt(it[1])
          var nameIdx = 0
          for item in items(s):
            if not first: add " "
            if item.kind == SKeyword:
              aux(s[sortedFields[nameIdx][0]])
              inc nameIdx

            else:
              aux(item)

            first = false

        else:
          for item in items(s):
            if not first: add " "
            first = false
            aux(item)

        add ")" + dim

  aux(s)


# when isMainModule:
#   let s = @[
#     "(:a b :c d)",
#     "(:c d :a b)"
#   ]

#   for item in s:
#     echo item.parseSexp().toLine(sortfield = true)

# when isMainModule and false:
#   for str in @[
#     ("1", "2"),
#     ("(:line 12 :col 10)", "(:line 30 :col 30)"),
#     ("(Kind :expr 12)", "(Kind :expr 39)"),
#     ("(Kind :expr 12)", "(Kind)"),
#     ("(SymA :expr 12)", "(SymB :expr 12)")
#   ]:
#     let diff = sdiff(str[0], str[1])
#     if diff.isSome():
#       echo "```diff"
#       echo "- ", str[0]
#       echo "+ ", str[1]
#       echo diff.get()
#       echo "```\n"
