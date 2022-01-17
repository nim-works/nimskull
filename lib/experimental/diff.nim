#
#
#            Nim's Runtime Library
#        (c) Copyright 2018 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements an algorithm to compute the
## `diff`:idx: between two sequences of lines.
##
## - To learn more see `Diff on Wikipedia. <http://wikipedia.org/wiki/Diff>`_

runnableExamples:
  assert diffInt(
    [0, 1, 2, 3, 4, 5, 6, 7, 8],
    [-1, 1, 2, 3, 4, 5, 666, 7, 42]) ==
    @[Item(startA: 0, startB: 0, deletedA: 1, insertedB: 1),
      Item(startA: 6, startB: 6, deletedA: 1, insertedB: 1),
      Item(startA: 8, startB: 8, deletedA: 1, insertedB: 1)]

runnableExamples:
  # 2 samples of text (from "The Call of Cthulhu" by Lovecraft)
  let txt0 = """
abc
def ghi
jkl2"""
  let txt1 = """
bacx
abc
def ghi
jkl"""
  assert diffText(txt0, txt1) ==
    @[Item(startA: 0, startB: 0, deletedA: 0, insertedB: 1),
      Item(startA: 2, startB: 3, deletedA: 1, insertedB: 1)]

# code owner: Arne Döring
#
# This is based on C# code written by Matthias Hertel, http://www.mathertel.de
#
# This Class implements the Difference Algorithm published in
# "An O(ND) Difference Algorithm and its Variations" by Eugene Myers
# Algorithmica Vol. 1 No. 2, 1986, p 251.

import std/[tables, strutils, sequtils, algorithm]

type
  Item* = object    ## An Item in the list of differences.
    startA*: int    ## Start Line number in Data A.
    startB*: int    ## Start Line number in Data B.
    deletedA*: int  ## Number of changes in Data A.
    insertedB*: int ## Number of changes in Data B.

  DiffData = object ## Data on one input file being compared.
    data: seq[int] ## Buffer of numbers that will be compared.
    modified: seq[bool] ## Array of booleans that flag for modified
                        ## data. This is the result of the diff.
                        ## This means deletedA in the first Data or
                        ## inserted in the second Data.

  Smsrd = object
    x, y: int

# template to avoid a seq copy. Required until `sink` parameters are ready.
template newDiffData(initData: seq[int]; L: int): DiffData =
  DiffData(
    data: initData,
    modified: newSeq[bool](L + 2)
  )

proc len(d: DiffData): int {.inline.} = d.data.len

proc diffCodes(aText: string; h: var Table[string, int]): DiffData =
  ## This function converts all textlines of the text into unique numbers for every unique textline
  ## so further work can work only with simple numbers.
  ## `aText` the input text
  ## `h` This extern initialized hashtable is used for storing all ever used textlines.
  ## `trimSpace` ignore leading and trailing space characters
  ## Returns a array of integers.
  var lastUsedCode = h.len
  result.data = newSeq[int]()
  for s in aText.splitLines:
    if h.contains s:
      result.data.add h[s]
    else:
      inc lastUsedCode
      h[s] = lastUsedCode
      result.data.add lastUsedCode
  result.modified = newSeq[bool](result.data.len + 2)

proc optimize(data: var DiffData) =
  ## If a sequence of modified lines starts with a line that contains the same content
  ## as the line that appends the changes, the difference sequence is modified so that the
  ## appended line and not the starting line is marked as modified.
  ## This leads to more readable diff sequences when comparing text files.
  var startPos = 0
  while startPos < data.len:
    while startPos < data.len and not data.modified[startPos]:
      inc startPos
    var endPos = startPos
    while endPos < data.len and data.modified[endPos]:
      inc endPos

    if endPos < data.len and data.data[startPos] == data.data[endPos]:
      data.modified[startPos] = false
      data.modified[endPos] = true
    else:
      startPos = endPos

proc sms(dataA: var DiffData; lowerA, upperA: int; dataB: DiffData; lowerB, upperB: int;
         downVector, upVector: var openArray[int]): Smsrd =
  ## This is the algorithm to find the Shortest Middle Snake (sms).
  ## `dataA` sequence A
  ## `lowerA` lower bound of the actual range in dataA
  ## `upperA` upper bound of the actual range in dataA (exclusive)
  ## `dataB` sequence B
  ## `lowerB` lower bound of the actual range in dataB
  ## `upperB` upper bound of the actual range in dataB (exclusive)
  ## `downVector` a vector for the (0,0) to (x,y) search. Passed as a parameter for speed reasons.
  ## `upVector` a vector for the (u,v) to (N,M) search. Passed as a parameter for speed reasons.
  ## Returns a MiddleSnakeData record containing x,y and u,v.

  let max = dataA.len + dataB.len + 1

  let downK = lowerA - lowerB # the k-line to start the forward search
  let upK = upperA - upperB # the k-line to start the reverse search

  let delta = (upperA - lowerA) - (upperB - lowerB)
  let oddDelta = (delta and 1) != 0

  # The vectors in the publication accepts negative indexes. the vectors implemented here are 0-based
  # and are access using a specific offset: upOffset upVector and downOffset for downVector
  let downOffset = max - downK
  let upOffset = max - upK

  let maxD = ((upperA - lowerA + upperB - lowerB) div 2) + 1

  downVector[downOffset + downK + 1] = lowerA
  upVector[upOffset + upK - 1] = upperA

  for D in 0 .. maxD:
    # Extend the forward path.
    for k in countup(downK - D, downK + D, 2):
      # find the only or better starting point
      var x: int
      if k == downK - D:
        x = downVector[downOffset + k + 1] # down
      else:
        x = downVector[downOffset + k - 1] + 1 # a step to the right
        if k < downK + D and downVector[downOffset + k + 1] >= x:
          x = downVector[downOffset + k + 1] # down

      var y = x - k

      # find the end of the furthest reaching forward D-path in diagonal k.
      while x < upperA and y < upperB and dataA.data[x] == dataB.data[y]:
        inc x
        inc y

      downVector[downOffset + k] = x

      # overlap ?
      if oddDelta and upK - D < k and k < upK + D:
        if upVector[upOffset + k] <= downVector[downOffset + k]:
          return Smsrd(x: downVector[downOffset + k],
                       y: downVector[downOffset + k] - k)

    # Extend the reverse path.
    for k in countup(upK - D, upK + D, 2):
      # find the only or better starting point
      var x: int
      if k == upK + D:
        x = upVector[upOffset + k - 1] # up
      else:
        x = upVector[upOffset + k + 1] - 1 # left
        if k > upK - D and upVector[upOffset + k - 1] < x:
          x = upVector[upOffset + k - 1] # up

      var y = x - k
      while x > lowerA and y > lowerB and dataA.data[x - 1] == dataB.data[y - 1]:
        dec x
        dec y

      upVector[upOffset + k] = x

      # overlap ?
      if not oddDelta and downK-D <= k and k <= downK+D:
        if upVector[upOffset + k] <= downVector[downOffset + k]:
          return Smsrd(x: downVector[downOffset + k],
                       y: downVector[downOffset + k] - k)

  assert false, "the algorithm should never come here."

proc lcs(dataA: var DiffData; lowerA, upperA: int; dataB: var DiffData; lowerB, upperB: int;
         downVector, upVector: var openArray[int]) =
  ## This is the divide-and-conquer implementation of the longes common-subsequence (lcs)
  ## algorithm.
  ## The published algorithm passes recursively parts of the A and B sequences.
  ## To avoid copying these arrays the lower and upper bounds are passed while the sequences stay constant.
  ## `dataA` sequence A
  ## `lowerA` lower bound of the actual range in dataA
  ## `upperA` upper bound of the actual range in dataA (exclusive)
  ## `dataB` sequence B
  ## `lowerB` lower bound of the actual range in dataB
  ## `upperB` upper bound of the actual range in dataB (exclusive)
  ## `downVector` a vector for the (0,0) to (x,y) search. Passed as a parameter for speed reasons.
  ## `upVector` a vector for the (u,v) to (N,M) search. Passed as a parameter for speed reasons.

  # make mutable copy:
  var lowerA = lowerA
  var lowerB = lowerB
  var upperA = upperA
  var upperB = upperB

  # Fast walkthrough equal lines at the start
  while lowerA < upperA and lowerB < upperB and dataA.data[lowerA] == dataB.data[lowerB]:
    inc lowerA
    inc lowerB

  # Fast walkthrough equal lines at the end
  while lowerA < upperA and lowerB < upperB and dataA.data[upperA - 1] == dataB.data[upperB - 1]:
    dec upperA
    dec upperB

  if lowerA == upperA:
    # mark as inserted lines.
    while lowerB < upperB:
      dataB.modified[lowerB] = true
      inc lowerB

  elif lowerB == upperB:
    # mark as deleted lines.
    while lowerA < upperA:
      dataA.modified[lowerA] = true
      inc lowerA

  else:
    # Find the middle snake and length of an optimal path for A and B
    let smsrd = sms(dataA, lowerA, upperA, dataB, lowerB, upperB, downVector, upVector)
    # Debug.Write(2, "MiddleSnakeData", String.Format("{0},{1}", smsrd.x, smsrd.y))

    # The path is from LowerX to (x,y) and (x,y) to UpperX
    lcs(dataA, lowerA, smsrd.x, dataB, lowerB, smsrd.y, downVector, upVector)
    lcs(dataA, smsrd.x, upperA, dataB, smsrd.y, upperB, downVector, upVector)  # 2002.09.20: no need for 2 points

proc createDiffs(dataA, dataB: DiffData): seq[Item] =
  ## Scan the tables of which lines are inserted and deleted,
  ## producing an edit script in forward order.
  var startA = 0
  var startB = 0
  var lineA = 0
  var lineB = 0
  while lineA < dataA.len or lineB < dataB.len:
    if lineA < dataA.len and not dataA.modified[lineA] and
       lineB < dataB.len and not dataB.modified[lineB]:
      # equal lines
      inc lineA
      inc lineB
    else:
      # maybe deleted and/or inserted lines
      startA = lineA
      startB = lineB

      while lineA < dataA.len and (lineB >= dataB.len or dataA.modified[lineA]):
        inc lineA

      while lineB < dataB.len and (lineA >= dataA.len or dataB.modified[lineB]):
        inc lineB

      if (startA < lineA) or (startB < lineB):
        result.add Item(startA: startA,
                        startB: startB,
                        deletedA: lineA - startA,
                        insertedB: lineB - startB)


proc diffInt*(arrayA, arrayB: openArray[int]): seq[Item] =
  ## Find the difference in 2 arrays of integers.
  ##
  ## `arrayA` A-version of the numbers (usually the old one)
  ##
  ## `arrayB` B-version of the numbers (usually the new one)
  ##
  ## Returns a sequence of Items that describe the differences.

  # The A-Version of the data (original data) to be compared.
  var dataA = newDiffData(@arrayA, arrayA.len)

  # The B-Version of the data (modified data) to be compared.
  var dataB = newDiffData(@arrayB, arrayB.len)

  let max = dataA.len + dataB.len + 1
  # vector for the (0,0) to (x,y) search
  var downVector = newSeq[int](2 * max + 2)
  # vector for the (u,v) to (N,M) search
  var upVector = newSeq[int](2 * max + 2)

  lcs(dataA, 0, dataA.len, dataB, 0, dataB.len, downVector, upVector)
  result = createDiffs(dataA, dataB)

proc diffText*(textA, textB: string): seq[Item] =
  ## Find the difference in 2 text documents, comparing by textlines.
  ##
  ## The algorithm itself is comparing 2 arrays of numbers so when comparing 2 text documents
  ## each line is converted into a (hash) number. This hash-value is computed by storing all
  ## textlines into a common hashtable so i can find duplicates in there, and generating a
  ## new number each time a new textline is inserted.
  ##
  ## `textA` A-version of the text (usually the old one)
  ##
  ## `textB` B-version of the text (usually the new one)
  ##
  ## Returns a seq of Items that describe the differences.
  # See also `gitutils.diffStrings`.
  # prepare the input-text and convert to comparable numbers.
  var h = initTable[string, int]()  # TextA.len + TextB.len  <- probably wrong initial size
  # The A-Version of the data (original data) to be compared.
  var dataA = diffCodes(textA, h)

  # The B-Version of the data (modified data) to be compared.
  var dataB = diffCodes(textB, h)

  h.clear # free up hashtable memory (maybe)

  let max = dataA.len + dataB.len + 1
  # vector for the (0,0) to (x,y) search
  var downVector = newSeq[int](2 * max + 2)
  # vector for the (u,v) to (N,M) search
  var upVector = newSeq[int](2 * max + 2)

  lcs(dataA, 0, dataA.len, dataB, 0, dataB.len, downVector, upVector)

  optimize(dataA)
  optimize(dataB)
  result = createDiffs(dataA, dataB)


type
  SeqEditKind* = enum
    ## Kind of the sequence edit operation
    sekNone ## Empty edit operation
    sekKeep ## Keep original element unchanged
    sekInsert ## Insert new element into target sequence
    sekReplace ## Replace source element with the target
    sekDelete ## Delete element from the source sequence
    sekTranspose ## Transpose two elements

  SeqEdit* = object
    ## Sequence edit operation.
    kind*: SeqEditKind ## Sequence edit operation kind
    sourcePos*: int ## Position in the original sequence
    targetPos*: int ## Position in the target sequence

proc levenshteinDistance*[T](
    str1, str2: openarray[T]
  ): tuple[distance: int, operations: seq[SeqEdit]] =
  ## Compute edit distance between two item sequences, return list of edit
  ## operations necessary to transform `str1` into `str2`
  ##
  ## Adapted from https://phiresky.github.io/levenshtein-demo/
  var
    l1 = str1.len
    l2 = str2.len

    m: seq[seq[int]] = newSeqWith(l1 + 1, newSeqWith(l2 + 1, 0))
    paths: seq[seq[(int, int)]] = newSeqWith(l1 + 1,
                                             newSeqWith(l2 + 1, (0, 0)))

  for i in 0 .. l1:
    m[i][0] = i
    paths[i][0] = (i - 1, 0)

  for j in 0 .. l2:
    m[0][j] = j
    paths[0][j] = (0, j - 1)

  for i in 1 .. l1:
    for j in 1 .. l2:
      if (str1[i - 1] == str2[j - 1]):
        m[i][j] = m[i - 1][j - 1]
        paths[i][j] = (i - 1, j - 1)
      else:
        let min = min([m[i - 1][j], m[i][j - 1], m[i - 1][j - 1]])
        m[i][j] = min + 1;
        if (m[i - 1][j] == min):
          paths[i][j] = (i - 1, j)

        elif (m[i][j - 1] == min):
          paths[i][j] = (i, j - 1)

        elif (m[i - 1][j - 1] == min):
          paths[i][j] = (i - 1, j - 1)

  var levenpath: seq[tuple[i, j: int, t: SeqEditKind]]

  var j = l2
  var i = l1
  while i >= 0 and j >= 0:
    j = l2
    while i >= 0 and j >= 0:
      levenpath.add((i, j, sekNone))
      let t = i
      i = paths[i][j][0]
      j = paths[t][j][1]


  reverse(levenpath)
  result.distance = m[levenpath[^1][0]][levenpath[^1][1]]

  for i in 1 ..< levenpath.len:
    var
      last = levenpath[i - 1]
      cur = levenpath[i]

    if i != 0:
      if (
        cur.i == last.i + 1 and
        cur.j == last.j + 1 and
        m[cur.i][cur.j] != m[last.i][last.j]
      ):
        result.operations.add SeqEdit(kind: sekReplace)

      elif (cur.i == last.i and cur.j == last.j + 1):
        result.operations.add SeqEdit(kind: sekInsert)

      elif (cur.i == last.i + 1 and cur.j == last.j):
        result.operations.add SeqEdit(kind: sekDelete)

      else:
        result.operations.add SeqEdit(kind: sekKeep)

      result.operations[^1].sourcePos = cur.i - 1
      result.operations[^1].targetPos = cur.j - 1


type
  ShiftedDiff* = object
    ## Intermediate version of the diffed sequence
    oldShifted*: seq[tuple[kind: SeqEditKind, item: int]]
    newShifted*: seq[tuple[kind: SeqEditKind, item: int]]


proc myersDiff*[T](
    aSeq, bSeq: openarray[T], itemCmp: proc(x, y: T): bool): seq[SeqEdit] =
  ## Generate series of sequence edit operations necessary to trasnform
  ## `aSeq` into `bSeq`. For item equality comparison use `itemCmp`
  ##
  ## https://gist.github.com/adamnew123456/37923cf53f51d6b9af32a539cdfa7cc4
  var front: Table[int, tuple[x: int, history: seq[SeqEdit]]]
  front[1] = (0, @[])

  template one(idx: int): int = idx - 1

  let
    aMax = len(aSeq)
    bMax = len(bSeq)

  for d in countup(0, aMax + bMax + 1):
    for k in countup(-d, d + 1, 2):
      let goDown =
        (k == -d or (k != d and front[k - 1].x < front[k + 1].x))


      var (x, history) =
        if goDown:
          (front[k + 1].x, front[k + 1].history)

        else:
          (front[k - 1].x + 1, front[k - 1].history)

      var y = x - k

      if 1 <= y and y <= bMax and goDown:
        history.add SeqEdit(kind: sekInsert, targetPos: one(y))

      elif 1 <= x and x <= aMax:
        history.add SeqEdit(kind: sekDelete, sourcePos: one(x))

      while x < aMax and
            y < bMax and
            itemCmp(aSeq[x], bSeq[y]):

        x += 1
        y += 1
        history.add SeqEdit(kind: sekKeep, sourcePos: one(x), targetPos: one(y))

      if x >= aMax and y >= bMax:
        return history

      else:
        front[k] = (x, history)

proc shiftDiffed*[T](
    diff: seq[SeqEdit], oldSeq, newSeq: openarray[T]): ShiftedDiff =

  for line in items(diff):
    case line.kind:
      of sekReplace:
        result.oldShifted.add((sekReplace, line.sourcePos))

      of sekNone:
        assert false, "Input diff sequence should not contain empty operations"

      of sekTranspose:
        assert false, "Input diff sequence should not contain transpose operations"

      of sekDelete:
        result.oldShifted.add((sekDelete, line.sourcePos))

      of sekInsert:
        result.newShifted.add((sekInsert, line.targetPos))

      of sekKeep:
        var
          oldLen = result.oldShifted.len
          newLen = result.newShifted.len

        if oldLen < newLen:
          while oldLen < newLen:
            result.oldShifted.add((sekNone, 0))
            inc oldLen

        elif newLen < oldLen:
          while newLen < oldLen:
            result.newShifted.add((sekNone, 0))
            inc newLen

        result.oldShifted.add((sekKeep, line.sourcePos))
        result.newShifted.add((sekKeep, line.targetPos))


proc formatDiffed*(
    shifted: ShiftedDiff,
    oldSeq, newSeq: seq[string],
    sideBySide: bool,
    showLineNumbers: bool = false
  ): string =

  ##[

Plaintext format diff edit script for printing. Provides pretty barebones
implementation of the formatting - no coloring, diff formatting is not
configurable.

Generated diff formatting does not contain trailing newline

- `sideBySide`: stack modified lines on top of each other
  (unified diff) or side-by-side (split diff)
- `oldSeq`, `newSeq`: original diffed sequences with items
  converted to strings. It is assumed that each item is placed
  on one line and diffed between each other.
- `showLineNumbers`: Show original line (sequence) numbers
  in the generated string printout.


]##

  var
    # Diffed sequence of items
    oldText, newText: seq[tuple[
      text: string, # formatted line
      changed: bool # Whether line has changed. Used in unified diff
                    # formatting to avoid duplicate string printing.
    ]]

  let maxLhsIdx = len($shifted.oldShifted[^1].item)
  let maxRhsIdx = len($shifted.newShifted[^1].item)

  proc editFmt(fmt: SeqEditKind, idx: int, isLhs: bool): string =
    if showLineNumbers:
      let num =
        if fmt == sekNone:
          align(" ", maxLhsIdx)

        elif isLhs:
          align($idx, maxLhsIdx)

        else:
          align($idx, maxRhsIdx)

      case fmt:
        of sekDelete: "- " & num
        of sekInsert: "+ " & num
        of sekKeep: "~ " & num
        of sekNone: "? " & num
        of sekReplace: "-+" & num
        of sekTranspose: "^v" & num

    else:
      case fmt:
        of sekDelete: "- "
        of sekInsert: "+ "
        of sekReplace: "-+"
        of sekKeep: "~ "
        of sekTranspose: "^v"
        of sekNone: (if isLhs: "? " else: "?")


  var lhsMax = 0

  # Iterate over shifted diff sequence, construct formatted list of lines
  # that will be joined to final output.
  for (lhs, rhs) in zip(shifted.oldShifted, shifted.newShifted):
    oldText.add((editFmt(lhs.kind, lhs.item, true), true))

    newText.add((
      editFmt(rhs.kind, rhs.item, false),
      # Only newly inserted lines need to be formatted for the unified
      # diff, everything else is displayed on the 'original' version.
      not sideBySide and rhs.kind in {sekInsert}
    ))

    if lhs.kind == sekDelete and rhs.kind == sekInsert:
      oldText[^1].text.add oldSeq[lhs.item]
      newText[^1].text.add newSeq[rhs.item]

    elif rhs.kind == sekInsert:
      newText[^1].text.add newSeq[rhs.item]

    elif lhs.kind == sekDelete:
      oldText[^1].text.add oldSeq[lhs.item]

    else:
      oldText[^1].text.add oldSeq[lhs.item]
      newText[^1].text.add newSeq[rhs.item]

    lhsMax = max(oldText[^1].text.len, lhsMax)

  var first = true
  for (lhs, rhs) in zip(oldtext, newtext):
    if not first:
      # Avoid trailing newline of the diff formatting.
      result.add "\n"
    first = false

    if sideBySide:
      result.add alignLeft(lhs.text, lhsMax + 3)
      result.add rhs.text

    else:
      result.add lhs.text
      if rhs.changed:
        result.add "\n"
        result.add rhs.text


proc myersDiff*[T](aSeq, bSeq: openarray[T]): seq[SeqEdit] =
  ## Diff overload without explicit comparator proc - use default `==` for
  ## two items.
  myersDiff(aSeq, bSeq, proc(a, b: T): bool = a == b)

proc diffText(
    text1, text2: seq[string],
    sideBySide: bool,
    showLineNumbers: bool = false
  ): string =
  ## Format diff of two text lines using default `formatDiffed` implementation
  myersDiff(text1, text2).
    shiftDiffed(text1, text2).
    formatDiffed(text1, text2, sideBySide, showLineNumbers = showLineNumbers)

proc diffText*(
    text1, text2: string,
    sideBySide: bool,
    showLineNumbers: bool = false
  ): string =
  ## Format diff of two text blocks via newline split and default
  ## `formatDiffed` implementation
  diffText(
    text1.split("\n"),
    text2.split("\n"),
    sideBySide = sideBySide,
    showLineNumbers = showLineNumbers)
