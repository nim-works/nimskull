## ..include:: text_layouter.rst

# Thanks to language being syntactically close to python this is mostly
# just blatant copy-paste of python code with added type annotations.

import std/[
  strutils,
  sequtils,
  macros,
  tables,
  strformat,
  lenientops,
  options,
  hashes,
  math,
  sugar,
  intsets,
  bitops
]

import dod_helpers


const infty = 1024 * 1024 * 1024 # "Very large value"

func inf(a: int): bool =
  ## Check if value is 'very large'
  (infty - 4096 <= a) and (a <= infty + 4096)

func get[T](inseq: seq[Option[T]]): seq[T] =
  for elem in inseq:
    if elem.isSome():
      result.add elem.get()

iterator zip*[T1, T2, T3, T4, T5](
    s1: seq[T1],
    s2: seq[T2],
    s3: seq[T3],
    s4: seq[T4],
    s5: seq[T5]
  ): tuple[v1: T1, v2: T2, v3: T3, v4: T4, v5: T5] =

  for idx in 0 ..< min([s1.len, s2.len, s3.len, s4.len, s5.len]):
    yield (s1[idx], s2[idx], s3[idx], s4[idx], s5[idx])

iterator rmpairs*[T](s: var seq[T]): (int, var T) =
  ## Iterate over mutable sequence starting from the right
  for idx in countdown(s.len - 1, 0):
    yield (idx, s[idx])


#*************************************************************************#
#****************************  Format policy  ****************************#
#*************************************************************************#
#===========================  Type definition  ===========================#

declareIdType(LytStr)
declareHighMasking(LytStr, highMaskRange = 8)

type
  ## These are types for formatting text. Text formatting doesn't have a
  ## perfect solution that works in all cases and we have to resort to
  ## heuristics to choose the most approriate text layout on a case by case
  ## basis.
  ##
  ## Text is initially described via a DSL as a set of blocks (`LytBlock`).
  ## When it's time to output the text a layout (`LytLayout`) determines
  ## the exact strings (`LytStr`) to output. Text layout requires
  ## heuristics to choose between various options we use a solver
  ## ('LytSolution') which is used to determine the precise layout choosen
  ## for a given block.
  LayoutElementKind = enum
    lekString
    lekNewline
    lekNewlineSpace
    lekLayoutPrint

  LayoutElement = ref object
    ## An element of a layout object - a directive to the console.
    ##
    ## This object sports a collection of static methods, each of which
    ## returns an anonymous function invoking a method of the console to
    ## which it is applied.
    ##
    ## Refer to the corresponding methods of the LytConsole object for
    ## descriptions of the methods involved.
    id {.requiresInit.}: int
    case kind: LayoutElementKind:
      of lekString:
        text {.requiresInit.}: LytStrSpan ## Layout element text

      of lekNewlineSpace:
        spaceNum: int

      of lekLayoutPrint:
        layout: Layout

      of lekNewline:
        discard

    indent: bool

  LytEventKind* = enum
    layEvStr
    layEvNewline
    layEvSpaces

  LytEvent* = object
    case kind*: LytEventKind
      of layEvStr:
        str*: LytStr

      of layEvSpaces:
        spaces*: int

      of layEvNewline:
        discard


  Layout* = ref object
    ## An object containing a sequence of directives to the console.
    elements: seq[LayoutElement]

  LytSolution = ref object
    ## A Solution object effectively maps an integer (the left margin at
    ## which the solution is placed) to a layout notionally optimal for
    ## that margin, together with cost information used to evaluate the
    ## layout. For compactness, the map takes the form of a
    ## piecewise-linear cost function, with associated layouts.
    ##
    ## This object corresponds to a cost function in the main article.
    ## Layout is constructed from this function.

    id {.requiresInit.}: int

    knots: seq[int] ## a list of ints, specifying the margin settings at
    ## which the layout changes. Note that the first knot is required to be
    ## 0.
    spans: seq[int] ## a list of ints, giving for each knot, the width of
    ## the corresponding layout in characters.
    intercepts: seq[float] ## constant cost associated with each knot -
    ## computed cost of the outputting this layout solution at each know in
    ## the `knots` list
    gradients: seq[float] ## at each knot, the rate with which the layout
    ## cost increases with an additional margin indent of 1 character.
    layouts: seq[Layout] ## the Layout objects expressing the optimal
    ## layout between each knot.
    index: int

  LytBlockKind* = enum
    bkText ## A layout consisting of a single line of unbroken text.
    bkLine ## Horizontally stacked lines
    bkChoice ## Several alternating layouts
    bkStack ## Vertically stacked layouts
    bkWrap ## Mulitple blocks wrapped to create lowerst-cost layout
    bkVerb ## Multiple lines verbatim
    bkEmpty ## Empty layout block - ignored by `add` etc.

  LytStr* = object
    ## Single layout string object. It contains all the information
    ## required to perform the layout and refer back to the original string
    ## piece if needed.
    id*: LytStrId ## Id of the original piece of text
    len*: int ## It's lengh in units (units are specified - can be ASCII or
              ## unicode or anything else)

  LytStrSpan* = object
    ## Span of multiple layout strings
    strs: seq[LytStr]
    len: int

  LytBlock* = ref object
    layoutCache: Table[Option[LytSolution], Option[LytSolution]]
    isBreaking* {.requiresInit.}: bool ## Whether or not this block should end the line
    breakMult* {.requiresInit.}: int ## Local line break cost change

    id {.requiresInit.}: int

    case kind*: LytBlockKind
      of bkVerb:
        textLines*: seq[LytStrSpan] ## Multiple lines of text
        firstNl*: bool ## Insert newline at the block start

      of bkText:
        text*: LytStrSpan ## A single line of text, free of carriage
        ## returs etc.

      of bkWrap:
        prefix*: Option[LytStr]
        sep*: LytStr ## Separator for block wraps
        wrapElements*: seq[LytBlock]

      of bkStack, bkChoice, bkLine:
        elements*: seq[LytBlock]

      of bkEmpty:
        discard

  LytFormatPolicy = object
    breakElementLines: proc(
      blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] ## Hook

  LytOptions* = object
    leftMargin*: int ## position of the first right margin. Expected `0`
    rightMargin*: int ## position of the second right margin. Set for `80`
                      ## to wrap on default column limit.
    leftMarginCost*: float ## cost (per character) beyond margin 0.
                           ## Expected value `~0.05`
    rightMarginCost*: float ## cost (per character) beyond margin 1. Should
                            ## be much higher than `c0`. Expected value
                            ## `~100`
    linebreakCost*: int ## cost per line-break
    indentSpaces*: int ## spaces per indent
    cpack*: float ## cost (per element) for packing justified layouts.
                 ## Expected value `~0.001`
    formatPolicy*: LytFormatPolicy

  OutConsole* = object
    leftMargin: int
    rightMargin: int
    hPos: int ## Horizontal position on the output console
    margins: seq[int]


# Special magic to handle user-provided indentation blocks of text without
# having to add new strings to the system.
const LytSpacesId* = LytStrId(high(int) - 120)
const EmptyLytStr* = LytStr(id: LytStrId(0), len: 0)

func `$`(s: LytStr): string =
  if s.id == LytSpacesId:
    &"{s.len} spaces"

  else:
    $s.id


func isSpaces*(s: LytStr): bool = s.id == LytSpacesId
func isEmpty*(s: LytStr): bool = s.id.int == 0


func lytStrSpaces(spaces: int): LytStr =
  LytStr(id: LytSpacesId, len: spaces)

func lytStrIdx*(idx: int, len: int): LytStr =
  ## Create layout string object from the element index
  LytStr(id: toLytStrId(idx), len: len)


func lytStrSpan*(str: LytStr): LytStrSpan =
  ## Construct layout string span with single element
  result.strs = @[str]
  result.len = str.len

func lytStrSpan*(strs: openArray[LytStr]): LytStrSpan =
  ## Construct layout string span from multiple elements
  result.strs = @strs
  for str in strs:
    result.len += str.len

func margin(buf: OutConsole): int =
  buf.margins[^1]

func addMargin(c: var OutConsole, m: int) =
  c.margins.add m

func popMargin(c: var OutConsole) =
  discard c.margins.pop

func event(s: LytStr): LytEvent =
  LytEvent(kind: layEvStr, str: s)

func event(spaces: int): LytEvent =
  LytEvent(kind: layEvSpaces, spaces: spaces)

func event(): LytEvent = LytEvent(kind: layEvNewline)

func format(str: LytStr, getStr: proc(s: LytStr): string): string =
  if getStr.isNil():
    result.add $str

  else:
    result.add getStr(str)

func format(span: LytStrSpan, getStr: proc(s: LytStr): string): string =
  result.add "["
  for idx, item in span.strs:
    if 0 < idx: result.add ", "
    result.add format(item, getStr)

  result.add "]"

func treeRepr*(
    self: Layout,
    getStr: proc(s: LytStr): string = nil,
    level: int = 0,
  ): string =

  var r = addr result
  func add(s: string) = r[].add s

  func aux(lyt: Layout, l: int)
  func aux(lyt: LayoutElement, l: int) =
    add repeat("  ", l)
    add &"id {lyt.id} "
    case lyt.kind:
      of lekString:
        add "[text] 《"
        add lyt.text.format(getStr)
        add "》\n"

      of lekNewline:
        add "[newline]\n"

      of lekNewlineSpace:
        add "[newline][space]\n"

      of lekLayoutPrint:
        add "[lyt]\n"
        aux(lyt.layout, l + 1)

  func aux(lyt: Layout, l: int) =
    for idx, elem in pairs(lyt.elements):
      if not idx == 0:
        add "\n"
      aux(elem, l)

  aux(self, level)

func treeRepr*(
    self: LytSolution,
    getStr: proc(s: LytStr): string = nil,
    level: int = 0
  ): string =

  result.add "[lyt solution]\n"
  for lyt in self.layouts:
    result.add "  [lyt]\n"
    result.add treeRepr(lyt, getStr, 2)
    result.add "\n"

func `$`*(le: LayoutElement): string = $le.text

proc `$`*(sln: LytSolution): string =
  result &= "<"
  var idx: int = 0
  for s in zip(
    sln.knots, sln.spans, sln.intercepts, sln.gradients, sln.layouts
  ):
    if idx > 0:
      result &= ", "

    result &= &"{s[0]}/({s[1]}, {s[2]:.2}, {s[3]})"
    inc idx

  result &= ">"

proc `$`*(sln: Option[LytSolution]): string =
  if sln.isSome(): return $sln.get()

proc `$`*(blc: LytBlock): string =
  result &= (
    case blc.kind:
      of bkText:
        "T[" & (if blc.isBreaking: "*" else: "") & &"\"{blc.text}\"]"

      of bkStack:
        "V[" & blc.elements.mapIt($it).join(" ↕ ") & "]"

      of bkLine:
        "H[" & blc.elements.mapIt($it).join(" ↔ ") & "]"

      of bkChoice:
        "(" & blc.elements.mapIt($it).join(" ? ") & ")"

      of bkWrap:
        "[" & blc.wrapElements.mapIt($it).join(" ") & "]"

      of bkVerb:
        $blc.textLines[0].strs[0].id.int & "..."

      of bkEmpty:
        "<empty>"
  )
      # &""">>{blc.textLines.join("⮒")}<<"""




proc treeRepr*(
    inBl: LytBlock,
    getStr: proc(str: LytStr): string = nil
  ): string =

  proc aux(bl: LytBlock, level: int): string =
    let name =
      case bl.kind:
        of bkLine: "L"
        of bkChoice: "C"
        of bkText: "T"
        of bkWrap: "W"
        of bkStack: "S"
        of bkVerb: "V"
        of bkEmpty: "E"

    var pref = align(name & " ", level * 2)

    if bl.isBreaking:
      pref.add &"brk: {bl.isBreaking} "

    if bl.breakMult != 1:
      pref.add &"mul: {bl.breakMult} "

    let pref2 = repeat(" ", level * 2)

    result = pref2 & pref

    case bl.kind:
      of bkLine, bkChoice, bkStack, bkWrap:
        result &= "\n"
        for elem in items(
          if bl.kind == bkWrap: bl.wrapElements else: bl.elements
        ):

          result &= elem.aux(level + 1)

      of bkText:
        result &= "〈" & bl.text.format(getStr) & "〉\n"

      of bkEmpty:
        result &= "<empty>"

      of bkVerb:
        result &= "\n"
        for line in items(bl.textLines):
          result &= pref2 & repeat("  ", clamp(
            level - 1, 0, high(int))) & "  〚" & line.format(getStr) & "〛\n"

  return aux(inBl, 0)





#*************************************************************************#
#************************  LytOptions configuration  ************************#
#*************************************************************************#


func hash(elem: LayoutElement): Hash = hash(elem.id)
func hash(lyt: Layout): Hash = hash(lyt.elements)

func hash(sln: Option[LytSolution]): Hash =
  if sln.isNone():
    return
  else:
    return sln.get.id

#*************************************************************************#
#*******************************  Layout  ********************************#
#*************************************************************************#

func getSId(): int =
  var slnId {.global.}: int
  {.cast(noSideEffect).}:
    inc slnId
    result = slnId

func lytString(s: LytStrSpan): LayoutElement =
  LayoutElement(text: s, kind: lekString, id: getSId())

func lytNewline(indent: bool = true): LayoutElement =
  LayoutElement(indent: indent, kind: lekNewline, id: getSId())

func lytNewlineSpace(n: int): LayoutElement =
  LayoutElement(spaceNum: n, kind: lekNewlineSpace, id: getSId())

proc lytPrint(lyt: Layout): LayoutElement =
  LayoutElement(kind: lekLayoutPrint, layout: lyt, id: getSId())

proc getStacked(layouts: seq[Layout]): Layout =
  ## Return the vertical composition of a sequence of layouts.

  ## Args:
  ##   layouts: a sequence of Layout objects.
  ## Returns:
  ##   A new Layout, stacking the arguments.
  var lElts: seq[LayoutElement]
  for l in layouts:
    for e in l.elements:
      lElts.add e

    lElts.add lytNewline()

  return Layout(elements: lElts[0 .. ^2])  # Drop the last NewLine()

func initLayout(elems: seq[LayoutElement]): Layout =
  Layout(elements: elems)

#*************************************************************************#
#******************************  LytSolution  *******************************#
#*************************************************************************#

proc initSolution(
    knots: seq[int], spans: seq[int], intercepts: seq[float],
    gradients: seq[float], layouts: seq[Layout]): LytSolution =
  result = LytSolution(
    knots: knots, spans: spans, intercepts: intercepts,
    gradients: gradients, layouts: layouts, id: getSId())


#===========================  Helper methods  ============================#
func reset(self: var LytSolution) =
  ## Begin iteration.
  self.index = 0

func advance(self: var LytSolution) =
  ## Advance to the next knot.
  self.index += 1

func retreat(self: var LytSolution) =
  ## Move back a knot.
  self.index -= 1

func curKnot(self: LytSolution): int =
  ## The currently indexed knot.
  return self.knots[self.index]

func curSpan(self: LytSolution): int =
  return self.spans[self.index]

func curIntercept(self: LytSolution): float =
  return self.intercepts[self.index]

func curGradient(self: LytSolution): float =
  return self.gradients[self.index]

func curLayout(self: LytSolution): Layout = self.layouts[self.index]
func curIndex(self: LytSolution): int = self.index

func curValueAt(self: LytSolution, margin: int): float =
  ## The value (cost) extrapolated for margin m from the current knot.
  # Since a LytSolution's cost is represented by a piecewise linear function,
  # the extrapolation in this case is linear, from the current knot.
  return self.curIntercept() + self.curGradient() * float(
    margin - self.curKnot())

func nextKnot(self: LytSolution): int =
  ## The knot after the once currently indexed.
  if self.index + 1 >= self.knots.len:
    infty
  else:
    self.knots[self.index + 1]

proc moveToMargin(self: var LytSolution, margin: int) =
  ## Adjust the index so m falls between the current knot and the next.
  if self.curKnot() > margin:
    while self.curKnot() > margin:
      self.retreat()
  else:
    while self.nextKnot() <= margin and self.nextKnot() != infty:
      self.advance()
      # info "Advancing to position", self.curIndex(),
      #   "next knot is", self.nextKnot(), "margin is", margin,
      #   self.nextKnot() <= margin


#==========================  LytSolution factory  ===========================#

proc add(
    self: var LytSolution,
    knot, span: int,
    intercept, gradient: float,
    layout: Layout
  ) =
  ## Add a segment to a LytSolution under construction.
  ##
  ## The function performs basic consistency checks, and eliminates
  ## redundant segments that are linear extrapolations of those that
  ## precede them.

  if self.isNil():
    new(self)

  if 0 < self.knots.len:
    # Don't add a knot if the new segment is a linear extrapolation of
    # the last.
    let
      kLast = self.knots[^1]
      sLast = self.spans[^1]
      iLast = self.intercepts[^1]
      gLast = self.gradients[^1]

    if (span == sLast and gradient == gLast and
        iLast + (knot - kLast) * gLast == intercept):
      return

  if knot < 0 or span < 0 or intercept < 0 or gradient < 0:
    raiseAssert(
      "Internal error: bad layout: " &
        &"(k {knot}, s {span}, i {intercept}, g {gradient})")

  self.knots.add knot
  self.spans.add span
  self.intercepts.add intercept
  self.gradients.add gradient
  self.layouts.add layout


#=====================  LytSolution manipulation logic  =====================#


proc minSolution(solutions: seq[LytSolution]): Option[LytSolution] =
  ## Form the piecewise minimum of a sequence of LytSolutions.
  ##
  ## Args:
  ##   solutions: a non-empty sequence of LytSolution objects
  ## Returns:
  ##   values LytSolution object whose cost is the piecewise minimum of the LytSolutions
  ##   provided, and which associates the minimum-cost layout with each piece.
  # debug "Minimal solution out of #", solutions.len
  if len(solutions) == 1:
    return some(solutions[0])

  var
    factory: LytSolution
    solutions = solutions

  new(factory)

  for s in mitems(solutions):
    s.reset()

  let n = len(solutions)
  var
    kL = 0
    lastIMinSoln = -1  # Index of the last minimum solution
    lastIndex = -1  # Index of the current knot in the last minimum
    # solution Move through the intervals [kL, kH] defined by the
    # glb of the partitions defined by each of the solutions.

  while kL < infty:
    let
      kH = min(solutions.map(nextKnot)) - 1
      gradients = solutions.map(curGradient)

    while true:
      let values = solutions.mapIt(it.curValueAt(kL))
      # Use the index of the corresponding solution to break ties.
      let (minValue, minGradient, iMinSoln) =
        (0 ..< n).mapIt((values[it], gradients[it], it)).min()

      let minSoln = solutions[iMinSoln]
      if iMinSoln != lastIMinSoln or minSoln.curIndex() != lastIndex:
        # Add another piece to the new LytSolution
        factory.add(
          kL,
          minSoln.curSpan(),
          minValue,
          minGradient,
          minSoln.curLayout()
        )

        lastIMinSoln = iMinSoln
        lastIndex = minSoln.curIndex()
      # It's possible that within the current interval, the minimum
      # solution may change, should a solution with a lower initial
      # value but greater gradient surpass the value of one with a
      # higher initial value but lesser gradient. In such instances,
      # we need to add an extra piece to the new solution.
      let distancesToCross = collect(newSeq):
        for i in 0 ..< n:
          if gradients[i] < minGradient:
            ceil((values[i] - minValue) / (minGradient - gradients[i]))

      # Compute positions of all crossovers in [kL, kH]
      let crossovers = collect(newSeq):
        for d in distancesToCross:
          if kL + d <= kH:
            kL + d

      if crossovers.len > 0:  # Proceed to crossover in [kL, kH]
        kL = min(crossovers).int # XXXX
      else:  # Proceed to next piece
        kL = kH + 1
        if kL < infty:
          for s in mitems(solutions):
            s.moveToMargin(kL)
        break

  return some factory

proc vSumSolution(solutions: seq[LytSolution]): LytSolution =
  ## The layout that results from stacking several LytSolutions vertically.
  ## Args:
  ##   solutions: a non-empty sequence of LytSolution objects
  ## Returns:
  ##   A LytSolution object that lays out the solutions vertically, separated by
  ##   newlines, with the same left margin.


  assert solutions.len > 0

  if len(solutions) == 1:
    return solutions[0]

  var solutions = solutions # XXXX
  for s in mitems(solutions):
    s.reset()

  var margin = 0  # Margin for all components
  while true:
    result.add(
      margin,
      solutions[^1].curSpan(),
      solutions.mapIt(it.curValueAt(margin)).sum(),
      solutions.mapIt(it.curGradient()).sum(),
      getStacked(solutions.mapIt(it.curLayout()))
    )

    # The distance to the closest next knot from the current margin.
    let dStar = min(
      solutions.
      filterIt(it.nextKnot() > margin).
      mapIt(it.nextKnot() - margin))  # TODO(pyelland): Redundant check?

    if dStar.inf:
      break

    margin += dStar

    for s in mitems(solutions):
      s.moveToMargin(margin)


proc hPlusSolution(s1, s2: var LytSolution, opts: LytOptions): LytSolution =
  ## The LytSolution that results from joining two LytSolutions side-by-side.
  ##
  ## Args:
  ##   `s1`: LytSolution object
  ##   `s2`: LytSolution object
  ## Returns:
  ##   A new LytSolution reflecting a layout in which `s2` ('s layout) is
  ##   placed immediately to the right of `s1`.
  ##
  ## The resulting LytSolution object maps each prospective left margin m
  ## to the span, cost and layout information that would result from
  ## siting LytSolution `s1` at m, and then placing `s2` at margin `m +
  ## sp1(m)`, where `sp1(m)` is the span of characters occupied by the
  ## layout to which `s1` maps m. In general, of course, both s1 and
  ## `s2`'s layouts may occupy multiple lines, in which case `s2`'s
  ## layout begins at the end of the last line of `s1`'s layout---the
  ## span in this case is the span of `s1`'s last line.
  s1.reset()
  s2.reset()
  var
    s1Margin = 0
    s2Margin = s1.curSpan()

  s2.moveToMargin(s2Margin)

  while true:
    # When forming the composite cost gradient and intercept, we must
    # eliminate the over-counting of the last line of the s1, which is
    # attributable to its projection beyond the margins.
    let
      g1 = s1.curGradient()
      g2 = s2.curGradient()
      overhang0 = s2Margin - opts.leftMargin  # s2Margin = rightMargin + span of s1
      overhang1 = s2Margin - opts.rightMargin  # s2Margin = rightMargin + span of s1
      gCur = (
        g1 +
        g2 -
        (if overhang0 >= 0: opts.leftMarginCost else: 0) -
        (if overhang1 >= 0: opts.rightMarginCost else: 0)
      )

      iCur = (
        s1.curValueAt(s1Margin) +
        s2.curValueAt(s2Margin) -
        opts.leftMarginCost * max(overhang0, 0) -
        opts.rightMarginCost * max(overhang1, 0)
      )

    # The Layout computed by the following implicitly sets the margin
    # for s2 at the end of the last line printed for s1.
    result.add(
      s1Margin, s1.curSpan() + s2.curSpan(), iCur, gCur,
      initLayout(@[
        lytPrint(s1.curLayout()),
        lytPrint(s2.curLayout())
    ]))

    # Move to the knot closest to the margin of the corresponding
    # component.
    let
      kn1 = s1.nextKnot()
      kn2 = s2.nextKnot()

    if kn1.inf and kn2.inf:
      break

    # Note in the following that one of kn1 or kn2 may be infinite.
    if kn1 - s1Margin <= kn2 - s2Margin:
      s1.advance()
      s1Margin = kn1
      s2Margin = s1Margin + s1.curSpan()
      # Note that s1.CurSpan() may have changed, and s2Margin may
      # decrease, so we cannot simply increment s2's index.
      s2.moveToMargin(s2Margin)
    else:
      s2.advance()
      s2Margin = kn2
      s1Margin = s2Margin - s1.curSpan()




proc plusConst(self: LytSolution, val: float): LytSolution =
  ## Add a constant to all values of this LytSolution.
  result = self
  for a in mitems(result.intercepts):
    a += val

proc withRestOfLine(
    self: var Option[LytSolution],
    rest: var Option[LytSolution], opts: LytOptions
  ): Option[LytSolution] =
  ## Return a LytSolution that joins the rest of the line right of this one.
  ##
  ## Args:
  ##   rest: a LytSolution object representing the code laid out on the
  ##     remainder of the line, or None, if the rest of the line is empty.
  ## Returns:
  ##   A new LytSolution object juxtaposing the layout represented by this
  ##   LytSolution to the immediate right of the remainder of the line.
  if rest.isNone():
    self
  else:
    some self.get().hPlusSolution(rest.get(), opts)


#*************************************************************************#
#*****************************  LytBlock type  ******************************#
#*************************************************************************#
proc elements(self: LytBlock): seq[LytBlock] =
  if contains({bkWrap}, self.kind):
    return self.elements
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    return self.elements
  raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

proc `elements=`(self: var LytBlock; it: seq[LytBlock]) =
  var matched: bool = false
  if contains({bkWrap}, self.kind):
    if true:
      matched = true
      self.wrapElements = it
  if contains({bkStack, bkChoice, bkLine}, self.kind):
    if true:
      matched = true
      self.elements = it
  if not matched:
    raiseAssert("#[ IMPLEMENT:ERRMSG ]#")

func len*(blc: LytBlock): int =
  case blc.kind:
    of bkWrap:
      blc.wrapElements.len()

    of bkStack, bkChoice, bkLine:
      blc.elements.len()

    else:
      0


func `[]`*(blc: LytBlock, idx: int): LytBlock =
  blc.elements[idx]

func `[]`*(blc: var LytBlock, idx: int): var LytBlock =
  blc.elements[idx]

iterator items*(blc: LytBlock): LytBlock =
  for item in blc.elements:
    yield item

iterator pairs*(blc: LytBlock): (int, LytBlock) =
  for idx, item in blc.elements:
    yield (idx, item)

iterator mitems*(blc: var LytBlock): var LytBlock =
  for item in mitems(blc.elements):
    yield item

iterator mpairs*(blc: var LytBlock): (int, var LytBlock) =
  for idx, item in mpairs(blc.elements):
    yield (idx, item)

#============================  Constructors  =============================#

func getBId(): int =
  var id {.global.}: int
  {.cast(noSideEffect).}:
    inc id
    return id

proc initBlock*(kind: LytBlockKind, breakMult: int = 1): LytBlock =
  assert kind notin {bkText}

  result = LytBlock(
    id: getBId(),
    kind: kind, breakMult: breakMult, isBreaking: false)

  if kind == bkVerb:
    result.isBreaking = true

func initEmptyBlock*(): LytBlock =
  LytBlock(
    id: getBId(), kind: bkEmpty, breakMult: 1, isBreaking: false)

func filterEmpty*(blocks: openArray[LytBlock]): seq[LytBlock] =
  for bl in blocks:
    if bl.kind != bkEmpty:
      result.add bl

proc initTextBlock*(
    text: LytStrSpan,
    breakMult: int = 1,
    breaking: bool = false
  ): LytBlock =

  assert not breaking
  result = LytBlock(
    kind: bkText,
    text: text,
    isBreaking: breaking,
    id: getBId(),
    breakMult: breakMult,
  )

proc initTextBlock*(
    text: LytStr, breakMult: int = 1, breaking: bool = false
  ): LytBlock =

  initTextBlock(lytStrSpan(text), breakMult, breaking)

proc initIndentBlock*(
  blc: LytBlock, indent: int, breakMult: int = 1): LytBlock

func isEmpty*(bl: LytBlock): bool {.inline.} =
  bl.kind == bkEmpty or
  (bl.kind in {bkStack, bkLine, bkChoice} and bl.len == 0)

template findSingle*(elems: typed, targetKind: typed): untyped =
  var
    countEmpty = 0
    countFull = 0
    idx = -1

  for item in elems:
    if item.isEmpty():
      inc countEmpty

    elif (
      when targetKind is set:
        item.kind in targetKind
      else:
        item.kind == targetKind
    ):
      if idx != -1:
        idx = -1
        break

      else:
        idx = countFull

    inc countFull

  if countFull == countEmpty + 1 and idx != -1:
    idx

  else:
    -1

func max(ints: seq[int], onEmpty: int): int =
  if ints.len == 0: onEmpty else: max(ints)

func min(ints: seq[int], onEmpty: int): int =
  if ints.len == 0: onEmpty else: min(ints)

func updateSizes(bk: var LytBlock) =
  if bk.kind in { bkChoice , bkLine, bkStack } and bk.elements.len > 0:
    bk.isBreaking = bk.elements[^1].isBreaking

proc convertBlock*(bk: LytBlock, newKind: LytBlockKind): LytBlock =
  result = LytBlock(
    id: getBId(), breakMult: bk.breakMult, kind: newKind,
    isBreaking: false
  )

  result.elements = bk.elements

  updateSizes(result)


func flatten*(bl: LytBlock, kind: set[LytBlockKind]): LytBlock =
  if bl.kind in kind and
     (let idx = findSingle(bl.elements, {
    low(LytBlockKind) .. high(LytBlockKind) } - { bkEmpty }); idx != -1):
    result = bl.elements[idx]

  else:
    result = bl

proc initChoiceBlock*(
    elems: openArray[LytBlock],
    breakMult: int = 1
  ): LytBlock =

  result = LytBlock(
    id: getBId(),
    isBreaking: false,
    breakMult: breakMult,
    kind: bkChoice,
    elements: filterEmpty(elems))

  updateSizes(result)


proc initLineBlock*(
    elems: openArray[LytBlock],
    breakMult: int = 1
  ): LytBlock =

  result = LytBlock(
    id: getBId(), isBreaking: false,
    breakMult: breakMult, kind: bkLine,
    elements: filterEmpty(elems))

  updateSizes(result)

proc initIndentBlock*(
    blc: LytBlock, indent: int, breakMult: int = 1): LytBlock =

  if indent == 0:
    blc

  else:
    initLineBlock(@[initTextBlock(lytStrSpaces(indent)), blc])



proc initStackBlock*(elems: openArray[LytBlock], breakMult: int = 1): LytBlock =
  result = LytBlock(
    id: getBId(), isBreaking: false,
    breakMult: breakMult, kind: bkStack,
    elements: filterEmpty(elems))

  updateSizes(result)


proc initWrapBlock*(
    elems: openArray[LytBlock],
    sep: LytStr,
    breakMult: int = 1,
  ): LytBlock =

  LytBlock(
    isBreaking: false,
    id: getBId(),
    sep: sep,
    kind: bkWrap,
    wrapElements: toSeq(elems),
    breakMult: breakMult)

proc initVerbBlock*(
    textLines: openArray[LytStrSpan],
    breaking: bool = true,
    firstNl: bool = false,
    breakMult: int = 1
  ): LytBlock =

  assert breaking
  result = LytBlock(
    breakMult: breakMult,
    id: getBId(), kind: bkVerb,
    textLines: @textLines,
    isBreaking: breaking,
    firstNl: firstNl,
  )

  updateSizes(result)

proc add*(target: var LytBlock, other: varargs[LytBlock]) =
  for bl in other:
    if bl.kind != bkEmpty:
      target.elements.add bl

  updateSizes(target)

proc initSeparated*(
    blocks: seq[LytBlock],
    vertical: bool,
    sep: LytBlock
  ): LytBlock =
  result =
    if vertical:
      initStackBlock(@[])
    else:
      initLineBlock(@[])

  if vertical:
    for idx, item in blocks:
      if idx < len(blocks) - 1:
        result.add initLineBlock([item, sep])

      else:
        result.add item

  else:
    for idx, item in blocks:
      if idx > 0:
        result.add sep

      result.add item

proc initVSeparated*(
    blocks: seq[LytBlock], sep: LytBlock): LytBlock =
  initSeparated(blocks, true, sep)

proc initHSeparated*(
    blocks: seq[LytBlock], sep: LytBlock): LytBlock =
  initSeparated(blocks, false, sep)


#============================  Layout logic  =============================#

proc doOptLayout*(
  self: var LytBlock,
  rest: var Option[LytSolution], opts: LytOptions): Option[LytSolution]

proc optLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =
  ## Retrieve or compute the least-cost (optimum) layout for this block.
  ## - @arg{rest} :: text to the right of this block.
  ## - @ret{} :: Optimal layout for this block and the rest of the line.
  # Deeply-nested choice block may result in the same continuation
  # supplied repeatedly to the same block. Without memoisation, this
  # may result in an exponential blow-up in the layout algorithm.
  if rest notin self.layoutCache:
    self.layoutCache[rest] = self.doOptLayout(rest, opts)

  return self.layoutCache[rest]

proc doOptTextLayout(
  self: LytBlock,
  rest: var Option[LytSolution], opts: LytOptions): Option[LytSolution] =

  let
    span = self.text.len
    layout = initLayout(@[lytString(self.text)])
  # The costs associated with the layout of this block may require 1, 2 or
  # 3 knots, depending on how the length of the text compares with the two
  # margins (leftMargin and rightMargin) in opts. Note that we assume
  # opts.rightMargin >= opts.leftMargin >= 0, as asserted in
  # base.Options.Check().
  if span >= opts.rightMargin:
    result = some initSolution(
      @[0],
      @[span],
      @[float(
        (span - opts.leftMargin) * opts.leftMarginCost +
        (span - opts.rightMargin) * opts.rightMargin)],
      @[float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout]
    )

  elif span >= opts.leftMargin:
    result = some initSolution(
      @[0, opts.rightMargin - span],
      @[span, span], # XXXX
      @[float((span - opts.leftMargin) * opts.leftMarginCost),
        float((opts.rightMargin - opts.leftMargin) * opts.leftMarginCost)],
      @[float(opts.leftMarginCost), float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout, layout] # XXXX
    )
  else:
    result = some initSolution(
      @[0, opts.leftMargin - span, opts.rightMargin - span],
      @[span, span, span], # XXXX
      @[float(0), float(0), float((opts.rightMargin - opts.leftMargin) * opts.leftMarginCost)],
      @[float(0), float(opts.leftMarginCost), float(opts.leftMarginCost + opts.rightMarginCost)],
      @[layout, layout, layout] # XXXX
    )

  return result.withRestOfLine(rest, opts)


proc doOptLineLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  assert self != nil
  if self.elements.len == 0:
    return rest

  var elementLines: seq[seq[LytBlock]] = @[]
  elementLines.add @[]

  for i, elt in self.elements:
    elementLines[^1].add elt
    if i < self.elements.high() and elt.isBreaking:
      elementLines.add @[]

  if len(elementLines) > 1:
    assert opts.formatPolicy.breakElementLines != nil
    elementLines = opts.formatPolicy.breakElementLines(elementLines)

  var lineSolns: seq[LytSolution]

  for i, ln in mpairs(elementLines):
    var lnLayout =
      if i == elementLines.high:
        rest
      else:
        none(LytSolution)

    for idx, elt in rmpairs(ln):
      lnLayout = elt.optLayout(lnLayout, opts)

    if lnLayout.isSome():
      lineSolns.add lnLayout.get()

  let soln = vSumSolution(lineSolns)

  result = some soln.plusConst(
    float(opts.linebreakCost * (len(lineSolns) - 1)))


proc doOptChoiceLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =
  # The optimum layout of this block is simply the piecewise minimum of its
  # elements' layouts.
  return minSolution():
    var tmp: seq[LytSolution]
    for it in mitems(self.elements):
      let lyt = it.optLayout(rest, opts)
      if lyt.isSome():
        tmp.add lyt.get()

    tmp


proc doOptStackLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  # The optimum layout for this block arranges the elements vertically. Only
  # the final element is composed with the continuation provided---all the
  # others see an empty continuation ("None"), since they face the end of
  # a line.
  if self.elements.len == 0:
    return rest

  let soln = vSumSolution():
    get():
      collect(newSeq):
        for idx, elem in mpairs(self.elements):
          if idx < self.elements.high:
            var it = none(LytSolution)
            optLayout(elem, it, opts)
          else:
            elem.optLayout(rest, opts)


  # Under some odd circumstances involving comments, we may have a
  # degenerate solution. WARNING
  if soln.layouts.len == 0:
    return rest

  # Add the cost of the line breaks between the elements.
  return some soln.plusConst float(
    opts.linebreakCost * self.breakMult *
    max(len(self.elements) - 1, 0))


proc doOptWrapLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =
  ## Computing the optimum layout for this class of block involves
  ## finding the optimal packing of elements into lines, a problem
  ## which we address using dynamic programming.
  var sepLayout = block:
    var it = (initTextBlock(self.sep), none(LytSolution))
    it[0].optLayout(it[1], opts)

  # TODO(pyelland): Investigate why OptLayout doesn't work here.
  var prefixLayout: Option[LytSolution] =
    if self.prefix.isSome():
      var it = (initTextBlock(self.prefix.get()), none(LytSolution))
      it[0].doOptLayout(it[1], opts)
    else:
      none(LytSolution)

  var eltLayouts = block:
    var res: seq[Option[LytSolution]]
    for it in mitems(self.wrapElements):
      var tmp = none(LytSolution)
      res.add it.optLayout(tmp, opts)

    res

  # Entry i in the list wrapSolutions contains the optimum layout for the
  # last n - i elements of the block.
  var wrapSolutions: seq[Option[LytSolution]] =
    self.len.newSeqWith(none(LytSolution))

  # Note that we compute the entries for wrapSolutions in reverse
  # order, at each iteration considering all the elements from i ... n
  # - 1 (the actual number of elements considered increases by one on
  # each iteration). This means that the complete solution, with
  # elements 0 ... n - 1 is computed last.
  for i in countdown(self.len - 1, 0): # XXXX
    # To calculate wrapSolutions[i], consider breaking the last n - i
    # elements after element j, for j = i ... n - 1. By induction,
    # wrapSolutions contains the optimum layout of the elements after
    # the break, so the full layout is calculated by composing a line
    # with the elements before the break with the entry from
    # wrapSolutions corresponding to the elements after the break.
    # The optimum layout to be entered into wrapSolutions[i] is then
    # simply the minimum of the full layouts calculated for each j.
    var solutionsI: seq[LytSolution]
    # The layout of the elements before the break is built up incrementally
    # in lineLayout.
    var lineLayout: Option[LytSolution] =
      if prefixLayout.isNone():
        eltLayouts[i]

      else:
        prefixLayout.withRestOfLine(eltLayouts[i], opts)

    var breakOut = false
    var lastBreaking: bool = self.wrapElements[i].isBreaking
    for j in i ..< self.len - 1:
      # Stack solutions for two lines on each other. NOTE this part is
      # different from the reference implementation, but I think this is
      # just a minor bug on the other side.
      let fullSoln = vSumSolution(
        @[lineLayout.withRestOfLine(sepLayout, opts),
          # Solutions for the previous lines
          wrapSolutions[j + 1]].get())
      # We adjust the cost of the full solution by adding the cost of
      # the line break we've introduced, and a small penalty
      # (Options.cpack) to favor (ceteris paribus) layouts with
      # elements packed into earlier lines.
      solutionsI.add(
        plusConst(
          fullSoln,
          float(opts.linebreakCost * self.breakMult +
                opts.cpack * (self.len - j))))
      # If the element at the end of the line mandates a following
      # line break, we're done.
      if lastBreaking:
        breakOut = true
        break
      # Otherwise, add a separator and the next element to the line
      # layout and continue.
      var sepEltLayout = sepLayout.withRestOfLine(
        eltLayouts[j + 1], opts)

      lineLayout = lineLayout.withRestOfLine(sepEltLayout, opts)
      lastBreaking = self.wrapElements[j + 1].isBreaking

    if not breakOut:
      var line = lineLayout.withRestOfLine(rest, opts)
      # line = sepLayout.withRestOfLine(line, opts)
      solutionsI.add line.get()

    wrapSolutions[i] = minSolution(solutionsI)
  # Once wrapSolutions is complete, the optimum layout for the entire
  # block is the optimum layout for the last n - 0 elements.
  return wrapSolutions[0]

proc doOptVerbLayout(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =


  # The solution for this block is essentially that of a TextBlock(''), with
  # an abberant layout calculated as follows.
  var lElts: seq[LayoutElement]

  for i, ln in self.textLines:
    if i > 0 or self.firstNl:
      lElts.add lytNewline()

    lElts.add lytString(ln)

  let layout = initLayout(lElts)
  let span = 0
  var sf: LytSolution
  new(sf)
  if opts.leftMargin > 0:  # Prevent incoherent solutions
    sf.add(0, span, 0, 0, layout)
  # opts.rightMargin == 0 is absurd
  sf.add(opts.leftMargin - span, span, 0, opts.leftMarginCost, layout)
  sf.add(
    opts.rightMargin - span, span,
    (opts.rightMargin - opts.leftMargin) * opts.leftMarginCost,
    opts.leftMarginCost + opts.rightMarginCost, layout)

  result = some sf

proc doOptLayout*(
    self: var LytBlock,
    rest: var Option[LytSolution],
    opts: LytOptions
  ): Option[LytSolution] =

  case self.kind:
    of bkText:   result = self.doOptTextLayout(rest, opts)
    of bkLine:   result = self.doOptLineLayout(rest, opts)
    of bkChoice: result = self.doOptChoiceLayout(rest, opts)
    of bkStack:  result = self.doOptStackLayout(rest, opts)
    of bkWrap:   result = self.doOptWrapLayout(rest, opts)
    of bkVerb:   result = self.doOptVerbLayout(rest, opts)
    of bkEmpty:  assert false


proc initLytOptions*(): LytOptions =
  result = LytOptions(
    leftMargin: 0,
    rightMargin: 80,
    leftMarginCost: 0.05,
    rightMarginCost: 100,
    linebreakCost: 5,
    indentSpaces: 2,
    cpack: 0.001,
    formatPolicy: LytFormatPolicy(
      breakElementLines: (
        proc(blc: seq[seq[LytBlock]]): seq[seq[LytBlock]] =

          proc strippedLine(line: seq[LytBlock]): LytBlock =
            return initLineBlock(line)

          result.add @[blc[0]]
          if blc.len > 1:
            let ind = initIndentBlock(
              initStackBlock(blc[1..^1].map(strippedLine)),
              2 * 2)

            result.add @[ind])))

proc join*(
    blocks: LytBlock,
    sep: LytBlock,
    vertLines: bool = true
  ): LytBlock =
  assert blocks.kind in {bkLine, bkStack},
    "Only stack or line layouts can be joined"

  result = initBlock(blocks.kind)

  for idx, item in pairs(blocks):
    let isLast = idx == len(blocks) - 1
    if blocks.kind == bkStack and vertLines:
      if not isLast:
        result.add initLineBlock([item, sep])

      else:
        result.add item

    else:
      result.add item
      if not isLast:
        result.add sep

proc join*(
    blocks: seq[LytBlock],
    sep: LytBlock,
    direction: LytBlockKind
  ): LytBlock =

  result = initBlock(direction)
  for idx, item in pairs(blocks):
    let isLast = idx == len(blocks) - 1
    result.add item
    if not isLast:
      result.add sep

template addItBlock*(
    res: LytBlock,
    item: typed,
    expr: untyped,
    join: LytBlock
  ): untyped =

  var idx = 0
  for idx, it {.inject.} in pairs(item):
    if idx < item.high:
      if res.kind == bkStack:
        res.add initLineBlock(@[expr, join])

      else:
        res.add expr
        res.add join

    else:
      res.add expr


template joinItBlock*(
    direction: LytBlockKind,
    item: typed,
    expr: untyped,
    join: LytBlock
  ): untyped =

  var res = initBlock(direction)
  res.addItBlock(item, expr, join)
  res

template joinItLine*(
    item: typed,
    expr: untyped,
    join: LytBlock
  ): untyped =

  var res = initBlock(bkLine)
  res.addItBlock(item, expr, join)
  res

proc expectValid*(bl: LytBlock, msg: string = "") =
  let ok = not(
     (bl.kind in {bkStack, bkLine, bkChoice} and bl.elements.len == 0) or
     (bl.kind in {bkWrap} and bl.wrapElements.len == 0))
  assert(ok,
        "Invalid combinator layout block passed - no nested elements " &
        "specified, so layout is impossible. Block kind - " &
        $bl.kind & ". " & msg)

proc toLayouts*(bl: LytBlock, opts: LytOptions): seq[Layout] =
  ## Return all possible formatting layouts for a given block with provided
  ## options. The best layout will be the first in the returned sequence.
  expectValid(bl)
  case bl.kind:
    of bkStack, bkChoice, bkLine:
      assert 0 < bl.elements.len

    of bkWrap:
      assert 0 < bl.wrapElements.len

    else:
      discard

  var bl = bl
  let sln = block:
    var it = none(LytSolution)
    bl.doOptLayout(it, opts)

  assert isSome(sln), "Could not perform layout for block " & $bl

  return sln.get().layouts

proc toLayout*(bl: LytBlock, opts: LytOptions): Layout =
  ## Return first best formatting layout for a given block. This is the
  ## procedure you should be using unless you need to have access to all
  ## the possible layouts.
  toLayouts(bl, opts)[0]



iterator formatEvents*(lyt: Layout): LytEvent =
  ## Generate formatting events for the given layout. The events are
  ## backend-agnostic and can be interpreted further by the user depending
  ## on their needs.
  var buf: OutConsole
  var stack: seq[tuple[lyt: Layout, idx: int]] = @[(lyt, 0)]
  template top(): untyped = stack[^1]

  buf.addMargin buf.hPos
  while 0 < len(stack):
    while top().idx < top().lyt.elements.len:
      let elem = top().lyt.elements[top().idx]
      inc top().idx
      buf.hPos = max(buf.hPos, buf.margin())
      case elem.kind
        of lekString:
          for item in elem.text.strs:
            if item.id.isNil():
              discard

            elif item.id == LytSpacesId:
              if item.len != 0:
                yield LytEvent(kind: layEvSpaces, spaces: item.len)

              buf.hPos += item.len

            else:
              yield LytEvent(kind: layEvStr, str: item)
              buf.hPos += item.len

        of lekNewline, lekNewlineSpace:
          yield LytEvent(kind: layEvNewline)
          buf.hPos = 0
          let mar = buf.margin()
          if mar != 0:
            yield LytEvent(kind: layEvSpaces, spaces: mar)
            buf.hPos += mar

          if elem.kind == lekNewlineSpace and elem.spaceNum != 0:
            yield LytEvent(kind: layEvSpaces, spaces: elem.spaceNum)
            buf.hPos += elem.spaceNum

        of lekLayoutPrint:
          stack.add((elem.layout, 0))
          buf.addMargin buf.hPos

    discard stack.pop()
    buf.popMargin()


proc lytSpaces*(count: int = 1): LytStr =
  result.id = LytSpacesId
  result.len = count

proc add(span: var LytStrSpan, str: LytStr) =
  span.strs.add str
  span.len += str.len

proc alignLeft*(span: sink LytStrSpan, target: int): LytStrSpan =
  result = span
  if result.len < target:
    result.add lytSpaces(target - result.len)

type
  LytAlignDirection* = enum
    lAlignLeft
    lAlignRight
    lAlignCenter

proc textLen(b: LytBlock): int =
  case b.kind:
    of bkText:
      result = b.text.len

    of bkChoice:
      for item in b.elements:
        let len = textLen(item)
        result = min(result, len)

    of bkStack:
      for item in b.elements:
        let len = textLen(item)
        result = max(result, len)

    of bkLine:
      for item in b.elements:
        result += textLen(item)

    else:
      discard

proc initAlignedGrid*(
    blocks: seq[seq[LytBlock]],
    aligns: openArray[tuple[
      leftPad, rightPad: int, direction: LytAlignDirection]]
  ): LytBlock =

  for idx, row in pairs(blocks):
    assert len(row) <= len(aligns),
      "Invalid number for column alignments specified - row " &
        $idx & " has total of " & $len(row) & " cells, but only " &
        $len(aligns) & " were specified."

  var colWidths = newSeqWith(len(aligns), 0)

  for rowIdx, row in pairs(blocks):
    for colIdx, col in pairs(row):
      colWidths[colIdx] = max(textLen(col), colWidths[colIdx])

  result = initStackBlock([])
  for row in items(blocks):
    var resRow = initLineBlock([])
    for idx, col in pairs(row):
      let al = aligns[idx]
      let diff = colWidths[idx] - textLen(col)
      case al.direction:
        of lAlignLeft:
          resRow.add initLineBlock([
            lytSpaces(al.leftPad).initTextBlock(),
            col,
            lytSpaces(al.rightPad + diff).initTextBlock()])

        of lAlignRight:
          resRow.add initLineBlock([
            lytSpaces(al.leftPad + diff).initTextBlock(),
            col,
            lytSpaces(al.rightPad).initTextBlock()])

        of lAlignCenter:
          let left = diff div 2
          let right = diff - left
          resRow.add initLineBlock([
            lytSpaces(al.leftPad + left).initTextBlock(),
            col,
            lytSpaces(al.rightPad + right).initTextBlock()])



    result.add resRow


proc initAlignedGrid*(
    blocks: seq[seq[LytBlock]],
    aligns: openArray[LytAlignDirection]
  ): LytBlock =

  initAlignedGrid(blocks, mapIt(aligns, (0, 0, it)))

template initBlockFormatDSL*() {.dirty.} =
  proc lH(args: varargs[LytBlock]): LytBlock = initLineBlock(@args)
  proc lV(args: varargs[LytBlock]): LytBlock = initStackBlock(@args)
  proc lI(sp: int, b: LytBlock): LytBlock = initIndentBlock(b, sp)
  proc lC(args: varargs[LytBlock]): LytBlock = initChoiceBlock(@args)
  proc lW(args: varargs[LytBlock]): LytBlock = initWrapBlock(@args, EmptyLytStr)
  proc lW(sep: LytStr, args: varargs[LytBlock]): LytBlock = initWrapBlock(@args, sep)
  proc lE(): LytBlock = initEmptyBlock()
  proc lT(str: LytStr): LytBlock = initTextBlock(str)
  proc lT(span: LytStrSpan): LytBlock = initTextBlock(span)
  proc lS(space: int = 0): LytBlock = initTextBlock(lytSpaces(space))
  proc lT(
    strs: openArray[LytStrSpan],
    breaking: bool = true,
    firstNl: bool = false,
    breakMult: int
  ): LytBlock = initVerbBlock(strs, breaking, firstNl, breakMult)

