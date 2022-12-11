import
  std/[
    strutils,
    terminal,
    options,
    tables,
    algorithm,
    pegs,
    intsets,
    os
  ],
  experimental/[
    sexp,
    sexp_diff,
    colortext,
    colordiff
  ]

import lib/stdtest/testutils

type
  GivenInlineError* = object
    ## Inline error provided in the test as inline comment
    kind*: string ## Error message kind (report kind)
    msg*: string ## Error message string
    line*, col*: int ## Line and column position of the inline message

  CompileDiagnostic* = object
    ## Single instance of the diagnostic issued at compile-time
    inline*: Option[GivenInlineError] ## Expected inline error
    node*: SexpNode ## S-expression node of the diagnostic
    file*: string ## File diagnostic occured in

  CompileSexpCompare* = ref object
    ## Result of comparing two data outputs for a given spec
    match*: bool ## Whether match was successfull or failed
    expectedReports*: seq[CompileDiagnostic] ## c-time diagnostics expected
    ## by the test.
    givenReports*: seq[CompileDiagnostic] ## c-time reports that were
    ## actually recorded.
    sortedMapping*: seq[tuple[pair: (int, int), cost: int]]
    diffMap*: Table[(int, int), seq[SexpMismatch]]
    ignoredExpected*: seq[int]
    ignoredGiven*: seq[int]
    cantIgnoreGiven*: bool

  CompileTextCompare* = object
    match*: bool
    excessiveMessages*: seq[GivenInlineError] ## Errors that weren't
    ## expected but occurred during the compilation.
    missingMessages*: seq[GivenInlineError] ## Inline errors that were
    ## expected in the specification but weren't found in the actual
    ## compiler output.

  CompileOutputCheck* = object
    ## Input parameters for S-expression diff
    enforceFullMatch* {.requiresInit.}: bool
    inlineErrors* {.requiresInit.}: seq[GivenInlineError] ## List of
                                                          ## expected
                                                          ## inline errors
    testName* {.requiresInit.}: string ## Name of the test
    expectedNimout* {.requiresInit.}: string ## Expected compiler output,
                            ## added together with inline error conversion
                            ## results.
    givenNimout* {.requiresInit.}: string ## Compiler output given
    expectedFile* {.requiresInit.}: string ## File name expected in the
                                           ## inline errors




proc trimUnitSep(x: var string) =
  let L = x.len
  if L > 0 and x[^1] == '\31':
    setLen x, L-1

proc diffStrings*(
    a, b: string, useColors: bool = true
  ): tuple[output: string, same: bool] =
  let
    a = a.split("\n")
    b = b.split("\n")

  var
    maxA = 0
    maxB = 0

  for line in a:
    maxA = max(maxA, line.len)

  for line in b:
    maxB = max(maxB, line.len)

  var conf = diffFormatter()
  conf.sideBySide = maxA + maxB + 8 < terminalWidth()
  conf.groupLine = true

  let diff = myersDiff(a, b)
  if len(diff) == 0:
    result.same = true

  else:
    result.same = false
    result.output = diff.shiftDiffed(a, b).
      formatDiffed(a, b, conf).toString(useColors)

proc format*(tcmp: CompileSexpCompare): ColText =
  ## Pretty-print structured output comparison for further printing.
  var conf = diffFormatter()

  coloredResult()

  var first = true
  proc addl() =
    if not first:
      add "\n"

    first = false

  for (pair, weight) in tcmp.sortedMapping:
    if 0 < weight:
      addl()
      addl()
      let exp = tcmp.expectedReports[pair[0]]
      add "Expected"
      if exp.inline.isSome():
        let inline = exp.inline.get()
        addf(" inline $# annotation at $#($#, $#)",
          inline.kind + fgGreen,
          exp.file + fgYellow,
          $inline.line + fgCyan,
          $inline.col + fgCyan
        )

      addf(":\n\n- $#\n\nGiven:\n\n+ $#\n\n",
        exp.node.toLine(sortfield = true),
        tcmp.givenReports[pair[1]].node.toLine(sortfield = true)
      )

      add tcmp.diffMap[pair].describeDiff(conf).indent(2)


  for exp in tcmp.ignoredExpected:
    addl()
    addl()
    addf(
      "Missing expected annotation:\n\n? $#\n\n",
      tcmp.expectedReports[exp].node.toLine(sortfield = true)
    )

  if tcmp.cantIgnoreGiven:
    for give in tcmp.ignoredGiven:
      addl()
      addl()
      addf(
        "Unexpected given annotation:\n\n? $#\n\n",
        tcmp.expectedReports[give].node.toLine(sortfield = true)
      )

proc sexpCheck*(data: CompileOutputCheck): CompileSexpCompare =
  ## Check if expected nimout values match with specified ones. Thish check
  ## implements a structured comparison of the data and returns full report
  ## about all the mismatches that can be formatted as needed.
  ## This procedure determines whether `given` spec matches `expected` test
  ## results.
  var r = CompileSexpCompare(
    cantIgnoreGiven: data.enforceFullMatch
  )
  # r.cantIgnoreGiven =  expected.nimoutFull

  for exp in data.inlineErrors:
    var parsed = parseSexp(exp.msg)
    var loc = convertSexp([sexp(data.testName), sexp(exp.line)])
    if exp.col > 0:
      loc.add sexp(exp.col)

    parsed.addField("location", loc)
    parsed.addField("severity", newSSymbol(exp.kind))
    r.expectedReports.add CompileDiagnostic(
      inline: some exp, node: parsed, file: data.expectedFile)

  for line in splitLines(data.expectedNimout):
    if 0 < line.len:
      r.expectedReports.add CompileDiagnostic(node: parseSexp(line))

  for line in splitLines(data.givenNimout):
    if 0 < line.len:
      r.givenReports.add CompileDiagnostic(node: parseSexp(line))

  proc reportCmp(a, b: int): int =
    # Best place for further optimization and configuration - if more
    # comparison speed is needed, try starting with error kind, file, line
    # comparison, then doing a regular msg != msg compare and only then
    # deep structural diff.
    if r.expectedReports[a].node[0] != r.givenReports[b].node[0]:
      result += 10

    let diff = diff(r.expectedReports[a].node, r.givenReports[b].node)
    r.diffMap[(a, b)] = diff
    result += diff.len

  (r.ignoredExpected, r.ignoredGiven, r.sortedMapping) = stableMatch(
    r.expectedReports.len,
    r.givenReports.len,
    reportCmp,
    Descending
  )

  if 0 < r.sortedMapping[0].cost:
    r.match = false
  elif 0 < r.ignoredGiven.len and data.enforceFullMatch:
    r.match = false
  else:
    r.match = true

  return r

proc checkForInlineErrors*(data: CompileOutputCheck): CompileTextCompare =
  ## Check for inline error annotations in the nimout results, comparing
  ## them with the output of the compiler.
  let pegLine = peg"{[^(]*} '(' {\d+} ', ' {\d+} ') ' {[^:]*} ':' \s* {.*}"
  var covered = initIntSet()
  for line in splitLines(data.givenNimout):
    # Iterate over each line in the output

    # Searching for the `file(line, col) Severity: text` pattern
    if line =~ pegLine:
      let
        file = extractFilename(matches[0])
        line = try: parseInt(matches[1]) except: -1
        col  = try: parseInt(matches[2]) except: -1
        kind = matches[3]
        msg  = matches[4]

      if file == extractFilename data.testName:
        # If annotation comes from the target file
        var isCovered: bool = false
        for idx, expected in data.inlineErrors:
          if expected.line == line and
             (expected.col == col or expected.col < 0) and
             expected.kind == kind and
             expected.msg in msg:
            # And annotaiton has matching line, column and message
            # information, register it as 'covered'
            covered.incl idx
            isCovered = true
            # WARNING original implementation considered all matching
            # inline errors as covered, which means if the same error was
            # printed twice in the same location it was an "ok". Direct
            # `break` fixes this (feature? bug?). IF the test suite doesn't
            # fail after this change THEN TODO: clean up this comment.
            break

        if not isCovered:
          result.excessiveMessages.add GivenInlineError(
            line: line, col: col, msg: msg, kind: kind)

  for idx, expected in data.inlineErrors:
    # For each output message that was not covered by annotations, add it
    # to the output as 'missing'
    if idx notin covered:
      result.missingMessages.add(expected)

  result.match = (
    result.missingMessages.len() == 0
  ) and (
    not data.enforceFullMatch or result.excessiveMessages.len() == 0
  )

proc compileOutputCheck*(full: bool, expected, given: string): bool =
  ## Check if expected nimout values match with specified ones. This check
  ## implements comparison of the unstructured data.
  result = true
  if full:
    if expected != given:
      result = false
  elif 0 < expected.len() and
       # NOTE this function should be made more generic and moved into the
       # diff library, it might have some pretty interesting use-cases in
       # places where determinization of the output gives UX improvements.
       not greedyOrderedSubsetLines(expected, given):
    result = false
