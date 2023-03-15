## This module implements a transformation pass that turns untrusted AST into
## untyped AST. The translation must support arbitrary AST, as well as the
## direct output of succesful semantic analysis.
##
## We can't hold onto any input node, as the user code might have stored
## references to them in some persistent storage (e.g. a
## ``var node {.compiletime.}: NimNode``). If we only strip all flags, etc.
## from the node but keep the instance, a future macro invocation could mutate
## the node without us knowing.
##
## In usage contexts (e.g. the ``x`` in ``var y = x``), symbol are attempted
## to be reused, but they're always turned into identifier (i.e. ``nkIdent``)
## in definition contexts.

# XXX: the code is a bit predicate heavy. A slightly more functional style
#      would make it more readable, but likely also less efficient, as we
#      can't use macros for rewriting and have to resort to templates (a bit
#      less efficient) or map-like procedures (very inefficient and easy to
#      accidentally use closures that capture something)

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    astalgo,
    errorhandling,
    errorreporting,
    lineinfos,
    idents,
    trees
  ],
  compiler/front/options,
  compiler/utils/[idioms],
  compiler/sem/lookups

from compiler/ast/ast import isError, nkStrKinds, nkIntKinds, pairs

from compiler/ast/reports_sem import SemReport
from compiler/ast/report_enums import ReportKind

# XXX: we only need a fraction
from compiler/sem/semdata import PContext, config, getGenSym
from compiler/modules/modulegraphs import semtabAll

type UntypedAst = object
  n: PNode
  hasError: bool ## whether there exists an error somwhere in the tree. This
                 ## bool is used to propagate this information upwards

# TODO: see if the converter can get rid of
func toUntyped(n: sink PNode): UntypedAst =
  result.hasError = n.isError
  result.n = n

func `[]=`[I](x: var UntypedAst, i: I, n: sink PNode) =
  x.hasError = x.hasError or n.isError
  x.n[i] = n

func `[]=`[I](x: var UntypedAst, i: I, n: sink UntypedAst) =
  x.hasError = x.hasError or n.hasError
  x.n[i] = n.n

func prepareFrom(n: PNode): UntypedAst =
  assert not n.isError
  result.n = ast.newNodeI(n.kind, n.info, n.len)

func newTreeI(kind: TNodeKind, info: TLineInfo, children: varargs[UntypedAst]): UntypedAst =
  result.n = ast.newNodeI(kind, info, children.len)
  for i, it in children.pairs:
    result.n[i] = it.n
    result.hasError = result.hasError or it.hasError

func newTree2(kind: TNodeKind, info: TLineInfo, children: varargs[PNode]): UntypedAst =
  result.n = ast.newNodeI(kind, info, children.len)
  for i, it in children.pairs:
    result.n[i] = it
    result.hasError = result.hasError or it.isError

func newNodeI(kind: TNodeKind, info: TLineInfo): UntypedAst =
  UntypedAst(n: ast.newNodeI(kind, info))

func newNodeI(kind: TNodeKind, info: TLineInfo, num: int): UntypedAst =
  UntypedAst(n: ast.newNodeI(kind, info, num))

func add(x: var UntypedAst, n: UntypedAst) =
  x.n.add n.n
  x.hasError = x.hasError or n.hasError

import compiler/utils/astrepr
proc get*(c: PContext, n: sink UntypedAst): PNode =
  if n.hasError:
    var r = implicitTReprConf
    r.flags.excl trfShowFullSymTypes
    r.flags.excl trfShowNodeTypes
    echo treeRepr(c.config, n.n, r)

    result = c.config.wrapError(n.n)
    localReport(c.config, result)
  else:
    #echo treeRepr(c.config, n.n, r)
    result = n.n

template guardLen(n: PNode, L: int, body: untyped): untyped =
  mixin c
  if n.len == L: body
  else: invalidAstLen(c, n, L)

template guardMinLen(n: PNode, L: int, body: untyped): untyped =
  mixin c
  if n.len >= L: body
  else: invalidAstLen(c, n, L)

iterator sliceIt[A, B](n: PNode, sl: HSlice[A, B]): (int, PNode) =
  let last = when B is BackwardsIndex: n.len - sl.b.int
             else: sl.b.int
  for i in sl.a..last:
    yield (i, n[i])

proc newIdentNode(id: PIdent, info: TLineInfo): UntypedAst =
  result.n = ast.newNodeI(nkIdent, info)
  result.n.ident = id

proc newSymNode(sym: PSym, info: TLineInfo): UntypedAst =
  result.n = ast.newNodeI(nkSym, info)
  result.n.sym = sym

proc unreachableSymError(c: ConfigRef, s: PSym, info: TLineInfo): UntypedAst =
  # TODO: use a dedicated report kind
  result.n = c.newError(ast.newNodeI(nkEmpty, info),
                        SemReport(kind: rsemUnknownIdentifier, sym: s))
  result.hasError = true

proc isReachable(c: PContext, s: PSym): bool =
  # speed up the check by rejecting easy-to-detect not-in-scope symbols
  # early on
  if s.kind in skAllVars and sfGlobal notin s.flags and
     s.itemId.module != c.module.itemId.module:
    # the symbol is not a global and not part of the current module -> it's
    # definitely not reachable
    return false

  # search all local scopes as well as the current module's top-level one
  for scope in allScopes(c.currentScope):
    let it = strTableGet(scope.symbols, s.name)
    if it != nil and it.id == s.id: # check if it's really the same symbol
      # it is. The symbol is in scope, so it's safe to use it
      return true

  # the symbol is not part of neither local scopes nor the top-level
  # scope. If it's not a global, it's unreachable
  if s.kind in skAllVars and sfGlobal notin s.flags:
    return false

  # check if the symbol is present in the hidden interface of the module it
  # is owned by. Note that since generic instantiations are not added to
  # the hidden interface, they'll never be treated as "in scope" here
  for it in semtabAll(c.graph, getModule(s)).data:
    if it != nil and it.id == s.id:
      # found it! the symbol is reachable
      return true

  result = false

proc desym(c: PContext, s: PSym, info: TLineInfo): UntypedAst =
  ## Turns a symbol into either an identifier or a symbol node depending on
  ## whether the latter is safe to do.
  ## There are two aspects here: symbol visibility and symbol scope. Macros
  ## are allowed to forego visibility rules (e.g. by inserting a symbol that
  ## is not visible from its insertion position), but scoping rules must not be
  ## violated, as violations would result in invalid programs. For example, a
  ## symbol of a local defined inside a procedure must not be used outside
  ## of said procedure.
  ## If using `s` in the current scope would be a scoping violation, an
  ## identifier is used

  # IDEA: the new node kind ``nkSymSuggestion`` (or similar) could be
  #       introduced. It communicates to sem that the symbol must be checked
  #       for reachability (i.e. that the defining scope is reachable from the
  #       usage site) first. This would allow to move the scope check out of the
  #       sanitizer, in which case it no longer needs access to an ``PContext``
  #       instance

  # always turn gensyms into identifiers
  if sfGenSym in s.flags:
    # XXX: we can't distinguish between gensyms from outside the macro (e.g.
    #      passed via typed AST), and gensym created via ``genSym`` inside the
    #      macro. For those created inside the macro, we need to create unique
    #      identifiers, but we do not for the others.
    #      This means symbols coming from input typed AST are turned into
    #      identifiers that match no symbol
    result = newIdentNode(c.cache.getIdent(s.name.s & "`gensym" & $s.id), info)
  else:
    if isReachable(c, s):
      result = newNodeI(nkSym, info)
      result.n.sym = s
    else:
      # instead of reporting an error, we let sem figure out what to do. It's
      # also not always an error if the symbol is not reachable (e.g. for
      # generic instantiations)
      result = newIdentNode(s.name, info)

proc invalidAst(c: PContext, wrongAst: PNode, instLoc: InstantiationInfo): UntypedAst =
  result.n = c.config.newError(wrongAst, SemReport(kind: rsemIllformedAst, ast: wrongAst), instLoc)
  result.hasError = true

template invalidAst(c: PContext, wrongAst: PNode): UntypedAst =
  invalidAst(c, wrongAst, instLoc())

proc invalidAstLen(c: PContext, n: PNode, expected: int): UntypedAst =
  result = toUntyped c.config.newError(n,
    SemReport(kind: rsemIllformedAst, ast: n,
              str: "Expected $1 elements, but found $2" % [$expected, $n.len]))

proc ident(c: PContext, id: string): UntypedAst {.inline.} =
  # XXX: ``getIdent`` should accept an ``openArray[char]``
  toUntyped ast.newIdentNode(c.cache.getIdent(id), unknownLineInfo)


proc safeIdent(c: PContext, n: PNode): UntypedAst =
  ## Macros can manually create ``nkIdent`` nodes that stores no identifier
  ## and we have to guard against that here
  assert n.kind == nkIdent
  if n.ident != nil:
    result = newIdentNode(n.ident, n.info)
  else:
    result = invalidAst(c, n)

proc safeSymToIdent(c: PContext, n: PNode): UntypedAst =
  assert n.kind == nkSym
  if n.sym != nil:
    if sfGenSym in n.sym.flags:
      result = newIdentNode(c.cache.getIdent(n.sym.name.s & "`gensym" & $n.sym.id), n.info)
    else:
      result = newIdentNode(n.sym.name, n.info)
  else:
    result = invalidAst(c, n)

proc parseTypeNode(c: PContext, n: PNode): UntypedAst =
  unreachable("missing")

proc parseProcExpr(c: PContext, isExpr: bool, n: PNode): UntypedAst =
  unreachable("missing")

proc quotedIdent(c: PContext, n: PNode): UntypedAst =
  case n.kind
  of nkSym:
    if n.sym != nil:
      newIdentNode(n.sym.name, n.info)
    else:
      invalidAst(c, n)
  of nkIdent:
    safeIdent(c, n)
  of nkOpenSymChoice, nkClosedSymChoice:
    if n[0].kind == nkSym and n[0].sym != nil:
      newIdentNode(n[0].sym.name, n[0].info)
    else:
      invalidAst(c, n)
  else:
    invalidAst(c, n)

proc parseAccQuoted(c: PContext, n: PNode): UntypedAst =
  # we're doing a bit more than sanitizing here: to make processing for
  # semantic analysis easier, we evaluate identifier construction
  # expressions
  case n.len
  of 0: result = invalidAst(c, n)
  of 1:
    result = newTreeI(nkAccQuoted, n.info, quotedIdent(c, n[0]))
  else:
    result = newNodeI(nkAccQuoted, n.info)

    proc appendError(n: var UntypedAst, i: int, orig: PNode, e: UntypedAst) =
      # append all original nodes that we've processed since last adding an
      # error:
      for j in n.n.len..<i:
        n.n.add orig[j]

      n.add e

    var str = ""
    for i, it in n.pairs:
      var part = ""
      case it.kind
      of nkIdent:
        if it.ident != nil:
          part = it.ident.s
      of nkSym:
        if it.sym != nil:
          part = it.sym.name.s
      of nkIntKinds:
        part = $n.intVal
      else:
        part = ""

      if part.len == 0:
        # TODO: use the "identifier expected" error
        appendError(result, i, n, invalidAst(c, it))
      else:
        str.add part

    if result.hasError:
      # add the valid nodes coming after the last error
      for i in result.n.len..<n.len:
        result.n.add n[i]
    else:
      result.add newIdentNode(c.cache.getIdent(str), n.info)

proc parseIdent(c: PContext, n: PNode): UntypedAst =
  ## Tries to parse the node `n` as an unqualified identifier. Returns an
  ## ``nkError`` on failure
  # QUESTION: should certain symbols be rejected (even if we're only using
  #           their ident) depending on the context, e.g. an ``skProc`` symbol
  #           in a ``nkExprEqExpr`` parameter context?
  case n.kind
  of nkIdent:     safeIdent(c, n)
  of nkAccQuoted: parseAccQuoted(c, n)
  of nkSym:       safeSymToIdent(c, n)
  else:           invalidAst(c, n)

proc parseDef(c: PContext, n: PNode): UntypedAst =
  ## Parses `n` as a name-like entity in a definition context
  case n.kind
  of nkIdent:     safeIdent(c, n)
  of nkAccQuoted: parseAccQuoted(c, n)
  of nkSym:       safeSymToIdent(c, n)
  else:           invalidAst(c, n)

proc process*(c: PContext, n: PNode): UntypedAst

proc parseTypeExpr(c: PContext, n: PNode): UntypedAst =
  process(c, n)

proc expr(c: PContext, n: PNode): UntypedAst =
  process(c, n)

proc stmt(c: PContext, n: PNode): UntypedAst =
  process(c, n)

proc exprs(c: PContext, n: PNode): UntypedAst =
  result = newNodeI(n.kind, n.info, n.len)
  for i, it in n.pairs:
    result[i] = expr(c, it)

proc stmts(c: PContext, n: PNode): UntypedAst =
  result = newNodeI(n.kind, n.info, n.len)
  for i, it in n.pairs:
    result[i] = stmt(c, it)

proc processArg(c: PContext, n: PNode): UntypedAst =
  case n.kind
  of nkExprEqExpr:
    result = newTreeI(nkExprEqExpr, n.info): [parseIdent(c, n[0]), expr(c, n[1])]
  else:
    result = expr(c, n)

proc processArg2(c: PContext, call: var UntypedAst, n: PNode) =
  case n.kind
  of nkExprEqExpr:
    call.add newTreeI(nkExprEqExpr, n.info, [parseIdent(c, n[0]), expr(c, n[1])])
  of nkHiddenStdConv:
    # a varargs container can be wrapped in a hidden standard conversion, so
    # we have process it here
    if n.len == 2:
      # ignore the first sub-node -- it's getting skipped anyway
      if n[1].kind != nkExprEqExpr:
        processArg2(c, call, n[1])
      else:
        call.add invalidAst(c, n[1])
    else:
      call.add invalidAst(c, n)
  of nkBracket:
    if n.typ != nil and tfVarargs in n.typ.flags:
      # this is the only place where we look at the type of the input AST
      # (outside of type expressions). We need to expand the content of an
      # implicitly generated varargs container, as a round-trip would be
      # otherwise not possible when the parameter is a ``varargs[typed]``
      for it in n.items:
        call.add expr(c, it)
    else:
      call.add expr(c, n)
  else:
    call.add expr(c, n)

proc parseColonExpr(c: PContext, n: PNode): UntypedAst =
  if n.len == 2:
    newTreeI(nkExprColonExpr, n.info, expr(c, n[0]), expr(c, n[1]))
  else:
    invalidAst(c, n)

proc empty(info: TLineInfo): UntypedAst =
  toUntyped ast.newNodeI(nkEmpty, info)

proc parsePragma(c: PContext, n: PNode): UntypedAst =
  unreachable("missing")

proc parseStrLit(c: PContext, n: PNode): UntypedAst =
  if n.kind in nkStrKinds:
    toUntyped ast.newStrNode(n.kind, n.strVal)
  else:
    invalidAst(c, n)

proc parseTry(c: PContext, n: PNode): UntypedAst =
  if n.len >= 2:
    result = prepareFrom(n)
    result[0] = expr(c, n[0])

    # don't check whether the finally clause comes last -- that's the
    # responsibility of semantic analysis
    for i, it in sliceIt(n, 1..^1):
      result[i] =
        case it.kind
        of nkExceptBranch:
          guardMinLen(it, 1): exprs(c, it)
        of nkFinally:
          guardLen(it, 1): newTreeI(nkFinally, it.info, expr(c, it[0]))
        else: invalidAst(c, it)
  else:
    result = invalidAstLen(c, n, 2)

proc parseIdentVis(c: PContext, n: PNode): UntypedAst =
  unreachable("missing")

proc parsePragmaList(c: PContext, n: PNode): UntypedAst =
  # note: a pragma list is allowed to be empty
  result = prepareFrom(n)
  for i, it in n.pairs:
    result[i] =
      case it.kind
      of nkExprColonExpr: parseColonExpr(c, it)
      else:               expr(c, it)

proc parsePragmaExpr(c: PContext, n: PNode): UntypedAst =
  if n.len == 2:
    result = prepareFrom(n)
    {.warning: "implementation missing".}
    result[0] = n[0] # TODO: missing
    result[1] =
      case n[1].kind
      of nkPragma: parsePragmaList(c, n[1])
      else:        invalidAst(c, n[1])
  else:
    result = invalidAst(c, n)

proc requireEmpty(c: PContext, n: PNode): UntypedAst =
  if n.kind == nkEmpty:
    empty(n.info)
  else:
    invalidAst(c, n)

template emptyOr(c: PContext, n: PNode, prc: untyped): UntypedAst =
  case n.kind
  of nkEmpty:
    empty(n.info)
  else:
    prc(c, n)

proc typeExpr(c: PContext, n: PNode): UntypedAst =
  expr(c, n)

proc commentStmt(c: PContext, n: PNode): UntypedAst =
  let node = ast.newNodeI(nkCommentStmt, n.info)
  node.comment = n.comment
  result = toUntyped node

proc parseVarTuple(c: PContext, n: PNode): UntypedAst =
  if n.len >= 2:
    for i in 0..<n.len-1:
      case n[i].kind
      of nkPostfix:
        result[i] = parseIdentVis(c, n[i]) # TODO: use ``parseDefVis``
      of nkPragmaExpr:
        result[i] = parsePragmaExpr(c, n[i])
      else:
        result[i] = parseDef(c, n[i])

    result[^1] = requireEmpty(c, n[^1])
  else:
    result = invalidAst(c, n)

proc strictIdentDefs(c: PContext, n: PNode): UntypedAst =
  ## Tries to parse an ``nkIdentDefs`` node, treating a non-empty value slot as
  ## an error
  if n.len >= 3:
    result = prepareFrom(n)
    for i in 0..<n.len-2:
      result[i] = parseDef(c, n[i])

    result[^2] = parseTypeExpr(c, n[^2])
    result[^1] = requireEmpty(c, n[^1])
  else:
    result = invalidAst(c, n)

const IdentLike = {nkIdent, nkSym, nkAccQuoted}

proc identWithPragma(c: PContext, n: PNode): UntypedAst =
  case n.kind
  of IdentLike:    parseDef(c, n)
  of nkPostfix:    parseIdentVis(c, n)
  of nkPragmaExpr: parsePragmaExpr(c, n)
  else:            invalidAst(c, n)

proc identVis(c: PContext, n: PNode): UntypedAst =
  case n.kind
  of IdentLike:    parseDef(c, n)
  of nkPostfix:    parseIdentVis(c, n)
  else:            invalidAst(c, n)

proc identDefs(c: PContext, n: PNode): UntypedAst =
  if n.len >= 3:
    result = prepareFrom(n)
    for i in 0..<n.len-2:
      result[i] = identWithPragma(c, n[i])

    result[^2] = emptyOr(c, n[^2], parseTypeExpr)
    result[^1] = emptyOr(c, n[^1], expr)
  else:
    result = invalidAst(c, n)

proc parseVariable(c: PContext, n: PNode): UntypedAst =
  ## Parses an entry in a var/let section
  case n.kind
  of nkIdentDefs:
    result = identDefs(c, n)
  of nkVarTuple:
    if n.len >= 3:
      result = newNodeI(nkVarTuple, n.info, n.len)
      for i in 0..<n.len-2:
        result[i] = expr(c, n[i])
      result[^2] = requireEmpty(c, n[^2])
      result[^1] = emptyOr(c, n[^1], expr)
    else:
      result = invalidAst(c, n)
  of nkCommentStmt: result = commentStmt(c, n)
  else:
    result = invalidAst(c, n)

proc parseColonEqExpr(c: PContext, n: PNode): UntypedAst =
  case n.kind
  of nkExprEqExpr, nkExprColonExpr:
    unreachable("missing")
  else:
    invalidAst(c, n)

proc safeDesym(c: PContext, n: PNode): UntypedAst =
  ## Sanitizes the symbol node `n` and returns the resulting untyped AST. `n`
  ## may be an invalid node, in which case an error node is returned
  if n.kind == nkSym and n.sym != nil:
    desym(c, n.sym, n.info)
  else:
    invalidAst(c, n)

proc collapseSymChoice(c: PContext, n: PNode, allowQualified: bool): UntypedAst =
  if n.len == 0:
    return invalidAst(c, n)

  case n[0].kind
  of nkSym:
    if n.len == 1 and n.kind == nkClosedSymChoice and allowQualified:
      # there's only one candiate and the choice is closed -> try to turn the
      # symbol into a qualified identifier
      result =
        if n[0].sym != nil: desym(c, n[0].sym, n[0].info)
        else: invalidAst(c, n[0])
    else:
      result = newNodeI(n.kind, n.info)
      for i, it in n.pairs:
        if it.kind == nkSym and it.sym != nil:
          if isReachable(c, it.sym):
            let symN = newNodeI(nkSym, it.info)
            symN.n.sym = it.sym
            result.add symN
        else:
          result.add invalidAst(c, it)

      if result.n.len == 0:
        # all symbols are unreachable. Use a normal identifier and let
        # semantic analysis figure this out
        result = newNodeI(nkIdent, n.info)
        result.n.ident = n[0].sym.name

  else:
    result = invalidAst(c, n)

proc parseDotExpr(c: PContext, n: PNode): UntypedAst =
  if n.len == 2:
    result = prepareFrom(n)
    result[0] = expr(c, n[0])
    result[1] =
      case n[1].kind
      of nkSymChoices: collapseSymChoice(c, n[1], false)
      else:            parseIdent(c, n[1])
  else:
    result = invalidAstLen(c, n, 2)

proc qualifiedIdent(c: PContext, n: PNode): UntypedAst =
  case n.kind
  of nkSym:
    if n.sym != nil: desym(c, n.sym, n.info)
    else:            invalidAst(c, n)
  of nkIdent: safeIdent(c, n)
  of nkAccQuoted: parseAccQuoted(c, n)
  of nkOpenSymChoice, nkClosedSymChoice:
    collapseSymChoice(c, n, true)
  else: invalidAst(c, n)

proc parseBranch(c: PContext, n: PNode, isExpr: bool): UntypedAst =
  if n.len >= 2:
    result = newNodeI(n.kind, n.info, n.len)
    for i, it in sliceIt(n, 0..^2):
      result[i] = expr(c, it)

    result[^1] = if isExpr: expr(c, n[^1])
                 else: stmt(c, n[^1])
  else:
    result = invalidAst(c, n)


proc processConstDef(c: PContext, n: PNode): UntypedAst =
  if n.len == 3:
    result = prepareFrom(n)
    result[0] =
      case n[0].kind
      of nkIdentKinds, nkPragmaExpr, nkPostfix: identWithPragma(c, n[0])
      of nkVarTuple: parseVarTuple(c, n[0])
      else: invalidAst(c, n[0])

    result[1] = emptyOr(c, n[1], parseTypeNode)
    result[2] = emptyOr(c, n[2], expr)
  else:
    result = invalidAst(c, n)

proc process*(c: PContext, n: PNode): UntypedAst =
  template checkMinSonsLen(n: PNode, length: int) =
    if n.len < length:
      {.line.}:
        return toUntyped c.config.newError(n, SemReport(kind: rsemIllformedAst, ast: n,
          str: "Expected at least $1 elements, but found $2" % [$length, $n.len]))

  template checkSonsLen(n: PNode, length: int) =
    if n.len != length:
      {.line.}:
        return toUntyped c.config.newError(n, SemReport(kind: rsemIllformedAst, ast: n,
          str: "Expected $1 elements, but found $2" % [$length, $n.len]))

  template invalid() =
    result = toUntyped c.config.newError(n, SemReport(kind: rsemIllformedAst, ast: n))

  case n.kind
  of nkNone:
    invalid()
  of nkEmpty:
    result = newNodeI(nkEmpty, n.info)
  of nkIdent:
    if n.ident != nil:
      result = newIdentNode(n.ident, n.info)
    else:
      invalid()
  of nkSym:
    if n.sym != nil:
      result = desym(c, n.sym, n.info)
    else:
      invalid()
  of nkType:
    # try to turn it into an identifier
    if n.typ != nil and n.typ.sym != nil:
      result = desym(c, n.typ.sym, n.info)
    else:
      invalid()
  of nkCharLit..nkUInt64Lit:
    result = block:
      let r = ast.newIntNode(n.kind, n.intVal)
      r.info = n.info
      toUntyped r
  of nkFloatLit..nkFloat128Lit:
    result = block:
      let r = ast.newNodeI(n.kind, n.info)
      r.floatVal = n.floatVal
      toUntyped r
  of nkStrLit..nkTripleStrLit:
    result = block:
      let r = ast.newNodeI(n.kind, n.info)
      r.strVal = n.strVal
      toUntyped r
  of nkNilLit:
    result = newNodeI(nkNilLit, n.info)
  of nkDotCall:
    # XXX: hm, turn into an ``nkDotExpr`` + ``nkCall`` instead?
    invalid()
  of nkCommand, nkCall:
    checkMinSonsLen(n, 1)
    result = newNodeI(n.kind, n.info)
    result.n.sons = newSeqOfCap[PNode](n.len)
    for it in n.items:
      processArg2(c, result, it)

  of nkCallStrLit:
    checkSonsLen(n, 2)
    result = prepareFrom(n)
    result[0] = qualifiedIdent(c, n[0])
    # note: we also allow normal string literals here (the parser doesn't)
    result[1] = parseStrLit(c, n[1])
  of nkInfix:
    checkSonsLen(n, 3)
    result = prepareFrom(n)
    result[0] = qualifiedIdent(c, n[0])
    result[1] = expr(c, n[1])
    result[2] = expr(c, n[2])
  of nkPrefix:
    checkSonsLen(n, 2)
    result = prepareFrom(n)
    result[0] = qualifiedIdent(c, n[0])
    result[1] = expr(c, n[1])
  of nkPostfix:
    invalid()
  of nkHiddenCallConv:
    # turn it into a normal call
    checkMinSonsLen(n, 1)
    result = newNodeI(nkCall, n.info, n.len)
    for i in 0..<n.len:
      result[i] = processArg(c, n[i])

  of nkExprEqExpr, nkExprColonExpr, nkIdentDefs, nkVarTuple:
    # these are only allowed in certain contexts
    invalid()
  of nkPar:
    # an ``nkPar`` as an expression must only have a single child
    checkSonsLen(n, 1)
    result = newTreeI(nkPar, n.info): expr(c, n[0])
  of nkObjConstr:
    checkMinSonsLen(n, 1)
    result = newNodeI(nkObjConstr, n.info, n.len)
    result[0] = typeExpr(c, n[0])
    for i in 1..<n.len:
      let it = n[i]
      if it.kind == nkExprColonExpr:
        result[i] = newTreeI(nkExprColonExpr, it.info): [parseIdent(c, it[0]), expr(c, it[1])]
      else:
        result[i] = invalidAst(c, it)

  of nkCurly:
    result = exprs(c, n)
  of nkCurlyExpr:
    checkMinSonsLen(n, 1)
    result = exprs(c, n)
  of nkBracket:
    result = exprs(c, n)
  of nkBracketExpr:
    # only a single sub-node is required, because ``nkBracketExpr`` is also used
    # for ``n[]``
    checkMinSonsLen(n, 1)
    result = exprs(c, n)
  of nkPragmaExpr:
    unreachable("missing")
  of nkRange:
    checkSonsLen(n, 2)
    result = newTreeI(nkInfix, n.info, ident(c, ".."), expr(c, n[0]), expr(c, n[1]))
  of nkDotExpr:
    result = parseDotExpr(c, n)
  of nkCheckedFieldExpr:
    # tricky. We *could* emit an 'if' statement from the condition with a
    # raise statement inside, but that might be a bit unintuitive and would
    # also mean that the processing doesn't roundtrip when sem-checking the
    # result and then passing it back into the sanitizer. Instead, we discard
    # the check part
    checkSonsLen(n, 2)
    if n[0].kind == nkDotExpr:
      result = parseDotExpr(c, n[0])
    else:
      invalid()
  of nkDerefExpr:
    checkSonsLen(n, 1)
    result = newTreeI(nkDerefExpr, n.info, toUntyped n[0])
  of nkIfExpr, nkIfStmt, nkWhenStmt:
    # treat all three nodes as an expression when we're in an expression
    # context
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i, it in n.pairs:
      result[i] =
        case it.kind
        of nkElifExpr, nkElifBranch:
          parseBranch(c, it, n.kind == nkIfExpr)
        of nkElseExpr, nkElse:
          # XXX: hm, also make sure that the 'else' is the last item? Or leave
          #      that to sem?
          guardLen(it, 1):
            newTreeI(it.kind, it.info):
              if it.kind == nkElseExpr: expr(c, it[0]) else: stmt(c, it[0])
        else:
          invalidAst(c, n[i])

  of nkLambda, nkDo:
    result = parseProcExpr(c, true, n)
  of nkAccQuoted:
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i in 0..<n.len:
      case n[i].kind
      of nkIntKinds:
        result[i] = ast.newIntNode(n[i].kind, n[i].intVal)
      of nkIdent:
        result[i] = newIdentNode(n[i].ident, n.info)
      of nkSym:
        result[i] = safeSymToIdent(c, n[i])
      else:
        result[i] = invalidAst(c, n[i])

  of nkTableConstr:
    # must have at least one sub node -- it'd be a ``nkCurly`` otherwise
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i, it in n.pairs:
      result[i] =
        if it.kind == nkExprColonExpr:
          guardLen(it, 2):
            newTreeI(it.kind, it.info, expr(c, it[0]), expr(c, it[1]))
        else:
          invalidAst(c, it)

  of nkBind:
    checkSonsLen(n, 1)
    result = newTreeI(nkBind, n.info): expr(c, n[0])
  of nkClosedSymChoice, nkOpenSymChoice:
    # TODO: revisit
    result = collapseSymChoice(c, n, true)
  of nkHiddenStdConv, nkHiddenSubConv:
    # QUESTION: does validating the nodes make sense if we're skipping them?
    checkSonsLen(n, 2)
    result = expr(c, n[1])
  of nkConv:
    # ``nkConv`` is not part of the untyped AST, so we translate it into an
    # ``nkCall``
    checkSonsLen(n, 2)
    result = newTreeI(nkCall, n.info): [parseTypeExpr(c, n[0]), expr(c, n[1])]
  of nkCast:
    checkSonsLen(n, 2)
    result = newTreeI(nkCast, n.info): [parseTypeExpr(c, n[0]), expr(c, n[1])]
  of nkStaticExpr:
    # skip
    checkSonsLen(n, 1)
    result = expr(c, n[0])
  of nkAddr:
    checkSonsLen(n, 1)
    result = newTreeI(nkAddr, n.info): expr(c, n[0])
  of nkHiddenAddr, nkHiddenDeref:
    checkSonsLen(n, 1)
    result = expr(c, n[0])
  of nkObjDownConv, nkObjUpConv:
    # XXX: these don't exist in typed AST prior to ``transf``. How should they
    #      be treated?
    invalid()
  of nkChckRange, nkChckRangeF, nkChckRange64, nkStringToCString, nkCStringToString:
    # XXX: neither do these
    invalid()
  of nkAsgn, nkFastAsgn:
    checkSonsLen(n, 2)
    result = newTreeI(n.kind, n.info): [expr(c, n[0]), expr(c, n[1])]
  of nkGenericParams, nkFormalParams:
    unreachable("missing")
  of nkOfInherit:
    unreachable("missing")
  of nkImportAs:
    # turn it back into an infix
    checkSonsLen(n, 2)
    result = newTreeI(nkInfix, n.info): [ident(c, "as"), expr(c, n[0]), expr(c, n[1])]
  of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef, nkMacroDef, nkTemplateDef, nkIteratorDef:
    # TODO: revisit the translation here
    if n.len-1 notin bodyPos..resultPos:
      return invalidAstLen(c, n, bodyPos)

    result = prepareFrom(n)
    result[namePos] = emptyOr(c, n[namePos], parseDef)
    result[patternPos] = requireEmpty(c, n[patternPos]) # TODO: missing
    result[genericParamsPos] = requireEmpty(c, n[genericParamsPos]) # TODO: missing

    case n[paramsPos].kind
    of nkEmpty:
      result[paramsPos] = empty(n[paramsPos].info)
    of nkFormalParams:
      result[paramsPos] = guardMinLen(n[paramsPos], 1):
        var r = prepareFrom(n[paramsPos])
        r[0] =
          case n[paramsPos][0].kind
          of nkEmpty: empty(n[paramsPos][0].info)
          else: typeExpr(c, n[paramsPos][0])
        for i, it in sliceIt(n[paramsPos], 1..^1):
          r[i] = identDefs(c, it)
        r
    else:
      result[paramsPos] = invalidAst(c, n[paramsPos])

    result[pragmasPos] =
      case n[pragmasPos].kind
      of nkEmpty:  empty(n[pragmasPos].info)
      of nkPragma: parsePragma(c, n[pragmasPos])
      else:        invalidAst(c, n[pragmasPos])

    # ignore the contents of the 'misc' slot
    result[miscPos] = empty(n.info)
    result[bodyPos] = stmt(c, n[bodyPos])
    if n.len > resultPos:
      result[resultPos] = empty(n.info)

  of nkOfBranch, nkElifBranch, nkExceptBranch, nkElse, nkElifExpr, nkElseExpr:
    invalid()
  of nkAsmStmt:
    checkSonsLen(n, 2)
    result = newTreeI(nkAsmStmt, n.info)
    result[0] = emptyOr(c, n, parsePragma)
    result[1] = parseStrLit(c, n[1])
  of nkPragma:
    result = parsePragmaList(c, n)
  of nkPragmaBlock:
    checkSonsLen(n, 2)
    result = prepareFrom(n)
    result[0] =
      if n[0].kind == nkPragma:
        parsePragmaList(c, n[0])
      else:
        invalidAst(c, n[0])

    result[1] = expr(c, n[1])
  of nkForStmt:
    checkMinSonsLen(n, 3)
    result = prepareFrom(n)
    # note: this allows for code that the parser doesn't
    for i, it in sliceIt(n, 0..^3):
      result[i] =
        case it.kind
        of nkVarTuple:   parseVarTuple(c, it)
        of nkPostfix:    parseIdentVis(c, it)
        of nkPragmaExpr: parsePragmaExpr(c, it)
        else:            parseIdent(c, it)

    result[^2] = expr(c, n[^2])
    result[^1] = stmt(c, n[^1])
  of nkWhileStmt:
    checkSonsLen(n, 2)
    result = newTreeI(nkWhileStmt, n.info): [expr(c, n[0]), stmt(c, n[1])]
  of nkCaseStmt:
    checkMinSonsLen(n, 2)
    result = newNodeI(nkCaseStmt, n.info, n.len)
    result[0] = expr(c, n[0])
    for i, it in sliceIt(n, 1..^1):
      result[i] =
        case it.kind
        of nkOfBranch: parseBranch(c, it, true)
        of nkElifBranch, nkElifExpr: parseBranch(c, it, true)
        of nkElse, nkElseExpr:
          guardLen(it, 1):
            newTreeI(it.kind, it.info): expr(c, it[0])
        else: invalidAst(c, it)
  of nkTypeSection:
    unreachable("missing")
  of nkVarSection, nkLetSection:
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i in 0..<n.len:
      result[i] = parseVariable(c, n[i])
  of nkConstSection:
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i, it in n.pairs:
      result[i] =
        case it.kind
        of nkCommentStmt: commentStmt(c, it)
        of nkConstDef: processConstDef(c, it)
        else: invalidAst(c, it)
  of nkConstDef, nkTypeDef:
    # context dependent
    invalid()
  of nkDefer:
    unreachable("missing")
  of nkYieldStmt, nkRaiseStmt, nkReturnStmt, nkBreakStmt, nkContinueStmt, nkDiscardStmt:
    checkSonsLen(n, 1)
    result = prepareFrom(n)
    result[0] =
      if n[0].kind == nkEmpty: empty(n[0].info)
      else:                    expr(c, n[0])

  of nkFinally:
    invalid()
  of nkBlockStmt:
    checkSonsLen(n, 2)
    result = prepareFrom(n)
    result[0] = expr(c, n[0])
    result[1] = stmt(c, n[1])
  of nkStaticStmt:
    checkSonsLen(n, 1)
    result = stmt(c, n[0])
  of nkStmtList:
    result = stmts(c, n)
  of nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt, nkIncludeStmt:
    # QUESTION: should we perform any additional validation here or leave that
    #           to sem?
    checkMinSonsLen(n, 1)
    result = exprs(c, n)
  of nkBindStmt, nkMixinStmt:
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i in 0..<n.len:
      result[i] = qualifiedIdent(c, n[i])

  of nkUsingStmt:
    # we're much more strict than the parser here: everything besides an ident
    # def is disallowed
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i in 0..<n.len:
      result[i] =
        case n.kind
        of nkIdentDefs:   strictIdentDefs(c, n[i])
        of nkCommentStmt: commentStmt(c, n[i])
        else:             invalidAst(c, n[i])

  of nkCommentStmt:
    result = commentStmt(c, n)
  of nkStmtListExpr:
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i, it in sliceIt(n, 0..^2):
      result[i] = stmt(c, it)
    result[^1] = expr(c, n[^1])
  of nkBlockExpr:
    checkSonsLen(n, 2)
    result = prepareFrom(n)
    result[0] = expr(c, n[0])
    result[1] = expr(c, n[1])
  of nkStmtListType:
    checkMinSonsLen(n, 1)
    result = prepareFrom(n)
    for i, it in sliceIt(n, 0..^2):
      result[i] = stmt(c, it)
    result[^1] = typeExpr(c, n[^1])
  of nkBlockType:
    checkSonsLen(n, 2)
    result = prepareFrom(n)
    result[0] = expr(c, n[0])
    result[1] = typeExpr(c, n[1])

  of nkWith, nkWithout:
    unreachable("missing")

  of nkTypeOfExpr:
    checkSonsLen(n, 1)
    result = prepareFrom(n)
    result[0] = expr(c, n[0])
  of nkObjectTy,
    nkTupleTy    ,
    nkTupleClassTy,
    nkTypeClassTy  ,
    nkStaticTy:
    unreachable("missing")
  of nkRecList,
    nkRecCase,
    nkRecWhen:
    # context dependent
    invalid()
  of nkRefTy,
    nkPtrTy,
    nkVarTy,
    nkConstTy,
    nkMutableTy,
    nkDistinctTy,
    nkProcTy,
    nkIteratorTy,
    nkSharedTy,
    nkEnumTy,
    nkEnumFieldDef:
    invalid()
  of nkArgList:
    unreachable("missing")
  of nkPattern:
    # disallow in macro output
    invalid()
  of nkTryStmt, nkHiddenTryStmt:
    # treat a hidden 'try' statement as a normal one
    result = parseTry(c, n)
  of nkClosure:
    # can't be expressed in untyped AST. Also only present in post-transform
    # AST
    # XXX: we could throw away the 'env' part and only use the 'prc' -- I'm
    #      not sure if that's a good idea however
    invalid()
  of nkGotoState, nkState:
    # only present in post-transform AST. Reject them.
    invalid()
  of nkTupleConstr:
    # can have zero items
    result = prepareFrom(n)
    for i, it in n.pairs:
      result[i] =
        if it.kind == nkExprColonExpr: parseColonExpr(c, it)
        else:                          expr(c, it)

  of nkError:
    # persisting the node is dangerous (the macro could have stored it
    # somewhere), but copying also doesn't sound like a good idea
    result = toUntyped n
  of nkNimNodeLit:
    # make sure the tree is not cyclic, but leave it as is otherwise
    if cyclicTree(n):
      result = toUntyped c.config.newError(n, SemReport(kind: rsemCyclicTree, ast: n))
    else:
      result = toUntyped n
  of nkModuleRef, nkReplayAction, nkNilRodNode:
    # only used for .rod file support. Reject them.
    invalid()