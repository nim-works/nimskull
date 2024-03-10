#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Template evaluation engine. Now hygienic.

import
  compiler/ast/[
    ast,
    astalgo,
    lineinfos,
    idents,
    renderer,
    errorhandling,
    errorreporting,
    types
  ],
  compiler/front/[
    options,
    msgs
  ],
  compiler/utils/[
    debugutils
  ]

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport,
  reportAst,
  reportSem
from compiler/ast/report_enums import ReportKind

type
  TemplCtx = object
    owner, genSymOwner: PSym
    instLines: bool   ## use the instantiation lines numbers
    isDeclarative: bool
    mapping: TIdTable ## every gensym'ed symbol needs to be mapped to some
                      ## new symbol
    config: ConfigRef
    ic: IdentCache
    instID: int
    idgen: IdGenerator

proc copyNode(ctx: TemplCtx, a, b: PNode): PNode =
  result = copyNode(a)
  if ctx.instLines: result.info = b.info

proc evalTemplateAux(templ, actual: PNode, c: var TemplCtx, result: PNode) =
  template handleParam(param: PNode) =
    let x = param
    case x.kind
    of nkArgList:
      for y in items(x): result.add(y)
    else:
      var x = copyTree(x)
      if x.typ != nil:
        case x.typ.kind
        of tyStatic:
          x.typ = x.typ.base
        else:
          discard
      result.add x

  case templ.kind
  of nkSym:
    var s = templ.sym
    if (s.owner == nil and s.kind == skParam) or s.owner == c.owner:
      if s.kind == skParam and {sfGenSym, sfTemplateParam} * s.flags == {sfTemplateParam}:
        handleParam actual[s.position]
      elif (s.owner != nil) and (s.kind == skGenericParam or
           s.kind == skType and s.typ != nil and s.typ.kind == tyGenericParam):
        handleParam actual[s.owner.typ.len + s.position - 1]
      else:
        c.config.internalAssert(sfGenSym in s.flags or s.kind == skType)
        var x = PSym(idTableGet(c.mapping, s))
        if x == nil:
          x = copySym(s, nextSymId(c.idgen))
          # sem'check needs to set the owner properly later, see bug #9476
          x.owner = nil # c.genSymOwner
          #if x.kind == skParam and x.owner.kind == skModule:
          #  internalAssert c.config, false
          idTablePut(c.mapping, s, x)
        if sfGenSym in s.flags:
          result.add newIdentNode(getIdent(c.ic, x.name.s & "`gensym" & $c.instID),
            if c.instLines: actual.info else: templ.info)
        else:
          result.add newSymNode(x, if c.instLines: actual.info else: templ.info)
    else:
      result.add copyNode(c, templ, actual)
  of nkEmpty..nkIdent, nkType..nkNilLit: # atom
    result.add copyNode(c, templ, actual)
  of nkCommentStmt:
    # for the documentation generator we don't keep documentation comments
    # in the AST that would confuse it (bug #9432), but only if we are not in a
    # "declarative" context (bug #9235).
    if c.isDeclarative:
      result.add copyNode(c, templ, actual)
    else:
      result.add newNodeI(nkEmpty, templ.info)
  of nkError:
    c.config.internalError(templ.info, "unreported error")
  of nkWithSons:
    let parentIsDeclarative = c.isDeclarative
    if templ.kind in routineDefs + {nkTypeSection, nkVarSection, nkLetSection, nkConstSection}:
      c.isDeclarative = true
    # fixes bug #16993, bug #18054
    if c.isDeclarative or templ.kind notin nkCallKinds or not isRunnableExamples(templ[0]):
      var res = copyNode(c, templ, actual)
      for i in 0..<templ.len:
        evalTemplateAux(templ[i], actual, c, res)
      result.add res
    c.isDeclarative = parentIsDeclarative

proc evalTemplateArgs*(n: PNode, s: PSym; conf: ConfigRef; fromHlo: bool): PNode =
  ## Produces an ``nkArgList`` node storing all arguments taken from the
  ## call-like expression `n`. Immediate templates/macros are also considered,
  ## by making sure enough parameters are provided and by inserting the default
  ## for parameters where no argument is provided.
  ##
  ## Despite the name, the procedure also applies to macro arguments.
  addInNimDebugUtils(conf, "evalTemplateArgs", s, n, result)
  # if the template has zero arguments, it can be called without ``()``
  # `n` is then a nkSym or something similar
  let
    totalParams = if n.kind in nkCallKinds: n.len-1
                  else: 0
    # XXX: Since immediate templates/macros are not subject to the
    # standard sigmatching algorithm, they will have a number
    # of deficiencies when it comes to generic params:
    # Type dependencies between the parameters won't be honoured
    # and the bound generic symbols won't be resolvable within
    # their bodies. We could try to fix this, but it may be
    # wiser to just deprecate immediate templates and macros
    # now that we have working untyped parameters.
    genericParams = if fromHlo: 0
                    else: s.ast[genericParamsPos].safeLen
    expectedRegularParams = s.typ.len-1 
    givenRegularParams = max(totalParams - genericParams, 0)

  if totalParams > expectedRegularParams + genericParams:
    globalReport(conf, n.info, reportAst(rsemWrongNumberOfArguments, n))

  if totalParams < genericParams:
    globalReport(conf, n.info, reportAst(
      rsemMissingGenericParamsForTemplate, n, sym = s))

  result = newNodeI(nkArgList, n.info)

  for i in 1..givenRegularParams:
    # xxx: propagate nkError
    for e in walkErrors(conf, n[i]):
      conf.localReport(e)

    result.add n[i]

  # handle parameters with default values, which were
  # not supplied by the user
  for i in givenRegularParams+1..expectedRegularParams:
    let default = s.typ.n[i].sym.ast

    if default.isNil or default.kind == nkEmpty:
      result.add newNodeI(nkEmpty, n.info)
      return newError(conf, result, PAstDiag(kind: adSemWrongNumberOfArguments))
    else:
      result.add default.copyTree

  # add any generic parameters
  for i in 1..genericParams:
    let it = n[givenRegularParams + i]

    # xxx: propagate nkError
    for e in walkErrors(conf, it):
      conf.localReport(e)

    result.add it

# to prevent endless recursion in template instantiation
const evalTemplateLimit* = 1000

proc wrapInComesFrom*(info: TLineInfo; sym: PSym; res: PNode): PNode =
  # The original idea of this proc was to use a 'nkComesFrom' node to
  # store template/macro information for generating better stack traces:
  # result = newTreeIT(nkStmtListExpr, info, res.typ):
  #   [newTreeI(nkComesFrom, info, newSymNode(sym, info)), res]
  result = res
  result.info = info
  if result.kind in {nkStmtList, nkStmtListExpr} and result.len > 0:
    result.lastSon.info = info

proc evalTemplate*(n: PNode, tmpl, genSymOwner: PSym;
                   conf: ConfigRef;
                   ic: IdentCache; instID: ref int;
                   idgen: IdGenerator;
                   fromHlo=false): PNode =
  addInNimDebugUtils(conf, "evalTemplate", n, result)

  inc(conf.evalTemplateCounter)
  if conf.evalTemplateCounter > evalTemplateLimit:
    globalReport(conf, n.info, SemReport(
      kind: rsemTemplateInstantiationTooNested))

    result = n

  # replace each param by the corresponding node:
  let args = evalTemplateArgs(n, tmpl, conf, fromHlo)
  var ctx = TemplCtx(
    owner: tmpl,
    genSymOwner: genSymOwner,
    config: conf,
    ic: ic,
    mapping: newIdTable(),
    instID: instID[],
    idgen: idgen
  )
  if args.kind == nkError:
    return args # xxx: not the best way to do it, but this signals a mismatch
  let body = tmpl.ast[bodyPos]
  if isAtom(body):
    result = newNodeI(nkPar, body.info)
    evalTemplateAux(body, args, ctx, result)
    assert result.len == 1
    result = result[0]
  else:
    ctx.instLines = sfCallsite in tmpl.flags
    result = copyNode(ctx, body, n)
    for i in 0..<body.safeLen:
      evalTemplateAux(body[i], args, ctx, result)
  result.flags.incl nfFromTemplate
  result = wrapInComesFrom(n.info, tmpl, result)

  dec(conf.evalTemplateCounter)
  # The instID must be unique for every template instantiation, so we increment it here
  inc instID[]
