## Implements the proto-MIR itself and the routines for translating AST to
## the proto-MIR.
##
## The proto-MIR is an intermediate representation that only covers a subset
## of expressions. It acts as an intermediate between AST and the MIR,
## offloading some attribute inheritance/synthesis from the main `AST-to-MIR
## translation <mirgen.html>`_.

import
  compiler/ast/[
    ast_types,
    ast_query,
    types,
    trees
  ],
  compiler/front/[
    in_options,
    options
  ],
  compiler/sem/[
    ast_analysis
  ],
  compiler/utils/[
    idioms
  ]

type
  ProtoItemKind* = enum
    # --- atoms
    pirLiteral ## some literal value or type
    pirProc
    pirParam
    pirLocal
    pirGlobal
    pirConst

    # --- projections
    pirDeref
    pirViewDeref

    pirTupleAccess
    pirFieldAccess
    pirArrayAccess
    pirSeqAccess
    pirVariantAccess

    pirLvalueConv

    # --- checked projections
    pirCheckedArrayAccess
    pirCheckedSeqAccess
    pirCheckedVariantAccess
    pirCheckedObjConv

    # --- non-constructors
    pirAddr
    pirView

    pirConv
    pirStdConv
    pirCast
    pirToSlice
    pirToSubSlice
    pirCall

    pirChckRange
    pirStringToCString
    pirCStringToString
    pirComplex ## a complex expression, such as an if-expression

    pirStmtList # usually skipped

    # --- constructors

    pirClosureConstr
    pirSetConstr
    pirObjConstr
    pirRefConstr
    pirTupleConstr
    pirArrayConstr

    pirConstExpr ## an expression that needs to be turned into an anonymous
                 ## constant

    # --- materialization

    pirMat       ## materialize the value into an owning temporary
    pirMatCursor ## materialize the value into non-owning temporary
    pirMatLvalue ## materialize the lvalue into a temporary reference

  Keep* = enum
    kDontCare  ## lvalue-ness doesn't matter
    kLvalue    ## whatever the item is translated to, it must have lvalue
               ## semantics
    kMutLvalue ## whatever the item is translated to, it must have mutable
               ## lvalue semantics

  ProtoItem* = object
    ## A chain of ``ProtoItem``s forms a proto-MIR expression. Multiple sub-
    ## expressions can be part of a single buffer, with each one delimited by
    ## a terminal item.
    ##
    ## The items are ordered as the AST expression would be displayed. Take,
    ## for example:
    ##
    ## .. code-block:: nim
    ##  a.b[1].c
    ##
    ## The item representing `a` comes first, then the dot (i.e., the field
    ## access), then `b`, etc.
    ##
    ## .. note:: All fields of ref types are cursors, in order to eliminate
    ##           costly copies and destructions. This is safe, because the AST
    ##           is guaranteed to outlive the proto-MIR items.
    orig* {.cursor.}: PNode
      ## the originating-from AST node
    typ* {.cursor.}: PType
      ## the type of the sub-expression. Can be different from that of `orig`
    keep*: Keep
      ## for lvalue items, what needs to be preserved for the resulting code
      ## to still have the correct behaviour. Ignored for all other items
    case kind*: ProtoItemKind
    of pirLocal, pirGlobal, pirParam, pirConst, pirProc:
      sym* {.cursor.}: PSym
    of pirTupleAccess:
      pos*: uint32
    of pirFieldAccess, pirVariantAccess:
      field* {.cursor.}: PSym
    of pirArrayAccess, pirSeqAccess, pirCheckedArrayAccess, pirCheckedSeqAccess:
      index*: int
        ## the item index of the index operand expression
    of pirCheckedVariantAccess:
      nodeIndex*: int
        ## the check's index in the originating-from ``nkCheckedFieldAccess``
    of pirCheckedObjConv:
      check* {.cursor.}: PType
        ## the type to emit a run-time check with
    of pirArrayConstr, pirClosureConstr, pirTupleConstr, pirObjConstr:
      owning*: bool
        ## whether the construction produces an owning aggregate value
    else:
      discard

  TranslateCtx* = object
    config: ConfigRef
    options: TOptions
    pickVm: bool
      ## pick the ``nimvm`` branch when faced with a when statement

  ExprKind* = enum
    Literal
    Lvalue
    Rvalue
      ## non-owning rvalue that can only be copied
    OwnedRvalue

const
  IrrelevantTypes = abstractInst + tyUserTypeClasses
  Projections = {pirDeref..pirLvalueConv,
                 pirCheckedArrayAccess..pirCheckedObjConv}

# ---- construction utilities ----

template addField(nodes: var seq[ProtoItem], n: PNode, k: ProtoItemKind, s: PSym) =
  nodes.add ProtoItem(orig: n, typ: s.typ, kind: k, field: s)

template add(nodes: var seq[ProtoItem], n: PNode, k: ProtoItemKind) =
  nodes.add ProtoItem(orig: n, typ: n.typ, kind: k)

template add(nodes: var seq[ProtoItem], n: PNode, t: PType, k: ProtoItemKind) =
  nodes.add ProtoItem(orig: n, typ: t, kind: k)

func add(nodes: var seq[ProtoItem], k: ProtoItemKind) =
  nodes.add ProtoItem(orig: nodes[^1].orig, typ: nodes[^1].typ, kind: k)

# ---- queries, analysis, etc. ----

func typ*(n: seq[ProtoItem]): PType {.inline.} =
  n[^1].typ

func classify*(e: seq[ProtoItem], i: int): ExprKind =
  ## Returns the kind of the given proto-MIR expression.
  # XXX: ownership is unrelated to whether a type has custom copy/sink/
  #      destruction logic. Taking the latter into consideration is an
  #      optimization that needs to eventually be removed
  case e[i].kind
  of pirLiteral, pirProc:
    Literal
  of Projections, pirLocal, pirGlobal, pirConst, pirParam, pirConstExpr,
     pirMatLvalue:
    # constant expression are later turned into anonymous constants, so
    # they're lvalues too
    Lvalue
  of pirCall, pirComplex, pirStringToCString, pirCStringToString:
    if hasDestructor(e[i].typ):
      OwnedRvalue
    else:
      Rvalue
  of pirObjConstr, pirTupleConstr, pirClosureConstr, pirArrayConstr:
    if e[i].owning and hasDestructor(e[i].typ):
      OwnedRvalue
    else:
      Rvalue
  of pirRefConstr:
    OwnedRvalue
  of pirSetConstr, pirAddr, pirView, pirCast, pirConv, pirStdConv,
     pirChckRange, pirToSlice, pirToSubSlice:
    Rvalue
  of pirMat:
    OwnedRvalue
  of pirMatCursor:
    Rvalue
  of pirStmtList:
    classify(e, i - 1)

func classify*(e: seq[ProtoItem]): ExprKind =
  ## Returns the kind of the given proto-MIR expression.
  classify(e, e.high)

func isPure(e: seq[ProtoItem], n: int): bool =
  ## Returns whether the expression at `n` is a pure expression. An expression
  ## is pure evaluating it doesn't depend on mutable state. In other words, a
  ## pure expression can be safely reordered with other statements/expressions.
  case e[n].kind
  of pirParam:
    # sink parameters are mutable and thus not pure
    e[n].typ.kind != tySink
  of pirConst, pirLiteral, pirProc, pirConstExpr:
    true
  of pirLocal, pirGlobal:
    # let bindings are pure, but only if they don't have a destructor (in
    # which case they're movable)
    e[n].sym.kind in {skLet, skForVar} and not hasDestructor(e[n].typ)
  of pirFieldAccess, pirTupleAccess, pirVariantAccess, pirLvalueConv,
     pirCheckedObjConv, pirCheckedVariantAccess, pirStmtList:
    # static projections are pure if their operand is pure. Statment-list are
    # later split from the expression, meaning that they don't affect pureness
    isPure(e, n - 1)
  of pirArrayAccess, pirSeqAccess, pirCheckedArrayAccess, pirCheckedSeqAccess:
    # note: the AST-to-PMIR tranlsation made sure that the index operand is
    # pure
    isPure(e, n - 1)
  of pirMatLvalue, pirAddr, pirView, pirConv, pirStdConv, pirCast, pirToSlice:
    # depends on the operand
    isPure(e, n - 1)
  of pirMat, pirMatCursor:
    # the materialized-into temporary is never assigned to
    true
  of pirDeref, pirViewDeref:
    # the pointer destination could change (unless it's an immutable view)
    false
  of pirSetConstr, pirObjConstr, pirTupleConstr, pirArrayConstr,
     pirClosureConstr, pirRefConstr, pirStringToCString, pirCStringToString,
     pirToSubSlice, pirChckRange, pirCall, pirComplex:
    # not analyzable
    false

func isStable(e: seq[ProtoItem], n: int): bool =
  ## Returns whether the run-time address of the lvalue expression `n` is
  ## always the same, regardless of when the address is computed.
  case e[n].kind
  of pirGlobal, pirParam, pirLocal, pirConst, pirConstExpr, pirMat,
     pirMatCursor, pirMatLvalue:
     # location names (which includes constant expressions) and materialized
     # expressions are never change identity
    true
  of pirArrayAccess, pirFieldAccess, pirTupleAccess, pirVariantAccess,
     pirLvalueConv, pirStmtList:
    isStable(e, n - 1)
  of pirSeqAccess, pirCheckedSeqAccess:
    # dynamic arrays are stable when the array and index operands are pure.
    # Index operand purity is ensured by the to-PMIR translation
    isPure(e, n - 1)
  of pirCheckedArrayAccess, pirCheckedVariantAccess, pirCheckedObjConv:
    # static checked access requires the operand to be captured, and
    # temporaries are always stable
    # XXX: the operand being captured is an implementation detail of the
    #      PMIR-to-MIR translation, which is accounted for here in order to
    #      produce better MIR code. Ideally this shouldn't be here
    true
  of pirDeref, pirViewDeref:
    # a pure target means that the pointer is always the same. Fortunately,
    # the to-PMIR translation made sure that the target always is pure
    true
  else:
    unreachable(e[n].kind)

func wantConsumeable*(e: var seq[ProtoItem]) =
  ## Makes sure `e` is an expression that can be used in a context requiring a
  ## certainly-consumeable value.
  case classify(e, e.high)
  of Rvalue, OwnedRvalue:
    if e[^1].kind != pirMat:
      # requires an owning temporary
      e.add pirMat
  of Lvalue:
    e.add pirMat
  of Literal:
    discard "okay, can be used as is"

proc wantPure*(e: var seq[ProtoItem]) =
  ## Makes sure `e` is a pure expression. An impure expression is turned into
  ## a pure expression by materializing it into a temporary and then using the
  ## temporary in its place.
  case classify(e, e.high)
  of Literal:
    discard "already pure"
  of Lvalue:
    if not isPure(e, e.high):
      e.add pirMatCursor
  of Rvalue:
    e.add pirMatCursor
  of OwnedRvalue:
    e.add pirMat

proc wantValue*(e: var seq[ProtoItem]) =
  ## Makes sure `e` is a literal value or lvalue expression.
  case classify(e, e.high)
  of Lvalue, Literal:
    discard "nothin to do"
  of Rvalue:
    e.add pirMatCursor
  of OwnedRvalue:
    e.add pirMat

proc wantShallow*(e: var seq[ProtoItem]) =
  ## Makes sure `e` is something that can be assigned to a non-owning
  ## destination.
  if classify(e, e.high) == OwnedRvalue:
    # commit to a temporary
    e.add pirMat

proc wantStable*(e: var seq[ProtoItem]) =
  ## Makes sure `e` is a stable lvalue expression. Rvalues and literal values
  ## are committed to temporaries.
  case classify(e, e.high)
  of Lvalue:
    if not isStable(e, e.high):
      e.add pirMatLvalue
  of OwnedRvalue:
    e.add pirMat
  of Rvalue, Literal:
    e.add pirMatCursor

# ---- translation routines ----

func selectWhenBranch*(n: PNode, isNimvm: bool): PNode =
  assert n.kind == nkWhen
  if isNimvm: n[0][1]
  else:       n[1][0]

func handleConstExpr(result: var seq[ProtoItem], n: PNode, kind: ProtoItemKind,
                     sink: bool) =
  ## If eligible, translates `n` to a constant expression. To a construction of
  ## kind `kind` otherwise.
  ##
  ## Only fully constant, non-empty aggregate or set constructions are
  ## treated as constant expressions.
  if not sink and n.len > ord(n.kind == nkObjConstr) and isDeepConstExpr(n):
    result.add ProtoItem(orig: n, typ: n.typ, kind: pirConstExpr)
  elif kind == pirSetConstr:
    result.add ProtoItem(orig: n, typ: n.typ, kind: kind)
  else:
    result.add ProtoItem(orig: n, typ: n.typ, kind: kind)
    result[^1].owning = sink

proc analyseObjConv(c: TranslateCtx, n: PNode): (PNode, PType) =
  ## Given an ``nkObjDownConv`` or ``nkObjUpConv`` AST, returns the operand to
  ## the conversion (skipping all intermediate up- and down-conversions)
  ## together with the type that needs to be checked at run-time.
  ##
  ## If object-conversion checks are disabled or there's no type that needs
  ## to be checked, 'nil' is returned for the type.
  let
    skipped = n.typ.skipTypes(IrrelevantTypes)
    # only ref and ptr types are checked during conversions, normal objects
    # are not
    needsCheck = (optObjCheck in c.options) and
                 (skipped.kind in {tyPtr, tyRef}) and
                 not isObjLackingTypeField(skipped.lastSon)

  # find the first non-conversion node:
  var start {.cursor.} = n
  while start.kind in {nkObjDownConv, nkObjUpConv}:
    start = start[0]

  var deepest = start.typ.skipTypes(skipPtrs)
    ## the type most nested in the type hierarchy that's certain to be valid.
    ## If a sibling conversion exists in the chain, this is the first
    ## encountered sibling type
  if needsCheck:
    # walk the conversions again and look for the type deepest in the
    # hierarchy
    var x {.cursor.} = n
    while x.kind in {nkObjDownConv, nkObjUpConv}:
      let typ = x.typ.skipTypes(skipPtrs)
      if (let rel = inheritanceDiff(typ, deepest); rel > 0):
        deepest = typ
        if rel == high(typeof(rel)):
          # whatever other types there are, a sibling conversion will always
          # result in a run-time error
          break

      x = x[0]

  if not needsCheck or deepest == start.typ.skipTypes(skipPtrs):
    # checks are disabled or the conversion is proven to never result
    # in run-time type errors
    deepest = nil

  result = (start, deepest)

proc arrayAccessToPmir(c: TranslateCtx, n: PNode, index: int,
                       op, checkedOp: ProtoItemKind): ProtoItem =
  {.cast(uncheckedAssign).}:
    if optBoundsCheck in c.options and needsIndexCheck(c.config, n[0], n[1]):
      ProtoItem(orig: n, typ: n.typ, kind: checkedOp, index: index)
    else:
      ProtoItem(orig: n, typ: n.typ, kind: op, index: index)

proc tupleAccessToPmir(n: PNode, tupType: PType, pos: int): ProtoItem =
  # XXX: the AST is not always correctly typed at the moment
  #      (tests/lang_callable/converter/tgenericconverter.nim is an example of
  #      where this happens), so the correct type has to looked up from the
  #      tuple
  ProtoItem(orig: n, typ: tupType[pos], kind: pirTupleAccess,
            pos: pos.uint32)

proc wantArray(e: var seq[ProtoItem]) =
  wantValue(e)
  if e[^1].typ.skipTypes(abstractVar).kind in {tyOpenArray, tyVarargs}:
    # no lvalue is required, but having one is also not an issue
    discard
  elif e[^1].kind notin {pirLiteral}:
    # XXX: behaviour-wise, sequence-like types don't need an lvalue capture,
    #      but ``vmgen`` would generate significantly less efficient code
    #      without them, so we do prefer lvalue captures
    e[^1].keep = kLvalue

proc exprToPmir(c: TranslateCtx, result: var seq[ProtoItem], n: PNode, sink: bool) =
  ## Translates the single node `n` and recurses if it's a non-terminal. This
  ## procedure makes up the core of the AST-to-proto-MIR translation.
  ##
  ## `sink` informs whether the expression appears in a context where an
  ## owning expression is expected. It decides whether aggregate productions
  ## produce owning or non-owning values, and is not propagated through
  ## projections:
  ##
  ##   var a = (x: "a", y: "b").x
  ##
  ## Here, the tuple construction doesn't result in an owning tuple. A sink
  ## context only propagates through lvalue-preserving conversions.
  template recurse(n: PNode; sink: bool) =
    exprToPmir(c, result, n, sink)

  template wantPure(n: PNode) =
    recurse(n, false)
    wantPure(result)

  template wantValue(n: PNode, sink = false) =
    recurse(n, sink)
    wantValue(result)

  template wantLvalue(n: PNode, sink = false) =
    wantValue(n, sink)
    result[^1].keep = kLvalue

  template wantArray(n: PNode) =
    recurse(n, false)
    wantArray(result)

  template node(k: ProtoItemKind) =
    result.add ProtoItem(orig: n, typ: n.typ, kind: k)

  template node(k: ProtoItemKind, field, val: untyped) =
    result.add ProtoItem(orig: n, typ: n.typ, kind: k, field: val)

  case n.kind
  of nkCharLit..nkNilLit, nkRange, nkNimNodeLit:
    node pirLiteral
  of nkLambdaKinds:
    node pirProc, sym, n[namePos].sym
  of nkSym:
    let kind: range[pirProc..pirConst] =
      case n.sym.kind
      of skVar, skLet, skForVar:
        if sfGlobal in n.sym.flags:
          pirGlobal
        else:
          pirLocal
      of skTemp, skResult:
        pirLocal
      of skParam:
        pirParam
      of skConst:
        pirConst
      of skProc, skFunc, skConverter, skMethod, skIterator:
        pirProc
      else:
        unreachable(n.sym.kind)

    result.add ProtoItem(orig: n, typ: n.sym.typ, kind: kind, sym: n.sym)
  of nkDerefExpr:
    wantPure(n[0])
    node pirDeref
  of nkHiddenDeref:
    case classifyBackendView(n[0].typ)
    of bvcSingle:
      # it's a deref of a view
      wantPure(n[0])
      node pirViewDeref
    of bvcSequence:
      # no node on its own
      wantValue(n[0])
    of bvcNone:
      # it's a ``ref`` or ``ptr`` deref
      wantPure(n[0])
      node pirDeref
  of nkDotExpr:
    # normalize into either a field or tuple access
    wantValue(n[0])
    let typ = n[0].typ.skipTypes(IrrelevantTypes)
    case typ.kind
    of tyObject:
      node pirFieldAccess, field, n[1].sym
    of tyTuple:
      result.add tupleAccessToPmir(n, typ, n[1].sym.position)
    else:
      unreachable()
  of nkBracketExpr:
    # normalize into either a tuple or array access
    let typ = n[0].typ.skipTypes(IrrelevantTypes)
    case typ.kind
    of tyTuple:
      wantValue(n[0])
      result.add tupleAccessToPmir(n, typ, n[1].intVal.int)
    of tyArray, tyUncheckedArray, tySequence, tyString, tyOpenArray,
       tyVarargs, tyCstring:
      wantPure(n[1]) # ensure pure expressions for index operands
      let index = result.high
      wantArray(n[0])
      if typ.kind in {tyArray, tyUncheckedArray}:
        result.add arrayAccessToPmir(c, n, index,
                                     pirArrayAccess, pirCheckedArrayAccess)
      else:
        result.add arrayAccessToPmir(c, n, index,
                                     pirSeqAccess, pirCheckedSeqAccess)
    else:
      unreachable()
  of nkCheckedFieldExpr:
    wantLvalue(n[0][0])
    let field = n[0][1].sym
    for i in countdown(n.len - 1, 1):
      let check = n[i]
      assert check.kind in nkCallKinds
      let discr =
        if check[0].sym.magic == mNot: check[1][2].sym
        else:                          check[2].sym

      if optFieldCheck in c.options:
        result.add ProtoItem(orig: n, typ: n[0][0].typ, keep: kLvalue,
                             kind: pirCheckedVariantAccess, nodeIndex: i)
      else:
        result.add ProtoItem(orig: n, typ: n[0][0].typ, keep: kLvalue,
                             kind: pirVariantAccess, field: discr)

    result.addField n, pirFieldAccess, field

  of nkObjDownConv, nkObjUpConv:
    # the object conversion might need a run-time check
    let (start, typ) = analyseObjConv(c, n)
    wantValue(start, sink) # propgate the sink context to the operand
    if typ.isNil:
      # no check is needed
      node pirLvalueConv
    else:
      # an lvalue is only required for pointer-like values when used in a sink
      # context (so that it can be moved)
      if n.typ.skipTypes(IrrelevantTypes).kind notin {tyPtr, tyRef} or
         sink:
        result[^1].keep = kLvalue

      node pirCheckedObjConv, check, typ

  of nkHiddenStdConv:
    case n.typ.skipTypes(abstractVar).kind
    of tyOpenArray:
      # an lvalue is always requested, even if the operand is of run-time-
      # array-like type (e.g., string). This is because some MIR passes need to
      # modify the sequence value directly (for example, this is needed for the
      # copy-on-write handling)
      wantLvalue(n[1])
      node pirToSlice
    else:
      wantValue(n[1])
      node pirStdConv
  of nkHiddenSubConv, nkConv:
    if compareTypes(n.typ, n[1].typ, dcEqIgnoreDistinct, {IgnoreTupleFields}):
      # it's an lvalue-preserving conversion
      wantValue(n[1], sink)
      node pirLvalueConv
    elif n.typ.skipTypes(abstractVar).kind == tyOpenArray:
      # to-openArray conversion also reach here as ``nkHiddenSubConv``
      # sometimes
      wantLvalue(n[1])
      node pirToSlice
    else:
      wantValue(n[1])
      node pirConv

  of nkAddr:
    wantLvalue(n[0])
    node pirAddr
  of nkHiddenAddr:
    recurse(n[0], false)
    case classifyBackendView(n.typ)
    of bvcSingle:
      # a view into the source operand is created
      node pirView
    of bvcSequence:
      # the addr operation itself is a no-op, but the operand needs to be
      # re-typed
      var i = result.high
      # push the type through statement lists
      while result[i].kind == pirStmtList:
        result[i].typ = n.typ
        dec i
      result[i].typ = n.typ
    of bvcNone:
      # normal address-of operation
      node pirAddr

  of nkChckRangeF, nkChckRange64, nkChckRange:
    if optRangeCheck notin c.options or
       skipTypes(n.typ, abstractVar).kind in {tyUInt..tyUInt64}:
      # unsigned types should be range checked, see: https://github.com/nim-works/nimskull/issues/574
      wantValue(n[0])
      node pirConv
    else:
      node pirChckRange
  of nkStringToCString:
    node pirStringToCString
  of nkCStringToString:
    node pirCStringToString
  of nkCast:
    wantValue(n[1])
    node pirCast
  of nkCallKinds:
    # some magics need special handling
    case getMagic(n)
    of mAccessEnv:
      # phase ordering issue: ``liftdestructors`` emits
      # ``(Addr (Call (Sym 'accessEnv') (...)))``, which is invalid, since
      # an rvalue expression cannot have its address taken. We work around
      # this by lowering the access into a tuple access already, making the
      # expression well-formed again
      wantValue(n[1])
      node pirTupleAccess, pos, 1'u32
    of mSlice:
      # XXX: a HiddenStdConv erroneously ends up in the array position
      #      sometimes, which would, if kept, lead to index errors when the
      #      array doesn't start at index 0
      wantLvalue(skipConv(n[1]))
      # the other operands are translated separately when needed
      node pirToSubSlice
    else:
      node pirCall

  of nkBracket:
    # if the construction is of seq type, then it's a constant seq value,
    # which we prefer to lift into a constant (again)
    let consume =
      n.typ.skipTypes(IrrelevantTypes).kind != tySequence and sink
    handleConstExpr(result, n, pirArrayConstr, consume)
  of nkCurly:
    # never treat set constructions as appearing in a sink context, so that
    # they're always turned into constants, if possible
    handleConstExpr(result, n, pirSetConstr, false)
  of nkObjConstr:
    if n.typ.skipTypes(IrrelevantTypes).kind == tyRef:
      # ref constructions are never constant
      result.add n, pirRefConstr
    else:
      handleConstExpr(result, n, pirObjConstr, sink)
  of nkTupleConstr:
    handleConstExpr(result, n, pirTupleConstr, sink)
  of nkClosure:
    handleConstExpr(result, n, pirClosureConstr, sink)

  of nkWhenStmt:
    # a ``when nimvm`` expression
    recurse(selectWhenBranch(n, c.pickVm), sink)
  of nkStmtListExpr:
    recurse(n.lastSon, sink)
    # HACK: inherit the type from the child node. This prevents incorrectly
    #       typed array constructions (a sem bug) from appearing to work
    #       correctly (see
    #       tests/lang_exprs/tempty_typed_expressions_issues.nim)
    result.add ProtoItem(orig: n, typ: result[^1].typ, kind: pirStmtList)
    #node pirStmtList
  of nkPragmaBlock:
    # the pragma is uninteresting here, just skip it
    recurse(n.lastSon, sink)
  of nkBlockExpr, nkIfExpr, nkCaseStmt, nkTryStmt:
    node pirComplex
  else:
    unreachable(n.kind)

proc exprToPmir*(options: TOptions, config: ConfigRef, pickVm: bool, n: PNode,
                 sink, mutable: bool): seq[ProtoItem] {.inline.} =
  ## Translates the expression `n` and all its relevant sub-expression into
  ## the corresponding proto-MIR representation, already performing some
  ## necessary lowering.
  exprToPmir(TranslateCtx(config: config, options: options, pickVm: pickVm),
             result, n, sink)
  # handle the mutability request:
  if mutable:
    assert classify(result, result.high) == Lvalue
    result[^1].keep = kMutLvalue
    # MIR rule: mutability must be preserved for lvalue conversion operands
    var i = result.high
    while result[i].kind in {pirLvalueConv, pirCheckedObjConv, pirStmtList}:
      dec i
    result[i].keep = kMutLvalue
