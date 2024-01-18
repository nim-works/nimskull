## Implements an IR for representing code in the code-generators. It's
## currently a slightly adjusted version of ``PNode``, but the idea is to
## simplify and evolve it, meaning that everything here is subject to
## change.

import
  std/[
    options
  ],
  compiler/ast/[
    ast_types,
    lineinfos,
    wordrecg
  ],
  compiler/utils/[
    containers
  ]

type
  CgNodeKind* = enum
    cnkInvalid ## the node is uninitialized

    cnkEmpty   ## represents the absence of something. The meaning depends
               ## on the context
    cnkType    ## a literal type

    cnkIntLit
    cnkUIntLit
    cnkFloatLit
    cnkStrLit

    cnkNilLit        ## the nil literal
    cnkAstLit        ## a ``NimNode`` literal

    cnkField         ## reference to an object field's symbol
    cnkLabel         ## name of a block
    cnkProc          ## name of a procedure
    cnkConst         ## reference to a named, global constant
    cnkGlobal        ## reference to a global location
    cnkLocal         ## reference to a local
    cnkMagic         ## name of a magic procedure. Only valid in the callee
                     ## slot of ``cnkCall`` and ``cnkCheckedCall`` nodes

    cnkCall          ## a procedure call. The first operand is the procedure,
                     ## the following operands the arguments
    cnkCheckedCall   ## like ``cnkCall``, but the call might raise an exception

    # constructors:
    cnkTupleConstr   ## tuple constructor
    cnkObjConstr     ## object constructor
    cnkSetConstr     ## set constructor
    cnkArrayConstr   ## array constructor
    cnkClosureConstr ## closure constructor

    cnkRange         ## a range expression in a ``case``-branch or

    cnkBinding       ## special node used in ``cnkObjConstr`` to associate a
                     ## field with a value

    cnkFieldAccess
    cnkArrayAccess
    cnkTupleAccess
    # future direction: merge ``cnkFieldAccess`` and ``cnkTupleAccess`` into a
    # single node (field access by position).

    cnkDeref         ## dereference 'x'
    cnkAddr          ## address of 'x'

    cnkHiddenAddr    ## create an internal reference of the operand
    cnkDerefView     ## dereference for a view
    # future direction: introduce ``cnkBind`` (or similar) for replacing the
    # ``x = hiddenAddr y`` operation
    # For views, consider lowering them into pointers (for backends where it
    # makes sense, e.g. C). This could make ``cnkDerefView`` obsolete

    cnkConv          ## a type conversion
    cnkLvalueConv    ## an lvalue-preserving conversion. The ones reaching
                     ## into the code generators are usually discarded, but
                     ## they're still required for proper typing
    cnkHiddenConv
    # future direction: the notion of "hidden" doesn't make any sense in the
    # context of code generation. Adjust the code generators so that they no
    # longer depend on ``cnkHiddenConv`` being different from ``cnkConv``, and
    # then remove the former
    cnkToSlice       ## slice creation. Works the same as the corresponding
                     ## MIR operation

    cnkObjDownConv   ## down conversion between `object` or `ref` types
    cnkObjUpConv     ## up conversion between `object` or `ref` types

    cnkCast          ## reinterpret the bit-pattern of the operand as a
                     ## different type

    cnkStmtList
    cnkStmtListExpr
    # future direction: remove ``cnkStmtListExpr``. The code generators know
    # based on the context a statement list appears in whether its an
    # expression or not

    cnkVoidStmt   ## discard the operand value (i.e., do nothing with it)
    cnkPragmaStmt ## a single compiler directive
    cnkEmitStmt   ## an ``emit`` statement
    cnkAsmStmt    ## an ``asm`` statement

    cnkIfStmt     ## only execute the body when the condition expression
                  ## evaluates to 'true'
    cnkRepeatStmt ## execute the body indefinitely
    cnkCaseStmt   ## a ``case`` statement
    cnkBlockStmt  ## an (optionally) labeled block

    cnkBreakStmt  ## break out of labeled block, or, if no label is provided,
                  ## the closest ``repeat`` loop
    cnkRaiseStmt  ## raise(x) -- set the `x` as the current exception and start
                  ## exceptional control-flow. `x` can be ``cnkEmpty`` in which
                  ## case "set current exception" part is skipped
    # future direction: lower the high-level raise statements (which means
    # "set the current exception" + "start exceptional control-flow") into
    # just "start exceptional control-flow"
    cnkReturnStmt

    cnkTryStmt
    cnkExcept
    cnkFinally

    cnkBranch     ## the branch of a ``case`` statement

    cnkDef        ## starts the lifetime of a local and optionally assigns an
                  ## initial value

    cnkAsgn       ## a = b
    cnkFastAsgn   ## fast assign b to a
    # future direction: have ``cnkAsgn`` mean "assign without implying any
    # copying" and then remove ``cnkFastAsgn``

const
  AllKinds = {low(CgNodeKind)..high(CgNodeKind)}

  cnkWithOperand*  = {cnkConv, cnkHiddenConv, cnkDeref, cnkAddr, cnkHiddenAddr,
                      cnkDerefView, cnkObjDownConv, cnkObjUpConv, cnkCast,
                      cnkLvalueConv}
  cnkAtoms*        = {cnkInvalid..cnkMagic, cnkReturnStmt, cnkPragmaStmt}
    ## node kinds that denote leafs
  cnkWithItems*    = AllKinds - cnkWithOperand - cnkAtoms
    ## node kinds for which the ``items`` iterator is available

  cnkLiterals* = {cnkIntLit, cnkUIntLit, cnkFloatLit, cnkStrLit}

type
  Local* = object
    ## Static information about a local variable. Initialized prior to code
    ## generation and only read (but not written) by the code generators.
    typ*: PType
    alignment*: uint32
    flags*: TSymFlags
    isImmutable*: bool
      ## whether the local is expected to not be mutated, from a high-level
      ## language perspective. Note that this doesn't meant that it really
      ## isn't mutated, rather this information is intended to help the
      ## the code generators optimize
    # future direction: merge `flags` and `isImmutable` into a single set of
    # flags
    name*: PIdent
      ## either the user-defined name or 'nil'

  BlockId* = distinct uint32
    ## Identifies a block within another block -- the IDs are **not** unique
    ## within a ``Body``. An outermost block has ID 0, a block within the
    ## block ID 1, etc.
  LocalId* = distinct uint32
    ## Identifies a local within a procedure.

  CgNode* {.acyclic.} = ref object
    ## A node in the tree structure representing code during the code
    ## generation stage. The "CG" prefix is short for "code generation".
    info*: TLineInfo
    typ*: PType
    case kind*: CgNodeKind
    of cnkInvalid, cnkEmpty, cnkType, cnkNilLit, cnkReturnStmt: discard
    of cnkIntLit, cnkUIntLit:
      # future direction: use a ``BiggestUint`` for uint values
      intVal*: BiggestInt
    of cnkFloatLit:   floatVal*: BiggestFloat
    of cnkStrLit:     strVal*: string
    of cnkAstLit:     astLit*: PNode
    of cnkMagic:      magic*: TMagic
    of cnkLabel:      label*: BlockId
    of cnkLocal:      local*: LocalId
    of cnkPragmaStmt: pragma*: TSpecialWord
    of cnkWithOperand: operand*: CgNode
    of cnkField, cnkProc, cnkConst, cnkGlobal:
      sym*: PSym
    of cnkWithItems:
      kids*: seq[CgNode]

  # future direction: move to a single-sequence-based, data-oriented design
  # for the code-generator IR

  Body* = object
    ## A self-contained CG IR fragment. This is usually the full body of a
    ## procedure.
    locals*: Store[LocalId, Local] ## all locals belonging to the body
    code*: CgNode

const
  resultId* = LocalId(0)
    ## the ID of the local representing the ``result`` variable

func len*(n: CgNode): int {.inline.} =
  n.kids.len

template `[]`*(n: CgNode, i: Natural): CgNode =
  n.kids[i]

template `[]`*(n: CgNode, i: BackwardsIndex): CgNode =
  {.cast(noSideEffect).}:
    n.kids[i]

iterator items*(n: CgNode): CgNode =
  var i = 0
  let L = n.kids.len
  while i < L:
    yield n.kids[i]
    inc i

iterator pairs*(n: CgNode): (int, CgNode) =
  var i = 0
  let L = n.kids.len
  while i < L:
    yield (i, n.kids[i])
    inc i

iterator sliceIt*[T](x: seq[T], lo, hi: Natural): (int, lent T) =
  var i = int(lo)
  while i <= hi:
    yield (i, x[i])
    inc i

template `[]`*(b: Body, id: LocalId): Local =
  ## Convenience shortcut.
  b.locals[id]

proc newStmt*(kind: CgNodeKind, info: TLineInfo,
              kids: varargs[CgNode]): CgNode =
  result = CgNode(kind: kind, info: info)
  result.kids = @kids

proc newExpr*(kind: CgNodeKind, info: TLineInfo, typ: PType,
              kids: varargs[CgNode]): CgNode =
  result = CgNode(kind: kind, info: info, typ: typ)
  result.kids = @kids

proc newNode*(kind: CgNodeKind; info = unknownLineInfo;
             typ = PType(nil)): CgNode =
  CgNode(kind: kind, info: info, typ: typ)

proc newOp*(kind: CgNodeKind; info: TLineInfo, typ: PType,
            opr: sink CgNode): CgNode =
  result = CgNode(kind: kind, info: info, typ: typ)
  result.operand = opr

func newLocalRef*(id: LocalId, info: TLineInfo, typ: PType): CgNode =
  CgNode(kind: cnkLocal, info: info, typ: typ, local: id)

proc `==`*(x, y: LocalId): bool {.borrow.}
proc `==`*(x, y: BlockId): bool {.borrow.}

proc merge*(dest: var Body, source: Body): CgNode =
  ## Merges `source` into `dest` by appending the former to the latter.
  ## Returns the node representing the code from `source` after it
  ## was merged.
  # merge the locals:
  let offset = dest.locals.merge(source.locals)

  proc update(n: CgNode, offset: uint32) {.nimcall.} =
    ## Offsets the ID of all references-to-``Local`` in `n` by `offset`.
    case n.kind
    of cnkLocal:
      n.local.uint32 += offset
    of cnkAtoms - {cnkLocal}:
      discard "nothing to do"
    of cnkWithOperand:
      update(n.operand, offset)
    of cnkWithItems:
      for it in n.items:
        update(it, offset)

  result = source.code

  if dest.code == nil:
    # make things easier by supporting `dest` being uninitialized
    dest.code = source.code
  elif source.code.kind != cnkEmpty:
    # update references to locals in source's code:
    update(source.code, offset.get(LocalId(0)).uint32)

    # merge the code fragments:
    case dest.code.kind
    of cnkEmpty:
      dest.code = source.code
    of cnkStmtList:
      dest.code.kids.add source.code
    else:
      dest.code = newStmt(cnkStmtList, dest.code.info,
                          [dest.code, source.code])
