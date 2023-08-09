## Implements an IR for representing code in the code-generators. It's
## currently a slightly adjusted version of ``PNode``, but the idea is to
## simplify and evolve it, meaning that everything here is subject to
## change.

import
  compiler/ast/[
    ast_types,
    lineinfos,
    wordrecg
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

    cnkSym
    # future direction: split up ``cnkSym`` in the way the MIR does it
    cnkMagic         ## name of a magic procedure. Only valid in the callee
                     ## slot of ``cnkCall`` nodes

    cnkCall          ## a procedure call. The first operand is the procedure,
                     ## the following operands the arguments

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
    cnkBracketAccess
    # future direction: split ``cnkBracketAccess`` into ``cnkArrayAccess`` (for
    # array-like operands) and ``cnkTupleAccess`` (for tuples).
    cnkCheckedFieldAccess
    # future direction: lower object access checks eariler (e.g., during the
    # MIR phase) and then remove ``cnkCheckedFieldAccess``

    cnkDeref         ## dereference 'x'
    cnkAddr          ## address of 'x'

    cnkHiddenAddr    ## create an internal reference of the operand
    cnkDerefView     ## dereference for a view
    # future direction: introduce ``cnkBind`` (or similar) for replacing the
    # ``x = hiddenAddr y`` operation
    # For views, consider lowering them into pointers (for backends where it
    # makes sense, e.g. C). This could make ``cnkDerefView`` obsolete

    cnkConv          ## a type conversion
    # future direction: introduce a dedicated operation for "l-value preserving
    # conversions"
    cnkHiddenConv
    # future direction: the notion of "hidden" doesn't make any sense in the
    # context of code generation. Adjust the code generators so that they no
    # longer depend on ``cnkHiddenConv`` being different from ``cnkConv``, and
    # then remove the former

    cnkObjDownConv   ## down conversion between `object` or `ref` types
    cnkObjUpConv     ## up conversion between `object` or `ref` types

    cnkCast          ## reinterpret the bit-pattern of the operand as a
                     ## different type

    # ---- special conversions kept for compatibility
    cnkChckRangeF      ## range check for floats
    cnkChckRange64     ## range check for 64-bit ints
    cnkChckRange       ## range check for ints
    cnkStringToCString ## string to cstring
    cnkCStringToString ## cstring to string
    # future direction: lower these coversion operations during the MIR
    # phase and then remove the node kinds
    # ---- end

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
                      cnkStringToCString, cnkCStringToString}
  cnkAtoms*        = {cnkInvalid..cnkMagic, cnkReturnStmt, cnkPragmaStmt}
    ## node kinds that denote leafs
  cnkWithItems*    = AllKinds - cnkWithOperand - cnkAtoms
    ## node kinds for which the ``items`` iterator is available

  cnkLiterals* = {cnkIntLit, cnkUIntLit, cnkFloatLit, cnkStrLit}

type
  CgNode* = ref object
    ## Code-generator node
    origin*: PNode
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
    of cnkSym:        sym*: PSym
    of cnkMagic:      magic*: TMagic
    of cnkPragmaStmt: pragma*: TSpecialWord
    of cnkWithOperand: operand*: CgNode
    of cnkWithItems:
      kids*: seq[CgNode]

  # future direction: move to a single-sequence-based, data-oriented design
  # for the code-generator IR

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