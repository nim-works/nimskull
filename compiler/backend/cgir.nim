## This module contains an IR of program code for use by the code-generators.
## It's semantics are derived from the MIR's, but it uses a ``ref``-based tree
## structure, which is meant to make it easier to transition over the
## code generators.
##
## The plan is to, at first, replace the usage of ``PNode`` AST in the code
## generators. Once that is done, the next step is to incrementally simplify
## and evolve the IR, propagating design improvement back into the MIR (and
## vice versa).

## Mapping ``TNodeKind`` to ``CgNodeKind``:
## - nkNone -> cgnkInvalid
## TODO: continue

import compiler/ast/[ast_types, lineinfos]
import compiler/mir/mirtrees

type
  CgNodeKind* = enum
    cgnkInvalid ## uninitialized node kind. Used to catch issues

    cgnkEmpty   ## represents the absence of a node. The meaning depend on the
                ## context
    cgnkType    ## a literal type

    cgnkIntLit            ## an embedded integer value; for internal use only -- doesn't represent a literal as used in the code
    cgnkLiteralData       ## embedded data
    cgnkNilLit              ## the nil literal
    cgnkNimNodeLit        ## a ``NimNode`` literal. Stores a single sub node
                          ## that represents the ``NimNode`` AST
                          ## end of atoms

    cgnkTemp
    cgnkProc, cgnkLocal, cgnkGlobal, cgnkConst, cgnkField, cgnkParam
    cgnkMagic,
    cgnkLabel

    cgnkCall              ## a procedure or magic call
    cgnkArgTuple          ## an argument tuple. Corresponds to ``mnkArgBlock``

    # constructors:
    cgnkTupleConstr         ## tuple constructor
    cgnkObjConstr           ## object constructor
    cgnkSetConstr           ## set constructor
    cgnkArrayConstr         ## array constructor
    cgnkClosureConstr       ## closure constructor

    cgnkRange # for compatibility

    cgnkGoto  ##
    cgnkJoin  ## the destination of one or more `goto`s

    cgnkMap

    #
    cgnkEmit
    cgnkAsm

    # access:
    cgnkObjAccess
    cgnkTupleAccess
    cgnkArrayAccess
    cgnkVariantAccess

    cgnkDeref
    cgnkDerefView
    cgnkAddr
    cgnkView              ## take a view of the input. Has the same meaning as ``mnkView``

    cgnkStmtList
    cgnkStmtListExpr
    cgnkScope

    cgnkVoidStmt

    cgnkIf
    cgnkRepeat
    cgnkCase
    cgnkBlock

    cgnkTryStmt
    cgnkExcept
    cgnkFinally

    cgnkBranch

    cgnkBreak
    cgnkRaise
    cgnkReturn

    cgnkConv                ## a type conversion
    cgnkLConv               ## a type conversion that is l-value preserving

    cgnkObjDownConv         ## down conversion between `object` or `ref` types.
    cgnkObjUpConv           ## up conversion between `object` or `ref` types.

    cgnkCast                ## a type cast

    cgnkAsgn              ## a = b
    cgnkBlitCopy
    cgnkSwitch            ## change the active branch of a discriminated union

    cgnkDef

    # conversions kept for compatibility:
    cgnkChckRangeF         ## range check for floats
    cgnkChckRange64        ## range check for 64 bit ints
    cgnkChckRange          ## range check for ints
    cgnkStringToCString   ## string to cstring
    cgnkCStringToString   ## cstring to string
    # TODO: the above operation should never reach the code-generators.
    #       Instead, they need to be lowered via a MIR pass



  CgNode* = ref object
    ## Code-generator node. Each node
    origin*: PNode
    typ*: PType
    case kind*: CgNodeKind
    of cgnkInvalid, cgnkEmpty, cgnkType, cgnkNilLit: discard
    of cgnkField, cgnkProc:
      sym*: PSym
    of cgnkTemp:
      temp*: TempId
    of cgnkLiteralData:
      data*: PNode
    of cgnkIntLit:
      intVal*: BiggestInt
    of cgnkNimNodeLit:
      nimNodeLit*: PNode
    of cgnkMagic:
      magic*: TMagic
    of cgnkLabel:
      lbl*: LabelId
    else:
      childs*: seq[CgNode]

# TODO: use ``distinct`` types to reduce the amount of dynamic typing with
#       ``CgNode``. ``CgNode`` is only the data representation.

func condition*(n: CgNode): CgNode =
  assert n.kind in {cgnkIf, cgnkCase}
  n.childs[0]

func body*(n: CgNode): CgNode =
  assert n.kind in {cgnkIf, cgnkBlock, cgnkRepeat}
  n.childs[^1]

func handler*(n: CgNode): CgNode =
  assert n.kind == cgnkTryStmt
  if n.childs.len > 1 and n.childs[1].kind == cgnkExcept:
    result = n.childs[1]

func finalizer*(n: CgNode): CgNode =
  assert n.kind == cgnkTryStmt
  if n.childs[^1].kind == cgnkFinally:
    result = n.childs[^1]

func value*(n: CgNode): CgNode =
  assert n.kind == cgnkRaise
  n.childs[0]


func lhs*(n: CgNode): CgNode =
  assert n.kind == cgnkRaise
  n.childs[0]

func rhs*(n: CgNode): CgNode =
  assert n.kind == cgnkRaise
  n.childs[0]

func index*(n: CgNode): CgNode =
  assert n.kind == cgnkArrayAccess
  n.childs[1]

func entity*(n: CgNode): CgNode =
  assert n.kind == cgnkDef
  n.childs[0]

func callee*(n: CgNode): CgNode =
  assert n.kind == cgnkCall
  n.childs[0]

func label*(n: CgNode): LabelId =
  assert n.kind in {cgnkBlock, cgnkBreak}
  n.childs[0].lbl

func arg*(n: CgNode, i: Natural): CgNode =
  n.childs[i + 1]

func numArgs*(n: CgNode): int =
  assert n.kind == cgnkCall
  n.childs.len - 1

func labelAt*(n: CgNode, i: Natural): CgNode =
  n.childs[i]

iterator arguments*(n: CgNode): (int, CgNode) =
  for i in 1..<n.childs.len:
    yield (i-1, n.childs[i])

iterator labels*(n: CgNode): (int, CgNode) =
  for i in 0..<n.childs.len - 1:
    yield (i, n.childs[i])

iterator stmts*(n: CgNode): CgNode =
  for it in n.childs.items:
    yield it

iterator branches*(n: CgNode): (int, CgNode) =
  let start = ord(n.kind == cgnkCase)
  for i in start..<n.childs.len:
    yield (i-start, n.childs[i])

func numLabels*(n: CgNode): int =
  assert n.kind == cgnkBranch
  n.childs.len - 1

func numBranches*(n: CgNode): int =
  assert n.kind in {cgnkExcept, cgnkCase}
  n.childs.len - ord(n.kind == cgnkCase)

func source*(n: CgNode): CgNode =
  assert n.kind in {cgnkArrayAccess, cgnkObjAccess, cgnkTupleAccess}
  n.childs[0]

func fieldIndex*(n: CgNode): int =
  assert n.kind == cgnkTupleAccess
  n.childs[1].intVal.int

func valid*(n: CgNode): CgNode =
  assert n.kind in {cgnkArrayAccess, cgnkObjAccess, cgnkTupleAccess}
  n.childs[1]

func discriminant*(n: CgNode): PSym =
  n.childs[2].sym

func checkExpr*(n: CgNode): CgNode =
  n.childs[3]

func field*(n: CgNode): PSym =
  n.childs[1].sym

func a*(n: CgNode): CgNode =
  n.childs[0]

func b*(n: CgNode): CgNode =
  n.childs[1]

func info*(n: CgNode): TLineInfo {.inline.} =
  if n.origin != nil:
    n.origin.info
  else:
    unknownLineInfo