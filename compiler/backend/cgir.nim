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


  CgNode* = object
    ## Code-generator node. Each node
    info*: TLineInfo
    typ*: PType
    case kind*: CgNodeKind
    of cgnkInvalid, cgnkEmpty, cgnkType, cgnkNilLit: discard
    of cgnkLiteralData:
      data*: NimNode
    of cgnkIntLit:
      intVal*: BiggestInt
    of cgnkNimNodeLit:
      nimNodeLit*: PNode
    of cgnkMagic:
      magic*: TMagic
    of cgnkLabel:
      lbl: LabelId
    else:
      childs: seq[CgNode]

func condition*(n: CgNode): CgNode =
  assert n.kind in {cgnkIf, cgnkCase}
  n.childs[0]

func body*(n: CgNode): CgNode =
  assert n.kind in {cgnkIf, cgnkBlock, cgnkRepeat}
  n.childs[^1]

func callee*(n: CgNode): CgNode =
  assert n.kind == cgnkCall
  n.childs[0]

func label*(n: CgNode): LabelId =
  assert n.kind in {cgnkBlock, cgnkBreak}
  n.childs[0].lbl

func arg*(n: CgNode, i: Natural): CgNode =
  n.childs[i + 1]