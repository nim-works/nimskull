## Implements the intermediate representation the C code generator outputs, as
## well as routines for producing and querying it.
##
## The IR is an abstract syntax tree (=AST) representing pre-preprocessor C
## code. For the convenience of the code generator, the tree doesn't encode
## only syntax, but also includes some symbol information. Handling all
## complexities of C's syntax is explicitly a non-goal. Where reasonable,
## simplification are made.

import
  compiler/mir/[
    mirtrees
  ]

import compiler/utils/measure

type
  CNodeKind* = enum
    cnkIdent     ## raw identifier
    cnkProcSym   ## procedure symbol
    cnkGlobalSym ## global location symbol
    cnkConstSym  ## constant location symbol
    cnkType      ## strong type reference
    cnkWeakType  ## weak type reference

    cnkCharLit
    # C has no signed integer literals, but for convenience, we do
    cnkIntLit
    cnkUIntLit
    cnkFloatLit
    cnkDoubleLit
    cnkStrLit
    cnkVerbatim  ## string that's interpreted as raw piece of C code

    cnkExpr ## local reference to an expression AST

    # ------------ end of atoms

    # expressions:
    cnkAddrOf    ## (operand: expr)
    cnkDeref     ## (operand: expr)
    cnkMember    ## (of: expr, member: ident)
    cnkPtrMember ## (of: expr, member: ident)
    cnkArrMember ## (of: expr, index: expr)
    cnkCast      ## (typ: typeexpr, opr: expr)
    # XXX: it's likely easier/better to use dedicated node kinds for all
    #      needed operators
    cnkPrefix    ## (name: ident, a: expr)
    cnkInfix     ## (name: ident, a: expr, b: expr)
    cnkPostfix   ## (name: ident, a: expr)

    cnkCall      ## (callee: expr, args...: expr)
    cnkTernary   ## (cond: expr, a: expr, b: expr)
    # an assignment is an expression in C
    cnkAsgn      ## (lhs: expr, rhs: expr)
    cnkBraced    ## (items...: expr)

    # statements
    cnkStmt    ## (e: expr)
    cnkGoto    ## (label: ident)
    cnkLabel   ## (label: ident)
    cnkBlock   ## (body+: stmt)
    cnkIf      ## (cond: expr, body: stmt)
    cnkWhile   ## (cond: expr, body: stmt)
    cnkReturn  ## (operand?: expr)
    cnkSwitch  ## (selector: expr, branch+:case|default)
    cnkCase    ## (value: expr, body: stmt)
    cnkDefault ## (body: stmt)
    # TODO: an asm statement is missing

    # declaration grammar:
    # doesn't cover everything that C supports, and also has to support macros
    cnkSpecList    ## (spec+: spec)
    cnkDeclaration ## (spec: spec, decl: declarator, init?: expr)
    cnkDefinition  ## (spec: spec, decl: declarator, body: block)
    cnkParamDecl   ## (spec: spec, decl: declarator)

    cnkArrayDecl   ## (name: ident, len?: expr)
    cnkPtrDecl     ## (name: ident)
    cnkFuncDecl    ## (name: ident, params...: decl)
    cnkDeclList    ## (decl...: declaration)

    cnkStructSpec  ## (attr?: expr, name?: ident, body: decllist)
    cnkUnionSpec   ## (attr?: expr, name?: ident, body: decllist)

    # directives:
    cnkEmit  ## emit(args...: expr|stmt|verbatim)

const
  cnkAtoms      = {cnkIdent .. cnkExpr}
  cnkWithNodes  = {low(CNodeKind) .. high(CNodeKind)} - cnkAtoms

  cnkWithNumber = {cnkIntLit, cnkUIntLit, cnkFloatLit, cnkDoubleLit}
  cnkWithString = {cnkStrLit, cnkVerbatim}
  cnkWithType   = {cnkWeakType, cnkType}

type
  CNodeIndex* = distinct uint32
  CIdentifier* = distinct uint32

  CNode* = object
    ## Node in a flat tree structure. A node is either atomic or not. Atoms
    ## have no children nodes. Nodes are layed out in depth first fashion.
    case kind*: CNodeKind
    of cnkIdent:      ident*: CIdentifier
    of cnkProcSym:    prc*: ProcedureId
    of cnkGlobalSym:  global*: GlobalId
    of cnkConstSym:   cnst*: ConstId
    of cnkWithType:   typ*: TypeId
    of cnkWithString: strId*: StringId
    of cnkCharLit:    charVal*: char
    of cnkWithNumber: number*: NumberId
    of cnkExpr:       node*: CNodeIndex
    of cnkWithNodes:  len*: uint32

  BufferType = enum
    btExpr
    btStmt

  CAst* = object
    ## In-progress AST. Non-atomic expressions are stored in a separate buffer
    ## from statements.
    buf: array[BufferType, seq[CNode]]

  CombinedCAst* = seq[CNode]
    ## Finalized AST where expressions and statement are combined.

func `==`*(a, b: CNodeIndex): bool {.borrow.}
func `<`*(a, b: CNodeIndex): bool {.borrow.}

iterator all*(ast: CombinedCAst, start: CNodeIndex): CNode =
  ## Returns all nodes in the tree starting at `start`.
  var i = uint32(start)
  var last = i
  while i <= last:
    let n = ast[i]
    yield n
    if ord(n.kind) > ord(cnkExpr): # not an atom?
      last += n.len
    inc i

proc append*(a: var CombinedCAst, b: sink CAst): CNodeIndex =
  ## Combines the expressions and statements of `b` into a single AST and
  ## appends the result to `a`.
  measure("append")
  let off = a.len.uint32
  for it in b.buf[btExpr].mitems:
    if it.kind == cnkExpr:
      uint32(it.node) += off
  for it in b.buf[btStmt].mitems:
    if it.kind == cnkExpr:
      uint32(it.node) += off

  a.add b.buf[btExpr]
  result = a.len.CNodeIndex
  a.add b.buf[btStmt]

proc add*(ast: var CAst, kind: CNodeKind) =
  # XXX: temporary procedure
  ast.buf[btStmt].add(CNode(kind: kind))
