## This module provides a list of type definitions and convenience
## procedures for dealing with /entities/ declared in the code - objects,
## enums, procedures. It provides additional layer of abstraction on top of
## the AST, allowing user to work in terms `"if obj.exported"` as opposed
## to `if obj[0].kind == nnkPrefix`. Recursively iterate fields,
## definitions.

import
  ast/[
    ast,
    renderer
  ],
  utils/[
    astrepr
  ],
  std/[
    options
  ],
  ./docgen_file_tracking,
  ./docgen_ast_aux

type
  DefTreeKind* = enum
    ## Definition tree kind
    deftProc ## Procedure-like element
    deftObject ## Object definitio
    deftEnum
    deftArg ## Procedure argument
    deftLet ## 'let' variable declaration
    deftVar ## 'var' declaration
    deftMagic ## Declaration of the magic type
    deftConst ## 'const' declaration
    deftField ## Object field declaration
    deftEnumField ## Enum field declaration
    deftAlias ## Type alias declaration
    deftDistinct ## `distinct` declaration

  DefFieldKind* = enum
    ## Kind of the object declaration
    deffPlain
    deffWrapCase
    deffWrapWhen

const deftIdentKinds* = { deftArg, deftLet, deftVar, deftConst, deftField }

type
  DefName* = object
    ## Unparsed definition name, split into semantically meaningfuly chunks
    ## that can be queried directly.
    ident*: PNode ## Name identifier - symbol/ident/accquoted node
    exported*: bool ## Whether or not original node was exported
    pragmas*: seq[PNode] ## list of name pragmas

  DefFieldBranch* = object
    branchExprs*: seq[PNode]
    subfields*: seq[DefField]


  DefField* = ref object
    head*: DefTree
    case kind*: DefFieldKind
      of deffWrapCase, deffWrapWhen:
        branches*: seq[DefFieldBranch]

      else:
        discard

  DefTree* = ref object
    defName*: DefName
    docs*: seq[PNode] ## Documentation comment for declaration - contains
    ## either nodes that had `.comment` attached, or comment
    ## statements/runnable entries
    defNode: PNode
    genericParams*: seq[tuple[
      name: PNode, constraint: Option[PNode]]]

    case kind*: DefTreeKind
      of deftProc:
        arguments*: seq[DefTree]
        returnType*: PNode

      of deftIdentKinds:
        typ*: PNode
        initExpr*: PNode
        initExprIdx*: Option[int]

      of deftAlias, deftDistinct:
        baseType*: PNode

      of deftEnum:
        enumFields*: seq[DefTree]

      of deftEnumField:
        strOverride*: PNode
        valOverride*: PNode

      of deftObject:
        objFields*: seq[DefField]
        objBase*: Option[PNode]

      of deftMagic:
        discard

func exported*(def: DefTree): bool = def.defName.exported
func exported*(def: DefField): bool = def.head.exported
func name*(def: DefTree): DefName = def.defName

func nameNode*(def: DefName): PNode = def.ident
func nameNode*(def: DefTree): PNode = def.name.nameNode()

func sym*(def: DefName | DefTree): PSym = def.nameNode().sym

func hasSym*(def: DefTree | DefName): bool =
  ## Check if unparsed definition tree or name has symbol in it's name
  ## (`nkSym`)
  def.nameNode.kind == nkSym


func filterPragmas*(pragmas: seq[PNode], names: seq[string]): seq[PNode] =
  ## Filter out list of pragma nodes, returning only ones whose names
  ## were in the `names` list.
  for pragma in pragmas:
    if pragma.safeLen > 0:
      case pragma[0].kind:
        of nkSym, nkIdent:
          if pragma[0].getSName() in names:
            result.add pragma

        of nkCall, nkCommand, nkExprColonExpr:
          if pragma[0][0].getSName() in names:
            result.add pragma

        else:
          assert false, $pragma[0].kind


func getSName*(def: DefName | DefTree): string =
  ## Get string name of definition tree or definition name
  def.nameNode().getSName()

func pragmas*(def: DefTree): seq[PNode] = def.name.pragmas
func node*(def: DefTree): PNode = def.defNode


func allFields*(def: DefTree): seq[DefField] =
  func aux(def: DefField, res: var seq[DefField]) =
    if def.kind in {deffWrapCase, deffWrapWhen}:
      for branch in def.branches:
        for field in branch.subfields:
          aux(field, res)

  for field in def.objFields:
    result.add field
    aux(field, result)

func newDef*(kind: DefTreeKind, name: DefName, node: PNode): DefTree =
  DefTree(kind: kind, defName: name, defNode: node)

proc unparseName*(node: PNode): DefName =
  ## Unparse name definition ast into `DefName`. Input node can be be a
  ## `PragmaExpr`, `Postfix` or `Ident/Sym/AccQuoted` node.
  case node.kind:
    of nkPragmaExpr:
      case node[0].kind:
        of nkPostfix:
          result.ident = node[0][1]
          result.exported = true

        of nkIdent, nkAccQuoted:
          # NOTE not entirely sure about `AccQuoted` here, but technically
          # it /is/ name - no need to reassemble the identifier from pices
          # here.
          result.ident = node[0]

        of nkSym:
          result.ident = node[0]
          result.exported = sfExported in node[0].sym.flags

        else:
          failNode node

      for pragma in node[1]:
        result.pragmas.add pragma

    of nkPostfix:
      result.ident = node[1]
      result.exported = true

    of nkIdent, nkAccQuoted:
      result.ident = node

    of nkSym:
      result.ident = node
      result.exported = sfExported in node.sym.flags

    else:
      failNode node

proc skipNodes*(t: PNode, kinds: set[TNodeKind]): PNode =
  result = t
  while result.kind in kinds:
    result = lastSon(result)

proc unparseIdentDefs(node: PNode): seq[DefTree] =
  let expr = node[^1]
  let typ = node[^2]
  for idx, name in node[0..^3]:
    var field = newDef(deftField, unparseName(name), name)
    field.typ = typ
    field.initExpr = expr
    if 0 < len(node.comment):
      # `0 < len` check is used to don't add nodes with empty comments
      if node.inFile("tfile"):
        if idx == node.len - 3:
          # QUESTION putting documentation comment only into the last
          # field, assuming documentation for 'group of fields' would
          # not be used this way, because it means that all target
          # fields must have the same type, which is a major block.
          field.docs = @[node]

    result.add field


proc unparseFields*(node: PNode): seq[DefField] =
  proc auxBranches(node: PNode): seq[DefFieldBranch]
  proc auxBranch(branch: PNode): DefFieldBranch
  proc auxFields(node: PNode): seq[DefField] =
    case node.kind:
      of nkIdentDefs:
        for field in unparseIdentDefs(node):
          result.add DefField(head: field)

      of nkSym:
        # Because typed AST for `std/options.Option` looks like this, we
        # get random freestanding `sym` nodes in the tree.
        #
        # RecList
        # 0 RecWhen
        #   0 ElifBranch
        #     0 Infix
        #       0 Sym "is" sk:Proc
        #       1 Sym "T" sk:Type
        #       2 Type
        #     1 Sym "val" sk:Field
        #   1 Else
        #     0 RecList
        #       0 Sym "val" sk:Field
        #       1 Sym "has" sk:Field
        # Sym "val" sk:Field
        result.add DefField(head: newDef(deftField, unparseName(node), node))

      of nkRecWhen:
        result.add DefField(kind: deffWrapWhen, branches: auxBranches(node))

      of nkRecCase:
        var field = DefField(
          kind: deffWrapCase,
          head: unparseIdentDefs(node[0])[0],
        )

        for branch in node[SliceAllBranches]:
          field.branches.add auxBranch(branch)

        result.add field

      of nkRecList:
        for sub in node:
          result.add auxFields(sub)

      of nkEmpty, nkNilLit:
        discard

      else:
        failNode node

  proc auxBranch(branch: PNode): DefFieldBranch =
    case branch.kind:
      of nkElifBranch:
        return DefFieldBranch(
          branchExprs: @[branch[0]],
          subfields: auxFields(branch[1])
        )

      of nkElse:
        return DefFieldBranch(subfields: auxFields(branch[0]))

      of nkOfBranch:
        return DefFieldBranch(
          branchExprs: branch[0..^2],
          subfields: auxFields(branch[^1])
        )

      else:
        failNode branch

  proc auxBranches(node: PNode): seq[DefFieldBranch] =
    for branch in node:
      result.add branch.auxBranch()

  return auxFields(node)


proc unparseDefs*(node: PNode): seq[DefTree]

proc unparseGenericParams*(node: PNode): seq[tuple[
  name: PNode, constraint: Option[PNode]]] =

  assert node.kind in {nkGenericParams, nkEmpty}, $treeRepr(nil, node)
  for param in node:
    assert param.kind in {nkSym, nkIdent, nkIdentDefs}, $treeRepr(nil, param)
    if param.kind == nkIdentDefs:
      for name in param[SliceAllIdents]:
        result.add((name, some param[PosType]))

    else:
      result.add((param, none PNode))

proc whichTypedefKind*(node: PNode): DefTreeKind =
  let node =
    if node.kind in {nkTypeDef}:
      node[2].skipNodes({nkPtrTy, nkRefTy})

    else:
      node

  case node.kind:
    of nkObjectTy: result = deftObject
    of nkEnumTy: result = deftEnum
    of nkDistinctTy: result = deftDistinct
    of nkInfix, # `A = B | C`
       nkProcTy, # `A = proc()`
       nkSym, # `A = B`
       nkIdent, # `A = B`
       nkBracketExpr, # `A = B[C]`
       nkTupleTy, # `A = tuple[]`
       nkCall, # `A = typeof()`
       nkVarTy, # `A = var B`
       nkDotExpr: # `A = module.B`,
      result = deftAlias

    of nkEmpty:
      result = deftMagic

    else:
      failNode node

proc isDocumentationNode*(n: PNode): bool =
  ## Check if node is a 'documentation' - either `CommentStmt` or
  ## `runnableExample` call.
  return n.kind == nkCommentStmt or (
    n.kind in nkCallKinds and
    n[0].kind in {nkSym, nkIdent} and
    n[0].getSName() == "runnableExamples"
  )

proc getRecComment*(n: PNode): seq[PNode] =
  ## Recursively travese through the procedure documentation, adding
  ## comment statements and runnable examples to a list.

  proc aux(n: PNode): seq[PNode] =
    if n.isNil():
      return

    elif isDocumentationNode(n):
      # adding runnable examples or documentation comments to the list
      result.add n

    elif n.kind in {
      nkStmtList, nkStmtListExpr, nkTypeDef, nkConstDef,
      nkObjectTy, nkRefTy, nkPtrTy, nkAsgn, nkFastAsgn, nkHiddenStdConv
    }:
      for i in 0..<n.len:
        result.add aux(n[i])

  if n.kind in {
    nkProcDef, nkFuncDef, nkMethodDef, nkIteratorDef,
    nkMacroDef, nkTemplateDef, nkConverterDef
  }:
    result = aux(n[bodyPos])

  else:
    result = aux(n)

proc unparseType*(node: PNode): DefTree =
  let body = node[2].skipNodes({nkPtrTy, nkRefTy})
  case body.whichTypedefKind():
    of deftObject:
      result = newDef(deftObject, unparseName(node[0]), node)
      if 0 < len(body[2].comment):
        # Documentation comments for objects are placed in the `RecList`
        # item.
        result.docs.add @[body[2]]

      result.objFields = unparseFields(body[2])

      if body[1].kind == nkOfInherit:
        result.objBase = some body[1][0]

    of deftEnum:
      result = newDef(deftEnum, unparseName(node[0]), node)
      if 0 < len(body.comment): result.docs = @[node]
      for field in body[1..^1]:
        if field.kind in {nkIdent, nkPrefix}:
          result.enumFields.add newDef(
            deftEnumField, unparseName(field), field)

        else:
          result.enumFields.add unparseDefs(field)

        if 0 < len(field.comment):
          result.enumFields[^1].docs = @[field]

    of deftAlias:
      result = newDef(deftAlias, unparseName(node[0]), node)
      result.baseType = body
      if 0 < len(node.comment):
        result.docs = @[node]

    of deftDistinct:
      result = newDef(deftDistinct, unparseName(node[0]), node)
      result.baseType = body[0]
      if 0 < len(node.comment):
        result.docs = @[node]

    of deftMagic:
      assert node[0].kind in {nkPragmaExpr}, $treeRepr(nil, node)
      result = newDef(deftMagic, unparseName(node[0]), node)
      if 0 < len(node.comment):
        result.docs = @[node]

    else:
      failNode node

  result.genericParams = unparseGenericParams(node[1])


proc unparseProcDef*(node: PNode): DefTree =
  assert node.kind in nkProcDeclKinds, $node.kind
  result = newDef(deftProc, unparseName(node[0]), node)
  if node[pragmasPos].kind == nkPragma:
    for arg in node[pragmasPos]:
      result.defName.pragmas.add arg

  result.returnType = node[paramsPos][0]
  result.genericParams = unparseGenericParams(node[genericParamsPos])

  for arg in node[paramsPos][1..^1]:
    result.arguments.add unparseIdentDefs(arg)

  result.docs = getRecComment(node)

proc unparseDefs*(node: PNode): seq[DefTree] =
  case node.kind:
    of nkStmtList, nkTypeSection:
      for sub in node:
        result.add unparseDefs(node)

    of nkEnumFieldDef:
      var res = newDef(deftEnumField, unparseName(node[0]), node)
      case node[1].kind:
        of nkIntLit:
          res.valOverride = node[1]

        of nkTupleConstr:
          res.valOverride = node[1][0]
          res.strOverride = node[1][1]

        of nkStrLit:
          res.strOverride = node[1]

        else:
          failNode node[1]

      result.add res

    of nkTypeDef:
      result.add unparseType(node)

    of nkConstDef, nkIdentDefs:
      result = unparseIdentDefs(node)

    of nkVarTuple:
      result = unparseIdentDefs(node)
      for idx, _ in result:
        result[idx].initExprIdx = some idx

    of nkProcDeclKinds:
      result.add unparseProcDef(node)

    else:
      failNode node
