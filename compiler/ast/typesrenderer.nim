#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import
  std/strutils,
  compiler/ast/[
    ast,
    astmsgs,
    renderer,
    types,
  ],
  compiler/front/options

const defaultParamSeparator* = ","

proc typeToString*(typ: PType; prefer: TPreferedDesc = preferName): string

template `$`*(typ: PType): string = typeToString(typ)

proc renderPlainSymbolName*(n: PNode): string =
  ## Returns the first non '*' nkIdent node from the tree.
  ##
  ## Use this on documentation name nodes to extract the *raw* symbol name,
  ## without decorations, parameters, or anything. That can be used as the base
  ## for the HTML hyperlinks.
  case n.kind
  of nkPostfix, nkAccQuoted:
    result = renderPlainSymbolName(n[^1])
  of nkIdent:
    result = n.ident.s
  of nkSym:
    result = n.sym.renderDefinitionName(noQuotes = true)
  of nkPragmaExpr:
    result = renderPlainSymbolName(n[0])
  else:
    result = ""
    #internalError(n.info, "renderPlainSymbolName() with " & $n.kind)

proc renderType(n: PNode): string =
  ## Returns a string with the node type or the empty string.
  case n.kind:
  of nkIdent: result = n.ident.s
  of nkSym: result = typeToString(n.sym.typ)
  of nkVarTy:
    if n.len == 1:
      result = renderType(n[0])
    else:
      result = "var"
  of nkRefTy:
    if n.len == 1:
      result = "ref." & renderType(n[0])
    else:
      result = "ref"
  of nkPtrTy:
    if n.len == 1:
      result = "ptr." & renderType(n[0])
    else:
      result = "ptr"
  of nkProcTy:
    assert n.len != 1
    if n.len > 1:
      let params = n[0]
      assert params.kind == nkFormalParams
      assert params.len > 0
      result = "proc("
      for i in 1..<params.len: result.add(renderType(params[i]) & ',')
      result[^1] = ')'
    else:
      result = "proc"
  of nkIdentDefs:
    assert n.len >= 3
    let typePos = n.len - 2
    let typeStr = renderType(n[typePos])
    result = typeStr
    for i in 1..<typePos:
      assert n[i].kind in {nkSym, nkIdent}
      result.add(',' & typeStr)
  of nkTupleTy:
    result = "tuple["
    for i in 0..<n.len: result.add(renderType(n[i]) & ',')
    result[^1] = ']'
  of nkBracketExpr:
    assert n.len >= 2
    result = renderType(n[0]) & '['
    for i in 1..<n.len: result.add(renderType(n[i]) & ',')
    result[^1] = ']'
  of nkCommand:
    result = renderType(n[0])
    for i in 1..<n.len:
      if i > 1: result.add ", "
      result.add(renderType(n[i]))
  else: result = ""

proc renderParamTypes(found: var seq[string], n: PNode) =
  ## Recursive helper, adds to `found` any types, or keeps diving the AST.
  ##
  ## The normal `doc` generator doesn't include .typ information, so the
  ## function won't render types for parameters with default values. The `doc`
  ## generator does include the information.
  case n.kind
  of nkFormalParams:
    for i in 1..<n.len: renderParamTypes(found, n[i])
  of nkIdentDefs:
    # These are parameter names + type + default value node.
    let typePos = n.len - 2
    assert typePos > 0
    var typeStr = renderType(n[typePos])
    if typeStr.len < 1 and n[typePos+1].kind != nkEmpty:
      # Try with the last node, maybe its a default value.
      let typ = n[typePos+1].typ
      if not typ.isNil: typeStr = typeToString(typ, preferExported)
      if typeStr.len < 1: return
    for i in 0..<typePos:
      found.add(typeStr)
  else:
    found.add($n)
    #internalError(n.info, "renderParamTypes(found,n) with " & $n.kind)

proc renderParamTypes*(n: PNode, sep = defaultParamSeparator): string =
  ## Returns the types contained in `n` joined by `sep`.
  ##
  ## This proc expects to be passed as `n` the parameters of any callable. The
  ## string output is meant for the HTML renderer. If there are no parameters,
  ## the empty string is returned. The parameters will be joined by `sep` but
  ## other characters may appear too, like ``[]`` or ``|``.
  result = ""
  var found: seq[string] = @[]
  renderParamTypes(found, n)
  if found.len > 0:
    result = found.join(sep)

proc addTypeHeader*(result: var string, conf: ConfigRef; typ: PType; prefer: TPreferedDesc = preferMixed; getDeclarationPath = true) =
  ## Calls `typeToString` on given `typ`.
  ## If `getDeclarationPath` is true, it will add the declared location.
  result.add typeToString(typ, prefer)
  if getDeclarationPath: result.addDeclaredLoc(conf, typ.sym)

proc valueToString(a: PNode): string =
  ## Returns `int`, `float`, or `string` literals from the node, otherwise returns `<invalid value>`.
  case a.kind
  of nkCharLit..nkUInt64Lit: result = $a.intVal
  of nkFloatLit..nkFloat128Lit: result = $a.floatVal
  of nkStrLit..nkTripleStrLit: result = a.strVal
  else: result = "<invalid value>"

proc rangeToStr(n: PNode): string =
  ## Encodes a range value for array types.
  assert(n.kind == nkRange)
  result = valueToString(n[0]) & ".." & valueToString(n[1])

const
  preferToResolveSymbols = {preferName, preferTypeName, preferModuleInfo,
  preferGenericArg, preferResolved, preferMixed}
  typeToStr*: array[TTypeKind, string] = ["None", "bool", "char", "empty",
    "Alias", "typeof(nil)", "untyped", "typed", "typeDesc",
    # xxx typeDesc=>typedesc: typedesc is declared as such, and is 10x more common.
    "GenericInvocation", "GenericBody", "GenericInst", "GenericParam",
    "distinct $1", "enum", "ordinal[$1]", "array[$1, $2]", "object", "tuple",
    "set[$1]", "range[$1]", "ptr ", "ref ", "var ", "seq[$1]", "proc",
    "pointer", "OpenArray[$1]", "string", "cstring", "Forward",
    "int", "int8", "int16", "int32", "int64",
    "float", "float32", "float64", "float128",
    "uint", "uint8", "uint16", "uint32", "uint64",
    "sink",
    "lent ", "varargs[$1]", "UncheckedArray[$1]", "Error Type",
    "BuiltInTypeClass", "UserTypeClass",
    "UserTypeClassInst", "CompositeTypeClass", "inferred",
    "and", "or", "not", "any", "static", "TypeFromExpr",
    "void"]

proc addTypeFlags(name: var string, typ: PType) {.inline.} =
  ## Adds " not nil" if tfNotNil is present
  if tfNotNil in typ.flags: name.add(" not nil")

proc typeToString*(typ: PType, prefer: TPreferedDesc = preferName): string =
  ## Formats types to strings
  let preferToplevel = prefer
  proc getPrefer(prefer: TPreferedDesc): TPreferedDesc =
    if preferToplevel in {preferResolved, preferMixed}:
      preferToplevel # sticky option
    else:
      prefer

  proc typeToString(typ: PType, prefer: TPreferedDesc = preferName): string =
    result = ""
    let prefer = getPrefer(prefer)
    let t = typ
    if t == nil: return
    if prefer in preferToResolveSymbols and t.sym != nil and
         sfAnon notin t.sym.flags and t.kind != tySequence:
      if t.kind == tyInt and isIntLit(t):
        result = t.sym.name.s & " literal(" & $t.n.intVal & ")"
      elif t.kind == tyAlias and t[0].kind != tyAlias:
        result = typeToString(t[0])
      elif prefer in {preferResolved, preferMixed}:
        case t.kind
        of IntegralTypes + {tyFloat..tyFloat128} + {tyString, tyCstring}:
          result = typeToStr[t.kind]
        of tyGenericBody:
          result = typeToString(t.lastSon)
        of tyCompositeTypeClass:
          # avoids showing `A[any]` in `proc fun(a: A)` with `A = object[T]`
          result = typeToString(t[0])
        else:
          result = t.sym.name.s
        if prefer == preferMixed and result != t.sym.name.s:
          result = t.sym.name.s & "{" & result & "}"
      elif prefer in {preferName, preferTypeName} or t.sym.owner.isNil:
        # note: should probably be: {preferName, preferTypeName, preferGenericArg}
        result = t.sym.name.s
        if t.kind == tyGenericParam and t.len > 0:
          result.add ": "
          var first = true
          for son in t.sons:
            if not first: result.add " or "
            result.add son.typeToString
            first = false
      else:
        result = t.sym.owner.name.s & '.' & t.sym.name.s
      result.addTypeFlags(t)
      return
    case t.kind
    of tyInt:
      if not isIntLit(t) or prefer == preferExported:
        result = typeToStr[t.kind]
      else:
        if prefer == preferGenericArg:
          result = $t.n.intVal
        else:
          result = "int literal(" & $t.n.intVal & ")"
    of tyGenericInst, tyGenericInvocation:
      result = typeToString(t[0]) & '['
      for i in 1..<t.len-ord(t.kind != tyGenericInvocation):
        if i > 1: result.add(", ")
        result.add(typeToString(t[i], preferGenericArg))
      result.add(']')
    of tyGenericBody:
      result = typeToString(t.lastSon) & '['
      for i in 0..<t.len-1:
        if i > 0: result.add(", ")
        result.add(typeToString(t[i], preferTypeName))
      result.add(']')
    of tyTypeDesc:
      if t[0].kind == tyNone: result = "typedesc"
      else: result = "typedesc[" & typeToString(t[0]) & "]"
    of tyStatic:
      if prefer == preferGenericArg and t.n != nil:
        result = t.n.renderTree
      else:
        result = "static[" & (if t.len > 0: typeToString(t[0]) else: "") & "]"
        if t.n != nil: result.add "(" & renderTree(t.n) & ")"
    of tyUserTypeClass:
      if t.sym != nil and t.sym.owner != nil:
        if t.isResolvedUserTypeClass: return typeToString(t.lastSon)
        return t.sym.owner.name.s
      else:
        result = "<invalid tyUserTypeClass>"
    of tyBuiltInTypeClass:
      result = case t.base.kind
        of tyVar: "var"
        of tyRef: "ref"
        of tyPtr: "ptr"
        of tySequence: "seq"
        of tyArray: "array"
        of tySet: "set"
        of tyRange: "range"
        of tyDistinct: "distinct"
        of tyProc: "proc"
        of tyObject: "object"
        of tyTuple: "tuple"
        of tyOpenArray: "openArray"
        else: typeToStr[t.base.kind]
    of tyInferred:
      let concrete = t.previouslyInferred
      if concrete != nil: result = typeToString(concrete)
      else: result = "inferred[" & typeToString(t.base) & "]"
    of tyUserTypeClassInst:
      let body = t.base
      result = body.sym.name.s & "["
      for i in 1..<t.len - 1:
        if i > 1: result.add(", ")
        result.add(typeToString(t[i]))
      result.add "]"
    of tyAnd:
      for i, son in t.sons:
        result.add(typeToString(son))
        if i < t.sons.high:
          result.add(" and ")
    of tyOr:
      for i, son in t.sons:
        result.add(typeToString(son))
        if i < t.sons.high:
          result.add(" or ")
    of tyNot:
      result = "not " & typeToString(t[0])
    of tyUntyped:
      #internalAssert t.len == 0
      result = "untyped"
    of tyFromExpr:
      if t.n == nil:
        result = "unknown"
      else:
        result = "typeof(" & renderTree(t.n) & ")"
    of tyArray:
      result = "array"
      if t.len > 0:
        if t[0].kind == tyRange:
          result &= "[" & rangeToStr(t[0].n) & ", " &
              typeToString(t[1]) & ']'
        else:
          result &= "[" & typeToString(t[0]) & ", " &
              typeToString(t[1]) & ']'
    of tyUncheckedArray:
      result = "UncheckedArray"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tySequence:
      if t.sym != nil and prefer != preferResolved:
        result = t.sym.name.s
      else:
        result = "seq"
        if t.len > 0:
          result &= "[" & typeToString(t[0]) & ']'
    of tyOrdinal:
      result = "ordinal"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tySet:
      result = "set"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tyOpenArray:
      result = "openArray"
      if t.len > 0:
        result &= "[" & typeToString(t[0]) & ']'
    of tyDistinct:
      result = "distinct " & typeToString(t[0],
        if prefer == preferModuleInfo: preferModuleInfo else: preferTypeName)
    of tyTuple:
      # we iterate over t.sons here, because t.n may be nil
      if t.n != nil:
        result = "tuple["
        assert(t.n.len == t.len)
        for i in 0..<t.n.len:
          assert(t.n[i].kind == nkSym)
          result.add(t.n[i].sym.name.s & ": " & typeToString(t[i]))
          if i < t.n.len - 1: result.add(", ")
        result.add(']')
      elif t.len == 0:
        result = "tuple[]"
      else:
        result = "("
        for i in 0..<t.len:
          result.add(typeToString(t[i]))
          if i < t.len - 1: result.add(", ")
          elif t.len == 1: result.add(",")
        result.add(')')
    of tyPtr, tyRef, tyVar, tyLent:
      result = typeToStr[t.kind]
      if t.len >= 2:
        setLen(result, result.len-1)
        result.add '['
        for i in 0..<t.len:
          result.add(typeToString(t[i]))
          if i < t.len - 1: result.add(", ")
        result.add ']'
      else:
        result.add typeToString(t[0])
    of tyRange:
      result = "range "
      if t.n != nil and t.n.kind == nkRange:
        result.add rangeToStr(t.n)
      if prefer != preferExported:
        result.add("(" & typeToString(t[0]) & ")")
    of tyProc:
      result = if tfIterator in t.flags: "iterator "
               elif t.owner != nil:
                 case t.owner.kind
                 of skTemplate: "template "
                 of skMacro: "macro "
                 of skConverter: "converter "
                 else: "proc "
              else:
                "proc "
      if tfUnresolved in t.flags:
        result.add renderTree(t.owner.ast[genericParamsPos])
      result.add "("
      for i in 1..<t.len:
        if t.n != nil and i < t.n.len and t.n[i].kind == nkSym:
          result.add(t.n[i].sym.name.s)
          result.add(": ")
        result.add(typeToString(t[i]))
        if i < t.len - 1: result.add(", ")
      result.add(')')
      if t.len > 0 and t[0] != nil: result.add(": " & typeToString(t[0]))
      var prag = if t.callConv == ccNimCall and tfExplicitCallConv notin t.flags: "" else: $t.callConv
      if tfNoSideEffect in t.flags:
        addSep(prag)
        prag.add("noSideEffect")
      if tfThread in t.flags:
        addSep(prag)
        prag.add("gcsafe")
      if t.lockLevel.ord != UnspecifiedLockLevel.ord:
        addSep(prag)
        prag.add("locks: " & $t.lockLevel)
      if prag.len != 0: result.add("{." & prag & ".}")
    of tyVarargs:
      result = typeToStr[t.kind] % typeToString(t[0])
    of tySink:
      result = "sink " & typeToString(t[0])
    else:
      result = typeToStr[t.kind]
    result.addTypeFlags(t)
  result = typeToString(typ, prefer)
