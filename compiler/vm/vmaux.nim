## This modules contains functions used by both ``vmgen`` and
## ``vmcompilerserdes`` that don't really fit into any other vm module

# TODO: at least some of the function here could be moved into other (non-vm)
#       modules

import
  compiler/ast/[
    ast_types,
    ast,
    types
  ],
  compiler/vm/[
    vmdef,
    vmtypegen
  ],

  std/[
    strutils,
    tables
  ]

# XXX: this proc was previously located in ``vmgen.nim``
func matches(s: PSym; x: IdentPattern): bool =
  var s = s
  for part in rsplit(string(x), '.'):
    if s == nil or (part.cmpIgnoreStyle(s.name.s) != 0 and part != "*"):
      return false
    s = if sfFromGeneric in s.flags: s.owner.owner else: s.owner
    while s != nil and s.kind == skPackage and s.owner != nil: s = s.owner
  result = true

func lookup*(patterns: seq[IdentPattern]; s: PSym): int =
  ## Tries to find and return the index of the pattern matching `s`. If none
  ## is found, -1 is returned
  var i = 0
  # XXX: `pairs` doesn't use `lent`, so a manual implementation of `pairs`
  #      is used
  for p in patterns.items:
    if s.matches(p): return i
    inc i

  result = -1

func findRecCaseAux(n: PNode, d: PSym): PNode =
  ## Find the `nkRecCase` node in the tree `r` that has `d` as the discriminator
  case n.kind
  of nkRecList:
    for x in n.items:
      result = findRecCaseAux(x, d)
      if result != nil:
        return
  of nkRecCase:
    if n[0].sym == d:
      return n
    else:
      for i in 1..<n.len:
        result = findRecCaseAux(n[i].lastSon, d)
        if result != nil: return
  of nkSym:
    discard
  else:
    assert false

# TODO: use `searchObjCase` from ``enumtostr`` instead (it does the same thing)
func findRecCase*(t: PType, d: PSym): PNode =
  ## Find the `nkRecCase` node in `t` or any of it's base types that has `d`
  ## as the discriminator
  result = findRecCaseAux(t.n, d)
  if result == nil and t.sons[0] != nil:
    result = findRecCase(t[0].skipTypes({tyAlias, tyGenericInst, tyRef, tyPtr}), d)

func findMatchingBranch*(recCase: PNode, val: Int128): int =
  # XXX: If Option[Natural] would be stored as a single integer it could be
  #      used as the result type here instead
  assert recCase.kind == nkRecCase

  func overlap(val: Int128, n: PNode): bool =
    if n.kind == nkRange:
      getInt(n[0]) <= val and val <= getInt(n[1])
    else:
      getInt(n) == val

  for i in 1..<recCase.len:
    let branch = recCase[i]
    for j in 0..<branch.len-1: # the last son is the content of the branch
      if overlap(val, branch[j]):
        return i - 1

    if branch.kind == nkElse:
      # The last branch is always the one. Reaching it means that no other
      # branch matched
      return i - 1

  result = -1

# TODO: should likely be moved somewhere else, since it's not strictly related
#       to the VM
func getEnvParam*(prc: PSym): PSym =
  ## Return the symbol the hidden environment parameter, or `nil` if there's
  ## none
  if prc.typ.callConv == ccClosure:
    lastSon(prc.ast[paramsPos]).sym
  else: nil


proc initProcEntry*(c: var TCtx, prc: PSym): FuncTableEntry =
  ## Returns an initialized function table entry. Execution information (such
  ## as the bytecode offset and register count) for procedures not overriden
  ## by callbacks is initialized to a state that indicates "missing" and needs
  ## to be filled in separately via `fillProcEntry`
  let cbIndex = lookup(c.callbackKeys, prc)
  result =
    if cbIndex >= 0:
      FuncTableEntry(kind: ckCallback, cbOffset: cbIndex)
    else:
      FuncTableEntry(kind: ckDefault, start: -1)

  result.sym = prc
  result.sig = c.typeInfoCache.makeSignatureId(prc.typ)
  result.retValDesc =
    if prc.kind == skMacro:
      c.typeInfoCache.nodeType
    else:
      let rTyp = prc.getReturnType()
      if not isEmptyType(rTyp):
        c.getOrCreate(rTyp)
      else:
        noneType

  # Create the env parameter type (if an env param exists)
  result.envParamType =
    if (let envP = getEnvParam(prc); envP != nil):
      c.getOrCreate(envP.typ)
    else:
      noneType

  assert result.envParamType == noneType or result.envParamType.kind == akRef

func fillProcEntry*(e: var FuncTableEntry, info: CodeInfo) {.inline.} =
  ## Sets the execution information of the function table entry to `info`
  e.start = info.start
  e.regCount = info.regCount.uint16

proc registerProc*(c: var TCtx, prc: PSym): FunctionIndex =
  ## Registers the procedure in the function table if it wasn't already. In
  ## both cases, an index into the function table is returned. Whether a
  ## procedure will resolve to a callback is decided on it's addition to the
  ## function table
  let next = LinkIndex(c.functions.len)

  result = c.symToIndexTbl.mgetOrPut(prc.id, next).FunctionIndex
  if result == next.FunctionIndex:
    # a new entry:
    c.functions.add(initProcEntry(c, prc))

proc lookupProc*(c: var TCtx, prc: PSym): FunctionIndex {.inline.} =
  ## Returns the function-table index corresponding to the provided `prc`
  ## symbol. Behaviour is undefined if `prc` has no corresponding function-
  ## table entry.
  c.symToIndexTbl[prc.id].FunctionIndex