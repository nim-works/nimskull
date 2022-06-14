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

from compiler/ast/nimsets import overlap
from compiler/sem/lambdalifting import getEnvParam

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

func findMatchingBranch*(recCase: PNode, lit: PNode): int =
  # XXX: If Option[Natural] would be stored as a single integer it could be
  #      used as the result type here instead
  assert recCase.kind == nkRecCase
  assert lit.kind in nkLiterals

  for i in 1..<recCase.len:
    let branch = recCase[i]
    for j in 0..<branch.len-1: # the last son is the content of the branch
      if overlap(lit, branch[j]):
        return i - 1

    if branch.kind == nkElse:
      # The last branch is always the one. Reaching it means that no other
      # branch matched
      return i - 1

  result = -1


proc getOrCreateFunction*(c: var TCtx, prc: PSym): FunctionIndex =
  ## Looks up the index of the given procs corresponding VM function object.
  ## If one does not exists yet, it is created first. If the function type
  ## description doesn't exist yet it is created too

  if prc.id in c.procToFuncObj:
    # XXX: double lookup
    result = c.procToFuncObj[prc.id]
  else:
    let cbIndex = lookup(c.callbackKeys, prc)
    var f = VmFunctionObject(prc: prc)
    f.kind =
      if cbIndex != -1: f.cbOffset = cbIndex; ckCallback
      else: ckDefault

    f.sig = c.typeInfoCache.makeSignatureId(prc.typ)
    let rTyp = prc.getReturnType()
    if rTyp != nil and rTyp.kind != tyVoid:
      f.retValDesc = c.getOrCreate(rTyp)
    else:
      f.retValDesc = noneType

    # Create the env parameter type (if an env param exists)
    f.envParamType =
      if (let envP = prc.getEnvParam(); envP != nil):
        c.getOrCreate(envP.typ)
      else:
        noneType

    assert f.envParamType == noneType or f.envParamType.kind == akRef

    result = FunctionIndex(c.functions.len)
    c.functions.add(f)

    c.procToFuncObj[prc.id] = result