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
func matches(s: PSym; x: string): bool =
  let y = x.split('.')
  var s = s
  for i in 1..y.len:
    if s == nil or (y[^i].cmpIgnoreStyle(s.name.s) != 0 and y[^i] != "*"):
      return false
    s = if sfFromGeneric in s.flags: s.owner.owner else: s.owner
    while s != nil and s.kind == skPackage and s.owner != nil: s = s.owner
  result = true

# XXX: this proc was previously located in ``vmgen.nim``
proc procIsCallback*(c: TCtx; s: PSym): bool =
  # XXX: indicating that a sym is a callback by using s.offset < -1 is really confusing
  if s.offset < -1: return true
  var i = -2
  for key, value in items(c.callbacks):
    if s.matches(key):
      doAssert s.offset == -1
      # TODO: instead of modifying the offset here, just return `i`.
      #       The procToFuncObj table makes sure that procIsCallback is not called
      #       for the same symbol twice
      s.offset = i
      return true
    dec i

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
    var f = VmFunctionObject(prc: prc)
    f.kind =
      if procIsCallback(c, prc): f.cbOffset = -prc.offset - 2; ckCallback
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