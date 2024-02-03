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
  compiler/front/[
    options
  ],
  compiler/vm/[
    vmdef,
    vmlinker,
    vmtypegen
  ]

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


proc initProcEntry*(keys: Patterns, config: ConfigRef,
                    tc: var TypeInfoCache, prc: PSym): FuncTableEntry =
  ## Returns an initialized function table entry. Execution information (such
  ## as the bytecode offset and register count) for procedures not overriden
  ## by callbacks is initialized to a state that indicates "missing"; it needs
  ## to be filled in separately via `fillProcEntry`.
  let cbIndex = lookup(keys, prc)
  result =
    if cbIndex >= 0:
      FuncTableEntry(kind: ckCallback, cbOffset: cbIndex)
    else:
      FuncTableEntry(kind: ckDefault, start: -1)

  var cl = GenClosure() # for creating the VM type structures

  result.sym = prc
  result.sig = tc.makeSignatureId(prc.typ)
  result.retValDesc =
    if prc.kind == skMacro:
      tc.nodeType
    else:
      let rTyp = prc.getReturnType()
      if not isEmptyType(rTyp):
        tc.getOrCreate(config, rTyp, false, cl)
      else:
        noneType

  result.isClosure = prc.typ.callConv == ccClosure

proc initProcEntry*(c: var TCtx, prc: PSym): FuncTableEntry {.inline.} =
  ## Convenience wrapper around
  ## `initProcEntry <#initProcEntry,Patterns,ConfigRef,TypeInfoCache,PSym>`_.
  initProcEntry(c.callbackKeys, c.config, c.typeInfoCache, prc)

func fillProcEntry*(e: var FuncTableEntry, info: CodeInfo) {.inline.} =
  ## Sets the execution information of the function table entry to `info`
  e.start = info.start
  e.regCount = info.regCount.uint16
