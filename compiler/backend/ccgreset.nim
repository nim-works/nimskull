#
#
#           The Nim Compiler
#        (c) Copyright 2020 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# included from cgen.nim

## Code specialization instead of the old, incredibly slow 'genericReset'
## implementation.

proc genCaseRange(p: BProc, branch: PNode|CgNode)
proc getTemp(p: BProc, t: PType, result: var TLoc; needsInit=false)

proc specializeResetT(p: BProc, accessor: Rope, typ: PType)

proc parentObj(accessor: Rope): Rope {.inline.} =
  result = "$1.Sup" % [accessor]

proc specializeResetN(p: BProc, accessor: Rope, n: PNode;
                     typ: PType) =
  if n == nil: return
  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      specializeResetN(p, accessor, n[i], typ)
  of nkRecCase:
    p.config.internalAssert(n[0].kind == nkSym, n.info, "specializeResetN")
    let disc = n[0].sym
    ensureObjectFields(p.module, disc, typ)
    lineF(p, cpsStmts, "switch ($1.$2) {$n", [accessor, p.fieldName(disc)])
    for i in 1..<n.len:
      let branch = n[i]
      assert branch.kind in {nkOfBranch, nkElse}
      if branch.kind == nkOfBranch:
        genCaseRange(p, branch)
      else:
        lineF(p, cpsStmts, "default:$n", [])
      specializeResetN(p, accessor, lastSon(branch), typ)
      lineF(p, cpsStmts, "break;$n", [])
    lineF(p, cpsStmts, "} $n", [])
    specializeResetT(p, "$1.$2" % [accessor, p.fieldName(disc)], disc.typ)
  of nkSym:
    let field = n.sym
    if field.typ.kind == tyVoid: return
    ensureObjectFields(p.module, field, typ)
    specializeResetT(p, "$1.$2" % [accessor, p.fieldName(field)], field.typ)
  else: internalError(p.config, n.info, "specializeResetN()")

proc specializeResetT(p: BProc, accessor: Rope, typ: PType) =
  if typ == nil: return

  case typ.kind
  of tyGenericInst, tyGenericBody, tyTypeDesc, tyAlias, tyDistinct, tyInferred,
     tySink:
    specializeResetT(p, accessor, lastSon(typ))
  of tyArray:
    let arraySize = lengthOrd(p.config, typ[0])
    var i: TLoc
    getTemp(p, getSysType(p.module.g.graph, unknownLineInfo, tyInt), i)
    linefmt(p, cpsStmts, "for ($1 = 0; $1 < $2; $1++) {$n",
            [i.r, arraySize])
    specializeResetT(p, ropecg(p.module, "$1[$2]", [accessor, i.r]), typ[1])
    lineF(p, cpsStmts, "}$n", [])
  of tyObject:
    for i in 0..<typ.len:
      var x = typ[i]
      if x != nil: x = x.skipTypes(skipPtrs)
      specializeResetT(p, parentObj(accessor), x)
    if typ.n != nil: specializeResetN(p, accessor, typ.n, typ)
  of tyTuple:
    let typ = getUniqueType(typ)
    for i in 0..<typ.len:
      specializeResetT(p, ropecg(p.module, "$1.Field$2", [accessor, i]), typ[i])

  of tyRef:
    lineCg(p, cpsStmts, "$1 = NIM_NIL;$n", [accessor])
  of tyString, tySequence:
    lineCg(p, cpsStmts, "$1.len = 0;$n", [accessor])
    lineCg(p, cpsStmts, "$1.data = NIM_NIL;$n", [accessor])
  of tyProc:
    if typ.callConv == ccClosure:
      lineCg(p, cpsStmts, "$1.ClE_0 = NIM_NIL;$n", [accessor])
      lineCg(p, cpsStmts, "$1.ClP_0 = NIM_NIL;$n", [accessor])
    else:
      lineCg(p, cpsStmts, "$1 = NIM_NIL;$n", [accessor])
  of tyChar, tyBool, tyEnum, tyInt..tyUInt64:
    lineCg(p, cpsStmts, "$1 = 0;$n", [accessor])
  of tyCstring, tyPointer, tyPtr, tyVar, tyLent:
    lineCg(p, cpsStmts, "$1 = NIM_NIL;$n", [accessor])
  else:
    discard

proc specializeReset(p: BProc, a: TLoc) =
  specializeResetT(p, rdLoc(a), a.t)
