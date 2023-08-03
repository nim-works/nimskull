#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Built-in types and compilerprocs are registered here.

import
  compiler/utils/[
    idioms,
    platform,
  ],
  compiler/ast/[
    lineinfos,
    errorhandling,
    idents,
    reports,
    ast,
    astalgo,
  ],
  compiler/modules/[
    modulegraphs,
  ],
  compiler/front/[
    msgs,
    options
  ]

# TODO: `reportStr` is being abused, it's not quite a sem error
from compiler/ast/reports_sem import reportStr,
  reportSymbols,
  reportTyp

# TODO: at least use internalAssert/Error better still have its own data type
#       for diag/event/telemetry
from compiler/ast/reports_internal import InternalReport
# from compiler/ast/report_enums import ReportKind

export createMagic

proc nilOrSysInt*(g: ModuleGraph): PType = g.sysTypes[tyInt]

proc newSysType(g: ModuleGraph; kind: TTypeKind, size: int): PType =
  result = newType(kind, nextTypeId(g.idgen), g.systemModule)
  result.size = size
  result.align = size.int16

proc getSysSym*(g: ModuleGraph; info: TLineInfo; name: string): PSym =
  result = systemModuleSym(g, getIdent(g.cache, name))
  if result.isNil:
    g.config.localReport(info, reportStr(rsemSystemNeeds, name))

    result = newSym(
      skError, getIdent(g.cache, name), nextSymId(g.idgen), g.systemModule, g.systemModule.info, {})
    result.typ = newType(tyError, nextTypeId(g.idgen), g.systemModule)


proc getSysMagic*(g: ModuleGraph; info: TLineInfo; name: string, m: TMagic): PSym =
  let id = getIdent(g.cache, name)
  for r in systemModuleSyms(g, id):
    if r.magic == m:
      # prefer the tyInt variant:
      if r.typ[0] != nil and r.typ[0].kind == tyInt: return r
      result = r
  if result != nil: return result
  g.config.localReport(info, reportStr(rsemSystemNeeds, name))

  result = newSym(skError, id, nextSymId(g.idgen), g.systemModule, g.systemModule.info, {})
  result.typ = newType(tyError, nextTypeId(g.idgen), g.systemModule)

proc sysTypeFromName*(g: ModuleGraph; info: TLineInfo; name: string): PType =
  result = getSysSym(g, info, name).typ

proc getSysType*(g: ModuleGraph; info: TLineInfo; kind: TTypeKind): PType =
  template sysTypeFromName(s: string): untyped = sysTypeFromName(g, info, s)
  result = g.sysTypes[kind]
  if result == nil:
    case kind
    of tyVoid: result = sysTypeFromName("void")
    of tyInt: result = sysTypeFromName("int")
    of tyInt8: result = sysTypeFromName("int8")
    of tyInt16: result = sysTypeFromName("int16")
    of tyInt32: result = sysTypeFromName("int32")
    of tyInt64: result = sysTypeFromName("int64")
    of tyUInt: result = sysTypeFromName("uint")
    of tyUInt8: result = sysTypeFromName("uint8")
    of tyUInt16: result = sysTypeFromName("uint16")
    of tyUInt32: result = sysTypeFromName("uint32")
    of tyUInt64: result = sysTypeFromName("uint64")
    of tyFloat: result = sysTypeFromName("float")
    of tyFloat32: result = sysTypeFromName("float32")
    of tyFloat64: result = sysTypeFromName("float64")
    of tyFloat128: result = sysTypeFromName("float128")
    of tyBool: result = sysTypeFromName("bool")
    of tyChar: result = sysTypeFromName("char")
    of tyString: result = sysTypeFromName("string")
    of tyCstring: result = sysTypeFromName("cstring")
    of tyPointer: result = sysTypeFromName("pointer")
    of tyNil: result = newSysType(g, tyNil, g.config.target.ptrSize)
    else:
      g.config.localReport InternalReport(
        kind: rintUnreachable, msg: "request for typekind: " & $kind)
    g.sysTypes[kind] = result
  if result.kind != kind:
    if kind == tyFloat64 and result.kind == tyFloat: discard # because of aliasing
    else:
      g.config.localReport InternalReport(
        kind: rintUnreachable,
        msg: "wanted: " & $kind & " got: " & $result.kind)
  if result == nil:
    g.config.localReport InternalReport(
      kind: rintUnreachable,
      msg: "type not found: " & $kind)

proc resetSysTypes*(g: ModuleGraph) =
  g.systemModule = nil
  initStrTable(g.compilerprocs)
  initStrTable(g.exposed)
  for i in low(g.sysTypes)..high(g.sysTypes):
    g.sysTypes[i] = nil

proc getFloatLitType*(g: ModuleGraph; literal: PNode): PType =
  # for now we do not cache these:
  result = newSysType(g, tyFloat, size=8)
  result.n = literal

proc skipIntLit*(t: PType; id: IdGenerator): PType {.inline.} =
  # xxx: rename to `numLitToType` or the like
  if t.n != nil and t.kind in {tyInt, tyFloat}:
    result = copyType(t, nextTypeId(id), t.owner)
    result.n = nil
  else:
    result = t

proc addSonSkipIntLit*(parent, kid: PType; id: IdGenerator) =
  let s = kid.skipIntLit(id)
  parent.sons.add(s)
  propagateToOwner(parent, s)

proc getCompilerProc*(g: ModuleGraph; name: string): PSym =
  let ident = getIdent(g.cache, name)
  result = strTableGet(g.compilerprocs, ident)
  if result == nil:
    result = loadCompilerProc(g, name)

proc registerCompilerProc*(g: ModuleGraph; s: PSym) =
  strTableAdd(g.compilerprocs, s)

proc registerNimScriptSymbol*(g: ModuleGraph; s: PSym) =
  # Nimscript symbols must be al unique:
  let conflict = strTableGet(g.exposed, s.name)
  if conflict == nil:
    strTableAdd(g.exposed, s)
  else:
    g.config.localReport(s.info, reportSymbols(
      rsemConflictingExportnims, @[s, conflict]))

proc registerNimScriptSymbol2*(g: ModuleGraph; s: PSym): PNode =
  # Nimscript symbols must be all unique:
  let conflict = strTableGet(g.exposed, s.name)
  if conflict.isNil:
    strTableAdd(g.exposed, s)
    g.emptyNode
  else:
    g.config.newError(
      newSymNode(s),
      PAstDiag(kind: adSemConflictingExportnims, conflict: conflict))

proc getNimScriptSymbol*(g: ModuleGraph; name: string): PSym =
  strTableGet(g.exposed, getIdent(g.cache, name))

proc resetNimScriptSymbols*(g: ModuleGraph) = initStrTable(g.exposed)

func getMagicEqForType*(t: PType): TMagic =
  ## Returns the ``mEqX`` magic for the given type `t`.
  case t.kind
  of tyInt,  tyInt8, tyInt16, tyInt32, tyInt64,
     tyUInt, tyUInt8, tyUInt16, tyUInt32, tyUInt64:
    mEqI
  of tyEnum:   mEqEnum
  of tyBool:   mEqB
  of tyRef, tyPtr, tyPointer: mEqRef
  of tyString: mEqStr
  of tyChar:   mEqCh
  of tySet:    mEqSet
  of tyProc:   mEqProc
  else:
    unreachable(t.kind)

proc getMagicEqSymForType*(g: ModuleGraph; t: PType; info: TLineInfo): PSym =
  let magic = getMagicEqForType(t)
  result = getSysMagic(g, info, "==", magic)
