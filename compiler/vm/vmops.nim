#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements overrides for various stdlib and system functions.
## The overrides are sorted into multiple categories and are provided through
## iterators.

import
  compiler/ast/[
    ast_types,
    ast,
    idents,
    lineinfos,
  ],
  compiler/front/[
    options
  ],
  compiler/sem/[
    sighashes
  ],
  compiler/vm/[
    gorgeimpl,
    vmconv,
    vmdef,
    vmdeps,
    vmerrors,
    vmhooks,
    vmmemory,
    vmobjects
  ],
  experimental/[
    results
  ]

from compiler/front/msgs import localReport

# xxx: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport
from compiler/ast/report_enums import ReportKind

from std/math import sqrt, ln, log10, log2, exp, round, arccos, arcsin,
  arctan, arctan2, cos, cosh, hypot, sinh, sin, tan, tanh, pow, trunc,
  floor, ceil, `mod`, cbrt, arcsinh, arccosh, arctanh, erf, erfc, gamma,
  lgamma
when declared(math.copySign):
  # pending bug #18762, avoid renaming math
  from std/math as math2 import copySign

when declared(math.signbit):

  from std/math as math3 import signbit

from std/os import getEnv, existsEnv, delEnv, putEnv, envPairs,
  dirExists, fileExists, walkDir, getAppFilename, getCurrentDir,
  raiseOSError, osLastError

from std/md5 import getMD5
from std/times import getTime
from std/hashes import hash
from std/osproc import nil
from system/formatfloat import writeFloatToBufferSprintf

from compiler/modules/modulegraphs import `$`

type
  Override = tuple
    ## The information about a procedure override.
    pattern: string
    prc: VmCallback

template override(p: string, cb: VmCallback) =
  yield (p, VmCallback(cb))

template mathop(op) {.dirty.} =
  override("stdlib.math." & astToStr(op), `op Wrapper`)

template osop(op) {.dirty.} =
  override("stdlib.os." & astToStr(op), `op Wrapper`)

template systemop(op) {.dirty.} =
  override("stdlib.system." & astToStr(op), `op Wrapper`)

template ioop(op) {.dirty.} =
  override("stdlib.io." & astToStr(op), `op Wrapper`)

template macrosop(op) {.dirty.} =
  override("stdlib.macros." & astToStr(op), `op Wrapper`)

template md5op(op) {.dirty.} =
  override("stdlib.md5." & astToStr(op), `op Wrapper`)

template wrap1f_math(op) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    doAssert a.numArgs == 1
    setResult(a, op(getFloat(a, 0)))
  mathop op

template wrap2f_math(op) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    setResult(a, op(getFloat(a, 0), getFloat(a, 1)))
  mathop op

template wrap0(op, modop) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    setResult(a, op())
  modop op

template wrap1s(op, modop) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    setResult(a, op(getString(a, 0)))
  modop op

template wrap2s(op, modop) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    setResult(a, op(getString(a, 0), getString(a, 1)))
  modop op

template wrap2si(op, modop) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    writeTo(op(getString(a, 0), getInt(a, 1)), a.getResultHandle(), a.mem[])
  modop op

template wrap1svoid(op, modop) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    op(getString(a, 0))
  modop op

template wrap2svoid(op, modop) {.dirty.} =
  proc `op Wrapper`(a: VmArgs) {.nimcall.} =
    op(getString(a, 0), getString(a, 1))
  modop op

proc getCurrentExceptionMsgWrapper(a: VmArgs) {.nimcall.} =
  if a.currentException.isNil:
    setResult(a, "")
  else:
    let h = tryDeref(a.heap[], a.currentException, noneType).value()

    deref(a.slots[a.ra].handle).strVal.asgnVmString(
      deref(h.getFieldHandle(FieldPosition(2))).strVal,
      a.mem.allocator)

proc getCurrentExceptionWrapper(a: VmArgs) {.nimcall.} =
  deref(a.slots[a.ra].handle).refVal = a.currentException
  if not a.currentException.isNil:
    a.heap[].heapIncRef(a.currentException)

proc setCurrentExceptionWrapper(a: VmArgs) {.nimcall.} =
  # set the current exception to the one provided as the first argument
  asgnRef(a.currentException, deref(a.getHandle(0)).refVal,
          a.mem[], reset=true)

proc prepareExceptionWrapper(a: VmArgs) {.nimcall.} =
  let
    raised = a.heap[].tryDeref(deref(a.getHandle(0)).refVal, noneType).value()
    nameField = raised.getFieldHandle(1.fpos)

  # set the name of the exception if it hasn't been already:
  if deref(nameField).strVal.len == 0:
    # XXX: the VM doesn't distinguish between a `nil` cstring and an empty
    #      `cstring`, leading to the name erroneously being overridden if
    #      it was explicitly initialized with `""`
    asgnVmString(deref(nameField).strVal,
                 deref(a.getHandle(1)).strVal,
                 a.mem.allocator)

proc nimUnhandledExceptionWrapper(a: VmArgs) {.nimcall.} =
  # setup the exception AST:
  let
    exc = a.heap[].tryDeref(a.currentException, noneType).value()
    ast = toExceptionAst($exc.getFieldHandle(1.fpos).deref().strVal,
                         $exc.getFieldHandle(2.fpos).deref().strVal)
  # report the unhandled exception:
  # XXX: the current stack-trace should be passed along, but we don't
  #      have access to it here
  raiseVmError(VmEvent(kind: vmEvtUnhandledException, exc: ast))

proc prepareMutationWrapper(a: VmArgs) {.nimcall.} =
  discard "no-op"

template wrapIteratorInner(a: VmArgs, iter: untyped) =
  let rh = a.getResultHandle()
  assert rh.typ.kind == akSeq

  let s = addr deref(rh).seqVal
  var i = 0
  for x in iter:
    s[].growBy(rh.typ, 1, a.mem[])
    writeTo(x, getItemHandle(s[], rh.typ, i, a.mem.allocator), a.mem[])
    inc i

template wrapIterator(fqname: string, iter: untyped) =
  override fqname, proc(a: VmArgs) {.nimcall.} =
    wrapIteratorInner(a, iter)


when defined(nimHasInvariant):
  from std / compilesettings import SingleValueSetting, MultipleValueSetting

  proc querySettingImpl(conf: ConfigRef, switch: BiggestInt): string =
    case SingleValueSetting(switch)
    of SingleValueSetting.arguments: result = conf.arguments
    of SingleValueSetting.outFile: result = conf.outFile.string
    of SingleValueSetting.outDir: result = conf.outDir.string
    of SingleValueSetting.nimcacheDir: result = conf.getNimcacheDir().string
    of SingleValueSetting.projectName: result = conf.projectName
    of SingleValueSetting.projectPath: result = conf.projectPath.string
    of SingleValueSetting.projectFull: result = conf.projectFull.string
    of SingleValueSetting.command: result = conf.command
    of SingleValueSetting.commandLine: result = conf.commandLine
    of SingleValueSetting.linkOptions: result = conf.getLinkOptionsStr()
    of SingleValueSetting.compileOptions: result = conf.getCompileOptionsStr()
    of SingleValueSetting.ccompilerPath: result = conf.cCompilerPath
    of SingleValueSetting.backend: result = $conf.backend
    of SingleValueSetting.libPath: result = conf.libpath.string
    of gc: result = $conf.selectedGC

  proc querySettingSeqImpl(conf: ConfigRef, switch: BiggestInt): seq[string] =
    template copySeq(field: untyped): untyped =
      for i in field: result.add i.string

    case MultipleValueSetting(switch)
    of MultipleValueSetting.nimblePaths: copySeq(conf.nimblePaths)
    of MultipleValueSetting.searchPaths: copySeq(conf.searchPaths)
    of MultipleValueSetting.lazyPaths: copySeq(conf.lazyPaths)
    of MultipleValueSetting.commandArgs: result = conf.commandArgs
    of MultipleValueSetting.cincludes: copySeq(conf.cIncludes)
    of MultipleValueSetting.clibs: copySeq(conf.cLibs)

proc getEffectList(cache: IdentCache, idgen: IdGenerator; a: VmArgs;
                   effectIndex: int) =
  let fn = getNode(a, 0)
  var list = newNodeI(nkBracket, fn.info)
  if fn.typ != nil and fn.typ.n != nil and fn.typ.n[0].len >= effectListLen and
      fn.typ.n[0][effectIndex] != nil:
    for e in fn.typ.n[0][effectIndex]:
      list.add opMapTypeInstToAst(cache, e.typ.skipTypes({tyRef}), e.info, idgen)
  else:
    list.add newIdentNode(getIdent(cache, "UncomputedEffects"), fn.info)

  setResult(a, list)

template writeResult(ret) {.dirty.} =
  writeTo(ret, a.getResultHandle(), a.mem[])

iterator basicOps*(): Override =
  ## Basic system operations as well as overrides for some stdlib functions
  ## that don't interact with the host environement, but use language features
  ## that the VM doesn't directly support (such as 'importc'-ed functions)

  # system operations
  systemop(getCurrentExceptionMsg)
  systemop(getCurrentException)
  systemop(prepareException)
  systemop(nimUnhandledException)
  systemop(prepareMutation)
  override("stdlib.system.closureIterSetupExc",
           setCurrentExceptionWrapper)

  # math operations
  wrap1f_math(sqrt)
  wrap1f_math(cbrt)
  wrap1f_math(ln)
  wrap1f_math(log10)
  wrap1f_math(log2)
  wrap1f_math(exp)
  wrap1f_math(arccos)
  wrap1f_math(arcsin)
  wrap1f_math(arctan)
  wrap1f_math(arcsinh)
  wrap1f_math(arccosh)
  wrap1f_math(arctanh)
  wrap2f_math(arctan2)
  wrap1f_math(cos)
  wrap1f_math(cosh)
  wrap2f_math(hypot)
  wrap1f_math(sinh)
  wrap1f_math(sin)
  wrap1f_math(tan)
  wrap1f_math(tanh)
  wrap2f_math(pow)
  wrap1f_math(trunc)
  wrap1f_math(floor)
  wrap1f_math(ceil)
  wrap1f_math(erf)
  wrap1f_math(erfc)
  wrap1f_math(gamma)
  wrap1f_math(lgamma)
  #wrap1f_math(`mod`)
  # XXX: the csources compiler doesn't accept ``nkAccQuoted`` during
  #      identifier construction, so the above can't be used here
  override "stdlib.math.mod", proc(a: VmArgs) {.nimcall.} =
    setResult(a, `mod`(getFloat(a, 0), getFloat(a, 1)))

  when declared(copySign):
    wrap2f_math(copySign)

  when declared(signbit):
    wrap1f_math(signbit)

  override "stdlib.math.round", proc (a: VmArgs) {.nimcall.} =
    let n = a.numArgs
    case n
    of 1: setResult(a, round(getFloat(a, 0)))
    of 2: setResult(a, round(getFloat(a, 0), getInt(a, 1).int))
    else: doAssert false, $n

  wrap1s(getMD5, md5op)

  # ``hashes`` module

  proc hashVmImpl(a: VmArgs) {.nimcall.} =
    # TODO: perform index check here
    var res = hashes.hash(a.getString(0), a.getInt(1).int, a.getInt(2).int)
    if a.config.backend == backendJs:
      # emulate JS's terrible integers:
      res = cast[int32](res)
    setResult(a, res)

  override "stdlib.hashes.hashVmImpl", hashVmImpl

  proc hashVmImplByte(a: VmArgs) {.nimcall.} =
    let sPos = a.getInt(1).int
    let ePos = a.getInt(2).int
    let arr = a.getHandle(0)
    # XXX: openArray is currently treated as `seq` in the vm
    assert arr.typ.kind == akSeq
    assert arr.typ.seqElemType.kind == akInt
    assert arr.typ.seqElemType.sizeInBytes == 1

    let seqVal = addr deref(arr).seqVal

    if ePos >= seqVal.length:
      raiseVmError(
        VmEvent(
          kind: vmEvtIndexError,
          indexSpec: (
            usedIdx: toInt128(ePos),
            minIdx: toInt128(0),
            maxIdx: toInt128(seqVal.length-1))))

    let p = seqVal.data.rawPointer

    var res = hashes.hash(toOpenArray(p, sPos, ePos), sPos, ePos)
    if a.config.backend == backendJs:
      # emulate JS's terrible integers:
      res = cast[int32](res)
    setResult(a, res)

  override "stdlib.hashes.hashVmImplByte", hashVmImplByte
  override "stdlib.hashes.hashVmImplChar", hashVmImplByte

  # ``formatfloat`` module

  override "stdlib.formatfloat.addFloatSprintf", proc(a: VmArgs) {.nimcall.} =
    let p = a.getVar(0)
    let x = a.getFloat(1)
    var temp {.noinit.}: array[65, char]
    let n = writeFloatToBufferSprintf(temp, x)
    let oldLen = deref(p).strVal.len
    deref(p).strVal.setLen(oldLen + n, a.mem.allocator)
    safeCopyMem(deref(p).strVal.data.slice(oldLen, n), temp, n)

iterator ioReadOps*(): Override =
  ## Returns overrides for read operations from the ``io`` module.
  wrap1s(readFile, ioop)
  wrap2si(readLines, ioop)

iterator ioWriteOps*(): Override =
  ## Returns overrides for write operations from the ``io`` module.
  wrap2svoid(writeFile, ioop)

iterator osOps*(): Override =
  ## OS operations that can't modify the host's enivronment.

  wrap2s(getEnv, osop)
  wrap1s(existsEnv, osop)
  wrap1s(dirExists, osop)
  wrap1s(fileExists, osop)
  override "stdlib.*.staticWalkDir", proc (a: VmArgs) {.nimcall.} =
    let path = getString(a, 0)
    let relative = getBool(a, 1)
    wrapIteratorInner(a):
      walkDir(path, relative)

  wrap0(getCurrentDir, osop)

  wrapIterator("stdlib.os.envPairsImplSeq"): envPairs()

  override "stdlib.times.getTime", proc (a: VmArgs) {.nimcall.} =
    writeResult times.getTime()

iterator os2Ops*(): Override =
  ## OS operations that are able to modify the host's environment or run
  ## external programs

  wrap2svoid(putEnv, osop)
  wrap1svoid(delEnv, osop)

  override "stdlib.osproc.execCmdEx", proc (a: VmArgs) {.nimcall.} =
    let options = readAs(getHandle(a, 1), set[osproc.ProcessOption])
    writeResult osproc.execCmdEx(getString(a, 0), options)

iterator compileTimeOps*(): Override =
  ## Operations for querying compiler related information at compile-time.

  when defined(nimHasInvariant):
    override "stdlib.compilesettings.querySetting", proc (a: VmArgs) {.nimcall.} =
      writeResult(querySettingImpl(a.config, getInt(a, 0)))
    override "stdlib.compilesettings.querySettingSeq", proc (a: VmArgs) {.nimcall.} =
      writeResult(querySettingSeqImpl(a.config, getInt(a, 0)))

  override "stdlib.os.getCurrentCompilerExe", proc (a: VmArgs) {.nimcall.} =
    setResult(a, getAppFilename())
  
  # xxx: not really a compile-time query, but runs at compiletime and unlike
  #      osOps it directly impacts compilation
  for op in ["stdlib.system.slurp", "stdlib.system.staticRead"]:
    override op, proc (a: VmArgs) {.nimcall.} =
      let output = opSlurp(getString(a, 0), a.currentLineInfo, a.currentModule,
                           a.config)
      writeResult(output)

iterator gorgeOps*(): Override =
  ## Special operations for executing external programs at compile time.
  for op in ["stdlib.system.gorge", "stdlib.system.staticExec"]:
    override op, proc (a: VmArgs) {.nimcall.} =
      let (output, _) = opGorge(getString(a, 0), getString(a, 1),
                                getString(a, 2), a.currentLineInfo, a.config)
      writeResult(output)

  override "stdlib.system.gorgeEx", proc (a: VmArgs) {.nimcall.} =
    let ret = opGorge(getString(a, 0), getString(a, 1), getString(a, 2),
                      a.currentLineInfo, a.config)
    writeResult(ret)

iterator debugOps*(): Override =
  override "stdlib.vmutils.vmTrace", proc (a: VmArgs) {.nimcall.} =
    # XXX: `isVmTrace` should probably be in `TCtx` instead of in the active
    a.config.active.isVmTrace = getBool(a, 0)

iterator macroOps*(): Override =
  ## Operations that are part of the Macro API

  # XXX: doesn't really have to do anything with macros, but it's in
  #      `stdlib.macros`, so...
  proc getProjectPathWrapper(a: VmArgs) {.nimcall.} =
    setResult a, a.config.projectPath.string
  macrosop getProjectPath

  override "stdlib.macros.symBodyHash", proc (a: VmArgs) {.nimcall.} =
    let n = getNode(a, 0)
    if n.kind != nkSym:
      raiseVmError(VmEvent(
        kind: vmEvtArgNodeNotASymbol,
        callName: "symBodyHash",
        argAst: n,
        argPos: 0))

    setResult(a, $symBodyDigest(a.graph, n.sym))

  override "stdlib.macros.isExported", proc(a: VmArgs) {.nimcall.} =
    let n = getNode(a, 0)
    if n.kind != nkSym:
      raiseVmError(VmEvent(
        kind: vmEvtArgNodeNotASymbol,
        callName: "isExported",
        argAst: n,
        argPos: 0))

    setResult(a, sfExported in n.sym.flags)

  override "stdlib.effecttraits.getRaisesListImpl", proc (a: VmArgs) {.nimcall.} =
    getEffectList(a.cache, a.idgen, a, exceptionEffects)
  override "stdlib.effecttraits.getTagsListImpl", proc (a: VmArgs) {.nimcall.} =
    getEffectList(a.cache, a.idgen, a, tagEffects)

  override "stdlib.effecttraits.isGcSafeImpl", proc (a: VmArgs) {.nimcall.} =
    let fn = getNode(a, 0)
    setResult(a, fn.typ != nil and tfGcSafe in fn.typ.flags)

  override "stdlib.effecttraits.hasNoSideEffectsImpl", proc (a: VmArgs) {.nimcall.} =
    let fn = getNode(a, 0)
    setResult(a, (fn.typ != nil and tfNoSideEffect in fn.typ.flags) or
                 (fn.kind == nkSym and fn.sym.kind == skFunc))

  override "stdlib.typetraits.hasClosureImpl", proc (a: VmArgs) {.nimcall.} =
    let fn = getNode(a, 0)
    setResult(a, fn.kind == nkClosure or (fn.typ != nil and fn.typ.callConv == ccClosure))

  template getInfo(a: VmArgs): TLineInfo =
    let b = getNode(a, 1)
    if b.kind == nkNilLit: a.currentLineInfo else: b.info

  override "stdlib.macros.error", proc (a: VmArgs) {.nimcall.} =
    raiseVmError(VmEvent(
      kind: vmEvtUserError,
      errMsg: getString(a, 0),
      errLoc: a.getInfo()))

  override "stdlib.macros.warning", proc (a: VmArgs) {.nimcall.} =
    a.config.localReport(a.getInfo(),
                         SemReport(kind: rsemUserWarning, str: getString(a, 0)))

  override "stdlib.macros.hint", proc (a: VmArgs) {.nimcall.} =
    a.config.localReport(a.getInfo(),
                         SemReport(kind: rsemUserHint, str: getString(a, 0)))
