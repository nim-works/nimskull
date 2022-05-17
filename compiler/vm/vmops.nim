#
#
#           The Nim Compiler
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Unfortunately this cannot be a module yet:
#import vmdeps, vm
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
  dirExists, fileExists, walkDir, getAppFilename, raiseOSError, osLastError

from std/md5 import getMD5
from std/times import cpuTime
from std/hashes import hash
from std/osproc import nil
from system/formatfloat import writeFloatToBufferSprintf


# There are some useful procs in vmconv.
import vmconv

template mathop(op) {.dirty.} =
  registerCallback(c, "stdlib.math." & astToStr(op), `op Wrapper`)

template osop(op) {.dirty.} =
  registerCallback(c, "stdlib.os." & astToStr(op), `op Wrapper`)

template timesop(op) {.dirty.} =
  registerCallback(c, "stdlib.times." & astToStr(op), `op Wrapper`)

template systemop(op) {.dirty.} =
  registerCallback(c, "stdlib.system." & astToStr(op), `op Wrapper`)

template ioop(op) {.dirty.} =
  registerCallback(c, "stdlib.io." & astToStr(op), `op Wrapper`)

template macrosop(op) {.dirty.} =
  registerCallback(c, "stdlib.macros." & astToStr(op), `op Wrapper`)

template md5op(op) {.dirty.} =
  registerCallback(c, "stdlib.md5." & astToStr(op), `op Wrapper`)

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

template wrapDangerous(op, modop) {.dirty.} =
  if vmopsDanger notin c.config.features and (defined(nimsuggest) or c.config.cmd == cmdCheck):
    proc `op Wrapper`(a: VmArgs) {.nimcall.} =
      discard
    modop op
  else:
    proc `op Wrapper`(a: VmArgs) {.nimcall.} =
      op(getString(a, 0), getString(a, 1))
    modop op

proc getCurrentExceptionMsgWrapper(a: VmArgs) {.nimcall.} =
  if a.currentException.isNil:
    setResult(a, "")
  else:
    let h = tryDeref(a.heap[], a.currentException, noneType).value()

    a.slots[a.ra].strVal.asgnVmString(
      deref(h.getFieldHandle(FieldPosition(2))).strVal,
      a.mem.allocator)

proc getCurrentExceptionWrapper(a: VmArgs) {.nimcall.} =
  deref(a.slots[a.ra].handle).refVal = a.currentException
  if not a.currentException.isNil:
    a.heap[].heapIncRef(a.currentException)

template wrapIteratorInner(a: VmArgs, iter: untyped) =
  let rh = a.getResultHandle()
  assert rh.typ.kind == akSeq

  let s = addr deref(rh).seqVal
  var i = 0
  for x in iter:
    s[].growBy(rh.typ, 1, a.mem[])
    writeTo(x, getItemHandle(s[], rh.typ, i), a.mem[])
    inc i


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
    of SingleValueSetting.linkOptions: result = conf.linkOptions
    of SingleValueSetting.compileOptions: result = conf.compileOptions
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

proc registerAdditionalOps*(c: PCtx) =

  template writeResult(ret) {.dirty.} =
    writeTo(ret, a.getResultHandle(), a.mem[])

  template wrapIterator(fqname: string, iter: untyped) =
    registerCallback c, fqname, proc(a: VmArgs) =
      wrapIteratorInner(a, iter)


  proc gorgeExWrapper(a: VmArgs) =
    let ret = opGorge(getString(a, 0), getString(a, 1), getString(a, 2),
                         a.currentLineInfo, c.config)
    writeResult(ret)

  proc getProjectPathWrapper(a: VmArgs) =
    setResult a, c.config.projectPath.string

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

  when declared(copySign):
    wrap2f_math(copySign)

  when declared(signbit):
    wrap1f_math(signbit)

  registerCallback c, "stdlib.math.round", proc (a: VmArgs) {.nimcall.} =
    let n = a.numArgs
    case n
    of 1: setResult(a, round(getFloat(a, 0)))
    of 2: setResult(a, round(getFloat(a, 0), getInt(a, 1).int))
    else: doAssert false, $n

  wrap1s(getMD5, md5op)

  proc `mod Wrapper`(a: VmArgs) {.nimcall.} =
    setResult(a, `mod`(getFloat(a, 0), getFloat(a, 1)))
  registerCallback(c, "stdlib.math.mod", `mod Wrapper`)

  when defined(nimcore):
    wrap2s(getEnv, osop)
    wrap1s(existsEnv, osop)
    wrap2svoid(putEnv, osop)
    wrap1svoid(delEnv, osop)
    wrap1s(dirExists, osop)
    wrap1s(fileExists, osop)
    wrapDangerous(writeFile, ioop)
    wrap1s(readFile, ioop)
    wrap2si(readLines, ioop)
    systemop getCurrentExceptionMsg
    systemop getCurrentException
    registerCallback c, "stdlib.*.staticWalkDir", proc (a: VmArgs) {.nimcall.} =
      let path = getString(a, 0)
      let relative = getBool(a, 1)
      wrapIteratorInner(a):
        walkDir(path, relative)

    when defined(nimHasInvariant):
      registerCallback c, "stdlib.compilesettings.querySetting", proc (a: VmArgs) =
        writeResult(querySettingImpl(c.config, getInt(a, 0)))
      registerCallback c, "stdlib.compilesettings.querySettingSeq", proc (a: VmArgs) =
        writeResult(querySettingSeqImpl(c.config, getInt(a, 0)))

    if defined(nimsuggest) or c.config.cmd == cmdCheck:
      discard "don't run staticExec for 'nim suggest'"
    else:
      systemop gorgeEx
  macrosop getProjectPath

  registerCallback c, "stdlib.os.getCurrentCompilerExe", proc (a: VmArgs) {.nimcall.} =
    setResult(a, getAppFilename())

  registerCallback c, "stdlib.macros.symBodyHash", proc (a: VmArgs) =
    let n = getNode(a, 0)
    if n.kind != nkSym:
      raiseVmError(reportAst(
        rsemVmNodeNotASymbol, n, str = "symBodyHash()"), n.info)

    setResult(a, $symBodyDigest(c.graph, n.sym))

  registerCallback c, "stdlib.macros.isExported", proc(a: VmArgs) =
    let n = getNode(a, 0)
    if n.kind != nkSym:
      raiseVmError(reportAst(
        rsemVmNodeNotASymbol, n, str = "isExported()"), n.info)

    setResult(a, sfExported in n.sym.flags)

  registerCallback c, "stdlib.vmutils.vmTrace", proc (a: VmArgs) =
    c.config.active.isVmTrace = getBool(a, 0)

  proc hashVmImpl(a: VmArgs) =
    # TODO: perform index check here
    var res = hashes.hash(a.getString(0), a.getInt(1).int, a.getInt(2).int)
    if c.config.backend == backendJs:
      # emulate JS's terrible integers:
      res = cast[int32](res)
    setResult(a, res)

  registerCallback c, "stdlib.hashes.hashVmImpl", hashVmImpl

  proc hashVmImplByte(a: VmArgs) =
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
        SemReport(
          kind: rsemVmIndexError,
          indexSpec: (
            usedIdx: toInt128(ePos),
            minIdx: toInt128(0),
            maxIdx: toInt128(seqVal.length-1))))

    let p = seqVal.data.rawPointer

    var res = hashes.hash(toOpenArray(p, sPos, ePos), sPos, ePos)
    if c.config.backend == backendJs:
      # emulate JS's terrible integers:
      res = cast[int32](res)
    setResult(a, res)

  registerCallback c, "stdlib.hashes.hashVmImplByte", hashVmImplByte
  registerCallback c, "stdlib.hashes.hashVmImplChar", hashVmImplByte

  if optBenchmarkVM in c.config.globalOptions or vmopsDanger in c.config.features:
    wrap0(cpuTime, timesop)
  else:
    proc cpuTime(): float = 5.391245e-44  # Randomly chosen
    wrap0(cpuTime, timesop)

  if vmopsDanger in c.config.features:
    ## useful procs but these should be opt-in because they may impact
    ## reproducible builds and users need to understand that this runs at CT.
    ## Note that `staticExec` can already do equal amount of damage so it's more
    ## of a semantic issue than a security issue.
    registerCallback c, "stdlib.os.getCurrentDir", proc (a: VmArgs) {.nimcall.} =
      setResult(a, os.getCurrentDir())
    registerCallback c, "stdlib.osproc.execCmdEx", proc (a: VmArgs) {.nimcall.} =
      let options = readAs(getHandle(a, 1), set[osproc.ProcessOption])
      writeResult osproc.execCmdEx(getString(a, 0), options)
    registerCallback c, "stdlib.times.getTime", proc (a: VmArgs) {.nimcall.} =
      writeResult times.getTime()

  proc getEffectList(c: PCtx; a: VmArgs; effectIndex: int) =
    let fn = getNode(a, 0)
    var list = newNodeI(nkBracket, fn.info)
    if fn.typ != nil and fn.typ.n != nil and fn.typ.n[0].len >= effectListLen and
        fn.typ.n[0][effectIndex] != nil:
      for e in fn.typ.n[0][effectIndex]:
        list.add opMapTypeInstToAst(c.cache, e.typ.skipTypes({tyRef}), e.info, c.idgen)
    else:
      list.add newIdentNode(getIdent(c.cache, "UncomputedEffects"), fn.info)

    setResult(a, list)

  registerCallback c, "stdlib.effecttraits.getRaisesListImpl", proc (a: VmArgs) =
    getEffectList(c, a, exceptionEffects)
  registerCallback c, "stdlib.effecttraits.getTagsListImpl", proc (a: VmArgs) =
    getEffectList(c, a, tagEffects)

  registerCallback c, "stdlib.effecttraits.isGcSafeImpl", proc (a: VmArgs) =
    let fn = getNode(a, 0)
    setResult(a, fn.typ != nil and tfGcSafe in fn.typ.flags)

  registerCallback c, "stdlib.effecttraits.hasNoSideEffectsImpl", proc (a: VmArgs) =
    let fn = getNode(a, 0)
    setResult(a, (fn.typ != nil and tfNoSideEffect in fn.typ.flags) or
                 (fn.kind == nkSym and fn.sym.kind == skFunc))

  registerCallback c, "stdlib.typetraits.hasClosureImpl", proc (a: VmArgs) =
    let fn = getNode(a, 0)
    setResult(a, fn.kind == nkClosure or (fn.typ != nil and fn.typ.callConv == ccClosure))

  registerCallback c, "stdlib.formatfloat.addFloatSprintf", proc(a: VmArgs) =
    let p = a.getVar(0)
    let x = a.getFloat(1)
    var temp {.noinit.}: array[65, char]
    let n = writeFloatToBufferSprintf(temp, x)
    let oldLen = deref(p).strVal.len
    deref(p).strVal.setLen(oldLen + n, a.mem.allocator)
    safeCopyMem(deref(p).strVal.data.subView(oldLen, n), temp, n)

  wrapIterator("stdlib.os.envPairsImplSeq"): envPairs()
