#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Thread var support for architectures that lack native support for
## thread local storage.

# included from cgen.nim

proc emulatedThreadVars(conf: ConfigRef): bool =
  result = {optThreads, optTlsEmulation} <= conf.globalOptions

proc accessThreadLocalVar(p: BProc, s: PSym) =
  if emulatedThreadVars(p.config) and threadVarAccessed notin p.flags:
    p.flags.incl threadVarAccessed
    incl p.module.flags, usesThreadVars
    p.procSec(cpsLocals).addf("\tNimThreadVars* NimTV_;$n", [])
    p.procSec(cpsInit).add(
      ropecg(p.module, "\tNimTV_ = (NimThreadVars*) #GetThreadLocalVars();$n", []))

proc declareThreadVar*(m: BModule, s: PSym, isExtern: bool) =
  if emulatedThreadVars(m.config):
    # we gather all thread locals var into a struct; we need to allocate
    # storage for that somehow, can't use the thread local storage
    # allocator for it :-(
    if not containsOrIncl(m.g.nimtvDeclared, s.id):
      m.g.nimtvDeps.add(s.typ)
      m.g.nimtv.addf("$1 $2;$n", [getTypeDesc(m, s.typ), m.globals[s].r])
  else:
    if isExtern: m.s[cfsVars].add("extern ")
    elif exfExportLib in s.extFlags: m.s[cfsVars].add("N_LIB_EXPORT_VAR ")
    else: m.s[cfsVars].add("N_LIB_PRIVATE ")
    if optThreads in m.config.globalOptions:
      m.s[cfsVars].add("NIM_THREADVAR ")
    m.s[cfsVars].add(getTypeDesc(m, s.typ))
    m.s[cfsVars].addf(" $1;$n", [m.globals[s].r])

proc generateThreadLocalStorage(m: BModule) =
  if m.g.nimtv != "" and (usesThreadVars in m.flags or sfMainModule in m.module.flags):
    for t in items(m.g.nimtvDeps): discard getTypeDesc(m, t)
    finishTypeDescriptions(m)
    m.s[cfsSeqTypes].addf("typedef struct {$1} NimThreadVars;$n", [m.g.nimtv])

proc generateThreadVarsSize(m: BModule) =
  if m.g.nimtv != "":
    m.s[cfsProcs].addf("NI NimThreadVarsSize(){return (NI)sizeof(NimThreadVars);}$n", [])
