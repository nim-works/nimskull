#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Module providing functions for calling the different external C
## compilers Uses some hard-wired facts about each C compiler, plus
## options read from a lineinfos file, to provide generalized procedures to
## compile nim files.

import
  compiler/utils/[
    ropes,
    platform,
    pathutils
  ],
  compiler/ast/[
    lineinfos,
  ],
  compiler/front/[
    options,
    msgs
  ],
  std/[
    os,
    strutils,
    osproc,
    sha1,
    streams,
    sequtils,
    times,
    strtabs,
    json,
    jsonutils,
    sugar
  ]

# TODO: does it really make sense that this module should produce all these
#       types of "reports"? Unlikely, it's probably creating a bunch of false
#       dependencies with other modules... yay!
#       Also, diagnostic, telemetry, and event are better terms
from compiler/ast/report_enums import ReportKind
from compiler/ast/reports_cmd import CmdReport
from compiler/ast/reports_backend import BackendReport


type
  TInfoCCProp* = enum         ## properties of the C compiler:
    hasSwitchRange            ## CC allows ranges in switch statements (GNU C)
    hasComputedGoto           ## CC has computed goto (GNU C extension)
    hasAssume                 ## CC has `__assume` (Visual C extension)
    hasGcGuard                ## CC supports GC_GUARD to keep stack roots
    hasGnuAsm                 ## CC's asm uses the absurd GNU assembler syntax
    hasDeclspec               ## CC has `__declspec(X)`
    hasAttribute              ## CC has `__attribute__((X))`

  TInfoCCProps* = set[TInfoCCProp]
  TInfoCC* = object
    name*: string           ## the short name of the compiler
    objExt*: string         ## the compiler's object file extension
    optSpeed*: string       ## the options for optimization for speed
    optSize*: string        ## the options for optimization for size
    compilerExe*: string    ## the compiler's executable
    compileTmpl*: string    ## the compile command template
    buildGui*: string       ## command to build a GUI application
    buildDll*: string       ## command to build a shared library
    buildLib*: string       ## command to build a static library
    linkerExe*: string      ## the linker's executable (if not matching compiler's)
    linkTmpl*: string       ## command to link files to produce an exe
    includeCmd*: string     ## command to add an include dir
    linkDirCmd*: string     ## command to add a lib dir
    linkLibCmd*: string     ## command to link an external library
    debug*: string          ## flags for debug build
    pic*: string            ## command for position independent code
                            ## used on some platforms
    asmStmtFrmt*: string    ## format of ASM statement
    structStmtFmt*: string  ## Format for struct statement
    produceAsm*: string     ## Format how to produce assembler listings
    props*: TInfoCCProps    ## properties of the C compiler


# Configuration settings for various compilers.
# When adding new compilers, the cmake sources could be a good reference:
# http://cmake.org/gitweb?p=cmake.git;a=tree;f=Modules/Platform;

template compiler(name, settings: untyped): untyped =
  proc name: TInfoCC {.compileTime.} = settings

const
  gnuAsmListing = "-Wa,-acdl=$asmfile -g -fverbose-asm -masm=intel"

# GNU C Compiler
compiler gcc:
  result = TInfoCC(
    name: "gcc",
    objExt: "o",
    optSpeed: " -O3 -fno-ident",
    optSize: " -Os -fno-ident",
    compilerExe: "gcc",
    compileTmpl: "-c $options $include -o $objfile $file",
    buildGui: " -mwindows",
    buildDll: " -shared",
    buildLib: "ar rcs $libfile $objfiles",
    linkerExe: "",
    linkTmpl: "$buildgui $builddll -o $exefile $objfiles $options",
    includeCmd: " -I",
    linkDirCmd: " -L",
    linkLibCmd: " -l$1",
    debug: "",
    pic: "-fPIC",
    asmStmtFrmt: "asm($1);$n",
    structStmtFmt: "$1 $3 $2 ", # struct|union [packed] $name
    produceAsm: gnuAsmListing,
    props: {hasSwitchRange, hasComputedGoto, hasGcGuard, hasGnuAsm,
            hasAttribute})

# GNU C and C++ Compiler
compiler nintendoSwitchGCC:
  result = TInfoCC(
    name: "switch_gcc",
    objExt: "o",
    optSpeed: " -O3 ",
    optSize: " -Os ",
    compilerExe: "aarch64-none-elf-gcc",
    compileTmpl: "-w -MMD -MP -MF $dfile -c $options $include -o $objfile $file",
    buildGui: " -mwindows",
    buildDll: " -shared",
    buildLib: "aarch64-none-elf-gcc-ar rcs $libfile $objfiles",
    linkerExe: "aarch64-none-elf-gcc",
    linkTmpl: "$buildgui $builddll -Wl,-Map,$mapfile -o $exefile $objfiles $options",
    includeCmd: " -I",
    linkDirCmd: " -L",
    linkLibCmd: " -l$1",
    debug: "",
    pic: "-fPIE",
    asmStmtFrmt: "asm($1);$n",
    structStmtFmt: "$1 $3 $2 ", # struct|union [packed] $name
    produceAsm: gnuAsmListing,
    props: {hasSwitchRange, hasComputedGoto, hasGcGuard, hasGnuAsm,
            hasAttribute})

# LLVM Frontend for GCC/G++
compiler llvmGcc:
  result = gcc() # Uses settings from GCC

  result.name = "llvm_gcc"
  result.compilerExe = "llvm-gcc"
  when defined(macosx) or defined(openbsd):
    # `llvm-ar` not available
    result.buildLib = "ar rcs $libfile $objfiles"
  else:
    result.buildLib = "llvm-ar rcs $libfile $objfiles"

# Clang (LLVM) C Compiler
compiler clang:
  result = llvmGcc() # Uses settings from llvmGcc

  result.name = "clang"
  result.compilerExe = "clang"

# Microsoft Visual C/C++ Compiler
compiler vcc:
  result = TInfoCC(
    name: "vcc",
    objExt: "obj",
    optSpeed: " /Ogityb2 ",
    optSize: " /O1 ",
    compilerExe: "cl",
    compileTmpl: "/c$vccplatform $options $include /nologo /Fo$objfile $file",
    buildGui: " /SUBSYSTEM:WINDOWS user32.lib ",
    buildDll: " /LD",
    buildLib: "lib /OUT:$libfile $objfiles",
    linkerExe: "cl",
    linkTmpl: "$builddll$vccplatform /Fe$exefile $objfiles $buildgui /nologo $options",
    includeCmd: " /I",
    linkDirCmd: " /LIBPATH:",
    linkLibCmd: " $1.lib",
    debug: " /RTC1 /Z7 ",
    pic: "",
    asmStmtFrmt: "__asm{$n$1$n}$n",
    structStmtFmt: "$3$n$1 $2",
    produceAsm: "/Fa$asmfile",
    props: {hasAssume, hasDeclspec})

compiler clangcl:
  result = vcc()
  result.name = "clang_cl"
  result.compilerExe = "clang-cl"
  result.linkerExe = "clang-cl"
  result.linkTmpl = "-fuse-ld=lld " & result.linkTmpl

# Intel C/C++ Compiler
compiler icl:
  result = vcc()
  result.name = "icl"
  result.compilerExe = "icl"
  result.linkerExe = "icl"

# Intel compilers try to imitate the native ones (gcc and msvc)
compiler icc:
  result = gcc()
  result.name = "icc"
  result.compilerExe = "icc"
  result.linkerExe = "icc"

# Borland C Compiler
compiler bcc:
  result = TInfoCC(
    name: "bcc",
    objExt: "obj",
    optSpeed: " -O3 -6 ",
    optSize: " -O1 -6 ",
    compilerExe: "bcc32c",
    compileTmpl: "-c $options $include -o$objfile $file",
    buildGui: " -tW",
    buildDll: " -tWD",
    buildLib: "", # XXX: not supported yet
    linkerExe: "bcc32",
    linkTmpl: "$options $buildgui $builddll -e$exefile $objfiles",
    includeCmd: " -I",
    linkDirCmd: "", # XXX: not supported yet
    linkLibCmd: "", # XXX: not supported yet
    debug: "",
    pic: "",
    asmStmtFrmt: "__asm{$n$1$n}$n",
    structStmtFmt: "$1 $2",
    produceAsm: "",
    props: {hasSwitchRange, hasComputedGoto, hasGcGuard, hasAttribute})

# Tiny C Compiler
compiler tcc:
  result = TInfoCC(
    name: "tcc",
    objExt: "o",
    optSpeed: "",
    optSize: "",
    compilerExe: "tcc",
    compileTmpl: "-c $options $include -o $objfile $file",
    buildGui: "-Wl,-subsystem=gui",
    buildDll: " -shared",
    buildLib: "", # XXX: not supported yet
    linkerExe: "tcc",
    linkTmpl: "-o $exefile $options $buildgui $builddll $objfiles",
    includeCmd: " -I",
    linkDirCmd: "", # XXX: not supported yet
    linkLibCmd: "", # XXX: not supported yet
    debug: " -g ",
    pic: "",
    asmStmtFrmt: "asm($1);$n",
    structStmtFmt: "$1 $2",
    produceAsm: gnuAsmListing,
    props: {hasSwitchRange, hasComputedGoto, hasGnuAsm})

# Your C Compiler
compiler envcc:
  result = TInfoCC(
    name: "env",
    objExt: "o",
    optSpeed: " -O3 ",
    optSize: " -O1 ",
    compilerExe: "",
    compileTmpl: "-c $ccenvflags $options $include -o $objfile $file",
    buildGui: "",
    buildDll: " -shared ",
    buildLib: "", # XXX: not supported yet
    linkerExe: "",
    linkTmpl: "-o $exefile $buildgui $builddll $objfiles $options",
    includeCmd: " -I",
    linkDirCmd: "", # XXX: not supported yet
    linkLibCmd: "", # XXX: not supported yet
    debug: "",
    pic: "",
    asmStmtFrmt: "__asm{$n$1$n}$n",
    structStmtFmt: "$1 $2",
    produceAsm: "",
    props: {hasGnuAsm})

const
  CC*: array[succ(low(TSystemCC))..high(TSystemCC), TInfoCC] = [
    gcc(),
    nintendoSwitchGCC(),
    llvmGcc(),
    clang(),
    bcc(),
    vcc(),
    tcc(),
    envcc(),
    icl(),
    icc(),
    clangcl()]

  hExt* = ".h"

template writePrettyCmdsStderr(cmd) =
  if cmd.len > 0:
    flushDot(conf)
    stderr.writeLine(cmd)

proc nameToCC*(name: string): TSystemCC =
  ## Returns the kind of compiler referred to by `name`, or ccNone
  ## if the name doesn't refer to any known compiler.
  for i in succ(ccNone)..high(TSystemCC):
    if cmpIgnoreStyle(name, CC[i].name) == 0:
      return i
  result = ccNone

proc listCCnames*(): seq[string] =
  for i in succ(ccNone)..high(TSystemCC):
    result.add CC[i].name

proc isVSCompatible*(conf: ConfigRef): bool =
  return conf.cCompiler == ccVcc or
          conf.cCompiler == ccClangCl or
          (conf.cCompiler == ccIcl and conf.target.hostOS in osDos..osWindows)

proc getConfigVar(conf: ConfigRef; c: TSystemCC, suffix: string): string =
  # use ``cpu.os.cc`` for cross compilation, unless ``--compileOnly`` is given
  # for niminst support
  var fullSuffix = suffix
  case conf.backend
  of backendJs, backendNimVm: fullSuffix = "." & $conf.backend & suffix
  of backendC: discard
  of backendInvalid:
    # during parsing of cfg files; we don't know the backend yet, no point in
    # guessing wrong thing
    return ""

  if (conf.target.hostOS != conf.target.targetOS or conf.target.hostCPU != conf.target.targetCPU) and
      optCompileOnly notin conf.globalOptions:
    let fullCCname = platform.CPU[conf.target.targetCPU].name & '.' &
                     platform.OS[conf.target.targetOS].name & '.' &
                     CC[c].name & fullSuffix
    result = getConfigVar(conf, fullCCname)
    if result.len == 0:
      # not overridden for this cross compilation setting?
      result = getConfigVar(conf, CC[c].name & fullSuffix)
  else:
    result = getConfigVar(conf, CC[c].name & fullSuffix)

proc setCC*(conf: ConfigRef; ccname: string): TSystemCC =
  ## returns the compiler that was set, or `ccNone` in case there was an error
  result = nameToCC(ccname)
  conf.cCompiler = result
  if conf.cCompiler == ccNone:
    # conf.localReport(ExternalReport(
    # kind: rextUnknownCCompiler,
    # knownCompilers: listCCnames(),
    # passedCompiler: ccname))
    return

  conf.compileOptions = getConfigVar(conf, conf.cCompiler, ".options.always")
  conf.linkOptions = ""
  conf.cCompilerPath = getConfigVar(conf, conf.cCompiler, ".path")

  for c in CC:
    undefSymbol(conf, c.name)

  defineSymbol(conf, CC[conf.cCompiler].name)

proc addOpt(dest: var string, src: string) =
  if dest.len == 0 or dest[^1] != ' ': dest.add(" ")
  dest.add(src)

proc addLinkOption*(conf: ConfigRef; option: string) =
  addOpt(conf.linkOptions, option)

proc addCompileOption*(conf: ConfigRef; option: string) =
  if strutils.find(conf.compileOptions, option, 0) < 0:
    addOpt(conf.compileOptions, option)

proc addLinkOptionCmd*(conf: ConfigRef; option: string) =
  conf.linkOptionsCmdAdd option

proc addCompileOptionCmd*(conf: ConfigRef; option: string) =
  conf.compileOptionsCmdAdd option

proc initVars*(conf: ConfigRef) =
  # we need to define the symbol here, because ``CC`` may have never been set!
  for c in CC:
    undefSymbol(conf, c.name)

  defineSymbol(conf, CC[conf.cCompiler].name)
  addCompileOption(conf, getConfigVar(conf, conf.cCompiler, ".options.always"))
  #addLinkOption(getConfigVar(cCompiler, ".options.linker"))
  if conf.cCompilerPath.len == 0:
    conf.cCompilerPath = getConfigVar(conf, conf.cCompiler, ".path")

proc completeCfilePath*(conf: ConfigRef; cfile: AbsoluteFile,
                        createSubDir: bool = true): AbsoluteFile =
  result = completeGeneratedFilePath(conf, cfile, createSubDir)

proc toObjFile*(conf: ConfigRef; filename: AbsoluteFile): AbsoluteFile =
  # Object file for compilation
  result = AbsoluteFile(filename.string & "." & CC[conf.cCompiler].objExt)

proc addFileToCompile*(conf: ConfigRef; cf: Cfile) =
  conf.toCompile.add(cf)

proc addLocalCompileOption*(conf: ConfigRef; option: string; nimfile: AbsoluteFile) =
  let key = completeCfilePath(conf, withPackageName(conf, nimfile)).string
  var value = conf.cfileSpecificOptions.getOrDefault(key)
  if strutils.find(value, option, 0) < 0:
    addOpt(value, option)
    conf.cfileSpecificOptions[key] = value

proc resetCompilationLists*(conf: ConfigRef) =
  conf.toCompile.setLen 0
  ## XXX: we must associate these with their originating module
  # when the module is loaded/unloaded it adds/removes its items
  # That's because we still need to hash check the external files
  # Maybe we can do that in checkDep on the other hand?
  conf.externalToLink.setLen 0

proc addExternalFileToLink*(conf: ConfigRef; filename: AbsoluteFile) =
  ## Add new file to be linked with every compiled target.
  conf.externalToLink.insert(filename.string, 0)

proc execWithEcho(conf: ConfigRef; cmd: string, execKind: ReportKind): int =
  conf.localReport(CmdReport(kind: execKind, cmd: cmd))
  result = execCmd(cmd)

proc execExternalProgram(conf: ConfigRef; cmd: string, kind: ReportKind) =
  let code = execWithEcho(conf, cmd, kind)
  if code != 0:
    conf.localReport CmdReport(kind: rcmdFailedExecution, cmd: cmd, code: code)

proc generateScript(conf: ConfigRef; script: Rope) =
  let (_, name, _) = splitFile(conf.outFile.string)
  let filename = getNimcacheDir(conf) / RelativeFile(
    addFileExt("compile_" & name, platform.OS[conf.target.targetOS].scriptExt))

  if not writeRope(script, filename):
    conf.globalReport BackendReport(
      kind: rbackCannotWriteScript,
      filename: filename.string)

proc getOptSpeed(conf: ConfigRef; c: TSystemCC): string =
  result = getConfigVar(conf, c, ".options.speed")
  if result == "":
    result = CC[c].optSpeed   # use default settings from this file

proc getDebug(conf: ConfigRef; c: TSystemCC): string =
  result = getConfigVar(conf, c, ".options.debug")
  if result == "":
    result = CC[c].debug      # use default settings from this file

proc getOptSize(conf: ConfigRef; c: TSystemCC): string =
  result = getConfigVar(conf, c, ".options.size")
  if result == "":
    result = CC[c].optSize    # use default settings from this file

proc noAbsolutePaths(conf: ConfigRef): bool {.inline.} =
  # We used to check current OS != specified OS, but this makes no sense
  # really: Cross compilation from Linux to Linux for example is entirely
  # reasonable.
  # `optGenMapping` is included here for niminst.
  result = conf.globalOptions * {optGenScript, optGenMapping} != {}

proc cFileSpecificOptions(conf: ConfigRef; nimname, fullNimFile: string): string =
  ## Construct CLI options to to compile a specific nim file. Options are
  ## added in the following order:
  ## `[global(--passc)][opt=speed/size/debug][file-local][<nimname>.always]`.
  ## `<nimname>.always` is a configuration variable.
  result = conf.compileOptions

  for option in conf.compileOptionsCmd:
    if strutils.find(result, option, 0) < 0:
      addOpt(result, option)

  if optCDebug in conf.globalOptions:
    let key = nimname & ".debug"
    if existsConfigVar(conf, key): addOpt(result, getConfigVar(conf, key))
    else: addOpt(result, getDebug(conf, conf.cCompiler))
  if optOptimizeSpeed in conf.options:
    let key = nimname & ".speed"
    if existsConfigVar(conf, key): addOpt(result, getConfigVar(conf, key))
    else: addOpt(result, getOptSpeed(conf, conf.cCompiler))
  elif optOptimizeSize in conf.options:
    let key = nimname & ".size"
    if existsConfigVar(conf, key): addOpt(result, getConfigVar(conf, key))
    else: addOpt(result, getOptSize(conf, conf.cCompiler))
  let key = nimname & ".always"
  if existsConfigVar(conf, key): addOpt(result, getConfigVar(conf, key))

  addOpt(result, conf.cfileSpecificOptions.getOrDefault(fullNimFile))

proc getCompileOptions(conf: ConfigRef): string =
  result = cFileSpecificOptions(conf, "__dummy__", "__dummy__")

proc vccplatform(conf: ConfigRef): string =
  # VCC specific but preferable over the config hacks people
  # had to do before, see #11306
  if conf.cCompiler == ccVcc:
    let exe = getConfigVar(conf, conf.cCompiler, ".exe")
    if "vccexe.exe" == extractFilename(exe):
      result = case conf.target.targetCPU
        of cpuI386: " --platform:x86"
        of cpuArm: " --platform:arm"
        of cpuAmd64: " --platform:amd64"
        else: ""

proc getLinkOptions(conf: ConfigRef): string =
  result = conf.linkOptions & " " & conf.linkOptionsCmd.join(" ") & " "
  for linkedLib in items(conf.cLinkedLibs):
    result.add(CC[conf.cCompiler].linkLibCmd % linkedLib.quoteShell)
  for libDir in items(conf.cLibs):
    result.add(join([CC[conf.cCompiler].linkDirCmd, libDir.quoteShell]))

proc needsExeExt(conf: ConfigRef): bool {.inline.} =
  result = (optGenScript in conf.globalOptions and conf.target.targetOS == osWindows) or
           (conf.target.hostOS == osWindows)

proc envFlags(conf: ConfigRef): string =
  result = getEnv("CFLAGS")

proc getCompilerExe(conf: ConfigRef; compiler: TSystemCC; cfile: AbsoluteFile): string =
  let target = "c"
  if compiler == ccEnv:
    result = getEnv("CC")
  else:
    result = CC[compiler].compilerExe

  if result.len == 0:
    conf.globalReport BackendReport(
      kind: rbackTargetNotSupported,
      requestedTarget: target,
      usedCompiler:  CC[compiler].name)

proc ccHasSaneOverflow*(conf: ConfigRef): bool =
  if conf.cCompiler == ccGcc:
    result = false # assume an old or crappy GCC
    var exe = getConfigVar(conf, conf.cCompiler, ".exe")
    if exe.len == 0: exe = CC[conf.cCompiler].compilerExe
    let (s, exitCode) = try: execCmdEx(exe & " --version") except: ("", 1)
    if exitCode == 0:
      var i = 0
      var j = 0
      # the version is the last part of the first line:
      while i < s.len and s[i] != '\n':
        if s[i] in {' ', '\t'}: j = i+1
        inc i
      if j > 0:
        var major = 0
        while j < s.len and s[j] in {'0'..'9'}:
          major = major * 10 + (ord(s[j]) - ord('0'))
          inc j
        if i < s.len and s[j] == '.': inc j
        while j < s.len and s[j] in {'0'..'9'}:
          inc j
        if j+1 < s.len and s[j] == '.' and s[j+1] in {'0'..'9'}:
          # we found a third version number, chances are high
          # we really parsed the version:
          result = major >= 5
  else:
    result = conf.cCompiler == ccCLang

proc getLinkerExe(conf: ConfigRef; compiler: TSystemCC): string =
  result =
    if CC[compiler].linkerExe.len > 0:
      CC[compiler].linkerExe
    else:
      getCompilerExe(conf, compiler, AbsoluteFile"")

proc getCompileCFileCmd*(conf: ConfigRef; cfile: Cfile,
                         isMainFile = false; produceOutput = false): string =
  let c = conf.cCompiler
  # We produce files like module.nim.c, so the absolute Nim filename is not
  # cfile.name but `cfile.cname.changeFileExt("")`:
  var options = cFileSpecificOptions(conf, cfile.nimname, cfile.cname.changeFileExt("").string)

  var exe = getConfigVar(conf, c, ".exe")
  if exe.len == 0: exe = getCompilerExe(conf, c, cfile.cname)

  if needsExeExt(conf): exe = addFileExt(exe, "exe")
  if optGenDynLib in conf.globalOptions and
      ospNeedsPIC in platform.OS[conf.target.targetOS].props:
    options.add(' ' & CC[c].pic)

  if cfile.customArgs != "":
    options.add ' '
    options.add cfile.customArgs

  var compilePattern: string
  # compute include paths:
  var includeCmd = CC[c].includeCmd & quoteShell(conf.libpath)
  if not noAbsolutePaths(conf):
    for includeDir in items(conf.cIncludes):
      includeCmd.add(join([CC[c].includeCmd, includeDir.quoteShell]))

    compilePattern = joinPath(conf.cCompilerPath, exe)
  else:
    compilePattern = getCompilerExe(conf, c, cfile.cname)

  includeCmd.add(join([CC[c].includeCmd, quoteShell(conf.projectPath.string)]))

  let cf = if noAbsolutePaths(conf): AbsoluteFile extractFilename(cfile.cname.string)
           else: cfile.cname

  let objfile =
    if cfile.obj.isEmpty:
      if CfileFlag.External notin cfile.flags or noAbsolutePaths(conf):
        toObjFile(conf, cf).string
      else:
        completeCfilePath(conf, toObjFile(conf, cf)).string
    elif noAbsolutePaths(conf):
      extractFilename(cfile.obj.string)
    else:
      cfile.obj.string

  # D files are required by nintendo switch libs for
  # compilation. They are basically a list of all includes.
  let dfile = objfile.changeFileExt(".d").quoteShell

  let cfsh = quoteShell(cf)
  result = quoteShell(compilePattern % [
    "dfile", dfile,
    "file", cfsh, "objfile", quoteShell(objfile), "options", options,
    "include", includeCmd, "nim", getPrefixDir(conf).string,
    "lib", conf.libpath.string,
    "ccenvflags", envFlags(conf)])

  if optProduceAsm in conf.globalOptions:
    if CC[conf.cCompiler].produceAsm.len > 0:
      let asmfile = objfile.changeFileExt(".asm").quoteShell
      addOpt(result, CC[conf.cCompiler].produceAsm % ["asmfile", asmfile])
      if produceOutput:
        conf.localReport BackendReport(kind: rbackProducedAssembly, filename: asmfile)

    else:
      if produceOutput:
        conf.localReport BackendReport(
          kind: rbackCannotProduceAssembly,
          usedCompiler: CC[conf.cCompiler].name)

  result.add(' ')
  strutils.addf(result, CC[c].compileTmpl, [
    "dfile", dfile,
    "file", cfsh, "objfile", quoteShell(objfile),
    "options", options, "include", includeCmd,
    "nim", quoteShell(getPrefixDir(conf)),
    "lib", quoteShell(conf.libpath),
    "vccplatform", vccplatform(conf),
    "ccenvflags", envFlags(conf)])

proc footprint(conf: ConfigRef; cfile: Cfile): SecureHash =
  result = secureHash(
    $secureHashFile(cfile.cname.string) &
    platform.OS[conf.target.targetOS].name &
    platform.CPU[conf.target.targetCPU].name &
    extccomp.CC[conf.cCompiler].name &
    getCompileCFileCmd(conf, cfile))

proc externalFileChanged(conf: ConfigRef; cfile: Cfile): bool =
  if conf.backend == backendJs: return false # pre-existing behavior, but not sure it's good

  let hashFile = toGeneratedFile(conf, conf.withPackageName(cfile.cname), "sha1")
  let currentHash = footprint(conf, cfile)
  var f: File
  if open(f, hashFile.string, fmRead):
    let oldHash = parseSecureHash(f.readLine())
    close(f)
    result = oldHash != currentHash
  else:
    result = true
  if result:
    if open(f, hashFile.string, fmWrite):
      f.writeLine($currentHash)
      close(f)

proc addExternalFileToCompile*(conf: ConfigRef; c: var Cfile) =
  # we want to generate the hash file unconditionally
  let extFileChanged = externalFileChanged(conf, c)
  if optForceFullMake notin conf.globalOptions and fileExists(c.obj) and
      not extFileChanged:
    c.flags.incl CfileFlag.Cached
  else:
    # make sure Nim keeps recompiling the external file on reruns
    # if compilation is not successful
    discard tryRemoveFile(c.obj.string)
  conf.toCompile.add(c)

proc addExternalFileToCompile*(conf: ConfigRef; filename: AbsoluteFile) =
  var c = Cfile(nimname: splitFile(filename).name, cname: filename,
    obj: toObjFile(conf, completeCfilePath(conf, filename, false)),
    flags: {CfileFlag.External})
  addExternalFileToCompile(conf, c)

proc getLinkCmd(conf: ConfigRef; output: AbsoluteFile,
                objfiles: string, isDllBuild: bool, removeStaticFile: bool): string =
  if optGenStaticLib in conf.globalOptions:
    if removeStaticFile:
      removeFile output # fixes: bug #16947
    result = CC[conf.cCompiler].buildLib % ["libfile", quoteShell(output),
                                            "objfiles", objfiles]
  else:
    var linkerExe = getConfigVar(conf, conf.cCompiler, ".linkerexe")
    if linkerExe.len == 0: linkerExe = getLinkerExe(conf, conf.cCompiler)
    # bug #6452: We must not use ``quoteShell`` here for ``linkerExe``
    if needsExeExt(conf): linkerExe = addFileExt(linkerExe, "exe")
    if noAbsolutePaths(conf): result = linkerExe
    else: result = joinPath(conf.cCompilerPath, linkerExe)
    let buildgui = if optGenGuiApp in conf.globalOptions and conf.target.targetOS == osWindows:
                     CC[conf.cCompiler].buildGui
                   else:
                     ""
    let builddll = if isDllBuild: CC[conf.cCompiler].buildDll else: ""
    let exefile = quoteShell(output)

    when false:
      if optCDebug in conf.globalOptions:
        writeDebugInfo(exefile.changeFileExt("ndb"))

    # Map files are required by Nintendo Switch compilation. They are a list
    # of all function calls in the library and where they come from.
    let mapfile = quoteShell(getNimcacheDir(conf) / RelativeFile(splitFile(output).name & ".map"))

    let linkOptions = getLinkOptions(conf) & " " &
                      getConfigVar(conf, conf.cCompiler, ".options.linker")
    var linkTmpl = getConfigVar(conf, conf.cCompiler, ".linkTmpl")
    if linkTmpl.len == 0:
      linkTmpl = CC[conf.cCompiler].linkTmpl
    result = quoteShell(result % ["builddll", builddll,
        "mapfile", mapfile,
        "buildgui", buildgui, "options", linkOptions, "objfiles", objfiles,
        "exefile", exefile, "nim", getPrefixDir(conf).string, "lib", conf.libpath.string])
    result.add ' '
    strutils.addf(result, linkTmpl, ["builddll", builddll,
        "mapfile", mapfile,
        "buildgui", buildgui, "options", linkOptions,
        "objfiles", objfiles, "exefile", exefile,
        "nim", quoteShell(getPrefixDir(conf)),
        "lib", quoteShell(conf.libpath),
        "vccplatform", vccplatform(conf)])

  if optCDebug in conf.globalOptions and conf.cCompiler == ccVcc:
    result.add " /Zi /FS /Od"

template getLinkCmd(conf: ConfigRef; output: AbsoluteFile, objfiles: string,
                    removeStaticFile = false): string =
  getLinkCmd(conf, output, objfiles, optGenDynLib in conf.globalOptions, removeStaticFile)

template tryExceptOSErrorMessage(conf: ConfigRef; errorPrefix: string = "", body: untyped) =
  try:
    body
  except OSError:
    let ose = (ref OSError)(getCurrentException())
    conf.localReport CmdReport(
      kind: rcmdFailedExecution,
      msg: if 0 < len(errorPrefix): errorPrefix & " " & ose.msg else: ose.msg,
      code: ose.errorCode)

    raise

proc getExtraCmds(conf: ConfigRef; output: AbsoluteFile): seq[string] =
  when defined(macosx):
    if optCDebug in conf.globalOptions and optGenStaticLib notin conf.globalOptions:
      # if needed, add an option to skip or override location
      result.add "dsymutil " & $(output).quoteShell

proc execLinkCmd(conf: ConfigRef; linkCmd: string) =
  tryExceptOSErrorMessage(conf, "invocation of external linker program failed."):
    execExternalProgram(conf, linkCmd, rcmdLinking)

proc execCmdsInParallel(conf: ConfigRef; cmds: seq[string]; prettyCb: proc (idx: int)) =
  let runCb = proc (idx: int, p: Process) =
    let exitCode = p.peekExitCode
    if exitCode != 0:
      conf.localReport CmdReport(
        kind: rcmdFailedExecution, cmd: cmds[idx], code: exitCode)

  if conf.numberOfProcessors == 0:
    conf.numberOfProcessors = countProcessors()

  var res = 0
  if conf.numberOfProcessors <= 1:
    for i in 0..high(cmds):
      tryExceptOSErrorMessage(conf, "invocation of external compiler program failed."):
        res = execWithEcho(conf, cmds[i], rcmdExecuting)

      if res != 0:
        conf.localReport CmdReport(
          kind: rcmdFailedExecution, cmd: cmds[i], code: res)
  else:
    tryExceptOSErrorMessage(conf, "invocation of external compiler program failed."):
      res = execProcesses(cmds, {poStdErrToStdOut, poUsePath, poParentStreams},
                            conf.numberOfProcessors, beforeRunEvent=prettyCb,
                            afterRunEvent=runCb)

  if res != 0:
    if conf.numberOfProcessors <= 1:
      conf.localReport CmdReport(
        kind: rcmdFailedExecution, cmd: cmds.join(), code: res)

proc linkViaResponseFile(conf: ConfigRef; cmd: string) =
  # Extracting the linker.exe here is a bit hacky but the best solution
  # given ``buildLib``'s design.
  var i = 0
  var last = 0
  if cmd.len > 0 and cmd[0] == '"':
    inc i
    while i < cmd.len and cmd[i] != '"': inc i
    last = i
    inc i
  else:
    while i < cmd.len and cmd[i] != ' ': inc i
    last = i
  while i < cmd.len and cmd[i] == ' ': inc i
  let linkerArgs = conf.projectName & "_" & "linkerArgs.txt"
  let args = cmd.substr(i)
  # GCC's response files don't support backslashes. Junk.
  if conf.cCompiler == ccGcc or conf.cCompiler == ccCLang:
    writeFile(linkerArgs, args.replace('\\', '/'))
  else:
    writeFile(linkerArgs, args)
  try:
    execLinkCmd(conf, cmd.substr(0, last) & " @" & linkerArgs)
  finally:
    removeFile(linkerArgs)

proc getObjFilePath(conf: ConfigRef, f: Cfile): string =
  if noAbsolutePaths(conf): f.obj.extractFilename
  else: f.obj.string

proc displayProgressCC(conf: ConfigRef, path, compileCmd: string): string =
  if conf.hasHint(rcmdCompiling):
    conf.localReport CmdReport(
      kind: rcmdCompiling,
      cmd: compileCmd,
      msg: demanglePackageName(path.splitFile.name))

proc callCCompiler*(conf: ConfigRef) =
  var
    linkCmd: string
    extraCmds: seq[string]
  if conf.globalOptions * {optCompileOnly, optGenScript} == {optCompileOnly}:
    return # speed up that call if only compiling and no script shall be
           # generated
  #var c = cCompiler
  var script = ""
  var cmds: TStringSeq
  var prettyCmds: TStringSeq
  let prettyCb = proc (idx: int) = writePrettyCmdsStderr(prettyCmds[idx])

  for idx, it in conf.toCompile:
    # call the C compiler for the .c file:
    if CfileFlag.Cached in it.flags: continue
    let compileCmd = getCompileCFileCmd(
      conf, it, idx == conf.toCompile.len - 1, produceOutput=true)

    if optCompileOnly notin conf.globalOptions:
      cmds.add(compileCmd)
      prettyCmds.add displayProgressCC(conf, $it.cname, compileCmd)
    if optGenScript in conf.globalOptions:
      script.add(compileCmd)
      script.add("\n")

  if optCompileOnly notin conf.globalOptions:
    execCmdsInParallel(conf, cmds, prettyCb)
  if optNoLinking notin conf.globalOptions:
    # call the linker:
    var objfiles = ""
    for it in conf.externalToLink:
      let objFile = if noAbsolutePaths(conf): it.extractFilename else: it
      objfiles.add(' ')
      objfiles.add(quoteShell(
          addFileExt(objFile, CC[conf.cCompiler].objExt)))

    block:
      for x in conf.toCompile:
        let objFile = if noAbsolutePaths(conf): x.obj.extractFilename else: x.obj.string
        objfiles.add(' ')
        objfiles.add(quoteShell(objFile))
      let mainOutput = if optGenScript notin conf.globalOptions: conf.prepareToWriteOutput
                       else: AbsoluteFile(conf.projectName)

      linkCmd = getLinkCmd(conf, mainOutput, objfiles, removeStaticFile = true)
      extraCmds = getExtraCmds(conf, mainOutput)
      if optCompileOnly notin conf.globalOptions:
        const MaxCmdLen = when defined(windows): 8_000 else: 32_000
        if linkCmd.len > MaxCmdLen:
          # Windows's command line limit is about 8K (don't laugh...) so C compilers on
          # Windows support a feature where the command line can be passed via ``@linkcmd``
          # to them.
          linkViaResponseFile(conf, linkCmd)
        else:
          execLinkCmd(conf, linkCmd)
        for cmd in extraCmds:
          execExternalProgram(conf, cmd, rcmdExecuting)
  else:
    linkCmd = ""
  if optGenScript in conf.globalOptions:
    script.add(linkCmd)
    script.add("\n")
    generateScript(conf, script)

template hashNimExe(): string = $secureHashFile(os.getAppFilename())

proc jsonBuildInstructionsFile*(conf: ConfigRef): AbsoluteFile =
  # `outFile` is better than `projectName`, as it allows having different json
  # files for a given source file compiled with different options; it also
  # works out of the box with `hashMainCompilationParams`.
  result = getNimcacheDir(conf) / conf.outFile.changeFileExt("json")

const cacheVersion = "D20230310T000000" # update when `BuildCache` spec changes
type BuildCache = object
  cacheVersion: string
  outputFile: string
  compile: seq[(string, string)]
  link: seq[string]
  linkcmd: string
  extraCmds: seq[string]
  configFiles: seq[string] # the hash shouldn't be needed
  inputMode: ProjectInputMode
  currentDir: string
  cmdline: string
  depfiles: seq[(string, string)]
  nimexe: string

proc writeJsonBuildInstructions*(conf: ConfigRef) =
  var linkFiles = collect(for it in conf.externalToLink:
    var it = it
    if conf.noAbsolutePaths: it = it.extractFilename
    it.addFileExt(CC[conf.cCompiler].objExt))
  for it in conf.toCompile: linkFiles.add it.obj.string
  var bcache = BuildCache(
    cacheVersion: cacheVersion,
    outputFile: conf.absOutFile.string,
    compile: collect(for i, it in conf.toCompile:
      if CfileFlag.Cached notin it.flags: (it.cname.string, getCompileCFileCmd(conf, it))),
    link: linkFiles,
    linkcmd: getLinkCmd(conf, conf.absOutFile, linkFiles.quoteShellCommand),
    extraCmds: getExtraCmds(conf, conf.absOutFile),
    inputMode: conf.inputMode,
    configFiles: conf.configFiles.mapIt(it.string),
    currentDir: getCurrentDir())
  if optRun in conf.globalOptions or isDefined(conf, "nimBetterRun"):
    bcache.cmdline = conf.commandLine
    bcache.depfiles = collect(for it in conf.m.fileInfos:
      let path = it.fullPath.string
      if isAbsolute(path): # TODO: else?
        (path, $secureHashFile(path)))
    bcache.nimexe = hashNimExe()
  conf.jsonBuildFile = conf.jsonBuildInstructionsFile
  conf.jsonBuildFile.string.writeFile(bcache.toJson.pretty)

proc changeDetectedViaJsonBuildInstructions*(conf: ConfigRef; jsonFile: AbsoluteFile): bool =
  if not fileExists(jsonFile) or not fileExists(conf.absOutFile): return true
  var bcache: BuildCache
  try: bcache.fromJson(jsonFile.string.parseFile)
  except IOError, OSError, ValueError:
    echo getCurrentException().msg
    stderr.write "Warning: JSON processing failed for: $#\n" % jsonFile.string
    return true
  if bcache.currentDir != getCurrentDir() or # fixes bug #16271
     bcache.configFiles != conf.configFiles.mapIt(it.string) or
     bcache.cacheVersion != cacheVersion or bcache.outputFile != conf.absOutFile.string or
     bcache.cmdline != conf.commandLine or bcache.nimexe != hashNimExe() or
     bcache.inputMode != conf.inputMode: return true
  if bcache.inputMode != pimFile: return true
    # xxx optimize by returning false if stdin input was the same
  for (file, hash) in bcache.depfiles:
    if $secureHashFile(file) != hash: return true

proc runJsonBuildInstructions*(conf: ConfigRef; jsonFile: AbsoluteFile) =
  var bcache: BuildCache
  try: bcache.fromJson(jsonFile.string.parseFile)
  except:
    let e = getCurrentException()
    conf.quitOrRaise "\ncaught exception:\n$#\nstacktrace:\n$#error evaluating JSON file: $#" %
      [e.msg, e.getStackTrace(), jsonFile.string]
  let output = bcache.outputFile
  createDir output.parentDir
  let outputCurrent = $conf.absOutFile
  if output != outputCurrent or bcache.cacheVersion != cacheVersion:
    conf.globalReport BackendReport(
      kind: rbackJsonScriptMismatch,
      jsonScriptParams: (outputCurrent, output, jsonFile.string))

  var cmds, prettyCmds: TStringSeq
  let prettyCb = proc (idx: int) = writePrettyCmdsStderr(prettyCmds[idx])
  for (name, cmd) in bcache.compile:
    cmds.add cmd
    prettyCmds.add displayProgressCC(conf, name, cmd)

  execCmdsInParallel(conf, cmds, prettyCb)
  execLinkCmd(conf, bcache.linkcmd)

  for cmd in bcache.extraCmds:
    execExternalProgram(conf, cmd, rcmdExecuting)

proc genMappingFiles(conf: ConfigRef; list: CfileList): Rope =
  for it in list:
    ropes.addf(result, "--file:r\"$1\"$N", [rope(it.cname.string)])

proc writeMapping*(conf: ConfigRef; symbolMapping: Rope) =
  if optGenMapping notin conf.globalOptions: return
  var code = rope("[C_Files]\n")
  code.add(genMappingFiles(conf, conf.toCompile))
  code.add("\n[C_Compiler]\nFlags=")
  code.add(strutils.escape(getCompileOptions(conf)))

  code.add("\n[Linker]\nFlags=")
  code.add(strutils.escape(getLinkOptions(conf) & " " &
                            getConfigVar(conf, conf.cCompiler, ".options.linker")))

  code.add("\n[Environment]\nlibpath=")
  code.add(strutils.escape(conf.libpath.string))

  ropes.addf(code, "\n[Symbols]$n$1", [symbolMapping])
  let filename = getNimcacheDir(conf) / RelativeFile"mapping.txt"
  if not writeRope(code, filename):
    conf.localReport BackendReport(
      kind: rbackCannotWriteMappingFile, filename: filename.string)
