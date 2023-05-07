## This module contains defintions of all types and configuration objects
## that are used to determine nim compiler inputs.

import compiler/utils/[pathutils, platform]
import compiler/ast/[report_enums]
import std/[sets, strtabs]

type
  TGlobalOption* = enum
    gloptNone
    optForceFullMake
    optWasNimscript           ## redundant with `cmdNimscript`, could be
                              ## removed
    optListCmd
    optCompileOnly
    optNoLinking
    optCDebug                 ## turn on debugging information
    optGenDynLib              ## generate a dynamic library
    optGenStaticLib           ## generate a static library
    optGenGuiApp              ## generate a GUI application
    optGenScript              ## generate a script file to compile the *.c
                              ## files
    optGenMapping             ## generate a mapping file
    optRun                    ## run the compiled project
    optUseNimcache            ## save artifacts (including binary) in $nimcache
    optStyleHint              ## check that the names adhere to NEP-1
    optStyleError             ## enforce that the names adhere to NEP-1
    optStyleUsages            ## only enforce consistent **usages** of the
                              ## symbol
    optSkipSystemConfigFile   ## skip the system's cfg/nims config file
    optSkipProjConfigFile     ## skip the project's cfg/nims config file
    optSkipUserConfigFile     ## skip the users's cfg/nims config file
    optSkipParentConfigFiles  ## skip parent dir's cfg/nims config files
    optNoMain                 ## do not generate a "main" proc
    optUseColors              ## use colors for hints, warnings, and errors
    optThreads                ## support for multi-threading
    optStdout                 ## output to stdout
    optThreadAnalysis         ## thread analysis pass
    optTlsEmulation           ## thread var emulation turned on
    optGenIndex               ## generate index file for documentation;
    optEmbedOrigSrc           ## embed the original source in the generated
                              ## code also: generate header file
    optIdeDebug               ## idetools: debug mode
    optIdeTerse               ## idetools: use terse descriptions
    optExcessiveStackTrace    ## fully qualified module filenames
    optShowAllMismatches      ## show all overloading resolution candidates
    optWholeProject           ## for 'doc': output any dependency
    optDocInternal            ## generate documentation for non-exported
                              ## symbols
    optDeclaredLocs           ## show declaration locations in messages
    optNoNimblePath
    optDynlibOverrideAll
    optSeqDestructors         ## active if the implementation uses the new
                              ## string/seq implementation based on destructors
    optTinyRtti               ## active if we use the new "tiny RTTI"
                              ## implementation
    optMultiMethods
    optBenchmarkVM            ## Enables cpuTime() in the VM
    optProduceAsm             ## produce assembler code
    optPanics                 ## turn panics (sysFatal) into a process
                              ## termination
    optSourcemap
    optProfileVM              ## enable VM profiler
    optEnableDeepCopy         ## ORC specific: enable 'deepcopy' for all types
    optCmdExitGcStats         ## print gc stats as part of command exit

  TGlobalOptions* = set[TGlobalOption]

  TGCMode* = enum             # the selected GC
    gcUnselected = "unselected"
    gcNone = "none"
    gcNative = "native" ## use the memory management native to the backend
    gcBoehm = "boehm"
    gcRegions = "regions"
    gcArc = "arc"
    gcOrc = "orc"
    gcMarkAndSweep = "markAndSweep"
    gcHooks = "hooks"
    gcRefc = "refc"
    gcV2 = "v2"
    gcGo = "go"
    # gcRefc and the GCs that follow it use a write barrier, as far as
    # usesWriteBarrier() is concerned

  TOption* = enum
    ##

    # please make sure we have under 32 options (improves code efficiency
    # a lot!) **keep binary compatible**.
    optNone
    optObjCheck ## `ccgenexprs.nim` generates `isObj` check if this options
                ## is enabled for a procedure
    optFieldCheck ## Codegen uses it to conditionally generate check for a
                  ## discriminant field
    optRangeCheck ## Control generation of range checks in the backend
    optBoundsCheck ## Control generation of the array boundary checks in
                   ## the backend
    optOverflowCheck ## Integer overflow check control
    optNaNCheck ## Raise float invalid defect C backend if operation
                ## returned nan
    optInfCheck ## Raise float overflow in C backend if operation reaturned
                ## inf
    optStaticBoundsCheck
    optStyleCheck ## Check symbol for spelling consistency
    optAssert
    optLineDir
    optWarns
    optHints
    optOptimizeSpeed
    optOptimizeSize
    optStackTrace     ## stack tracing support
    optStackTraceMsgs ## enable custom runtime msgs via `setFrameMsg`
    optLineTrace      ## line tracing support (includes stack tracing)
    optByRef          ## use pass by ref for objects
                      ## (for interfacing with C)
    optProfiler       ## profiler turned on
    optImplicitStatic ## optimization: implicit at compile time
                      ## evaluation
    optTrMacros       ## en/disable pattern matching
    optMemTracker
    optSinkInference  ## 'sink T' inference
    optCursorInference
    optImportHidden

  TOptions* = set[TOption]

  Command* = enum
    ## Compiler execution command
    cmdNone        ## not yet processed command
    cmdUnknown     ## command unmapped
    cmdCompileToC
    cmdCompileToJS
    cmdCompileToVM
    cmdCrun        ## compile and run in nimache
    cmdTcc         ## run the project via TCC backend
    cmdCheck       ## semantic checking for whole project
    cmdParse       ## parse a single file (for debugging)
    cmdScan        ## scan/lexically analyse a single file (for debugging)
    cmdRod         ## .rod to some text representation (for debugging)
    cmdIdeTools    ## ide tools (e.g. nimsuggest)
    cmdNimscript   ## evaluate nimscript
    cmdDoc         ## convert .nim doc comments to HTML
    cmdDoc2tex     ## convert .nim doc comments to LaTeX
    cmdRst2html    ## convert a reStructuredText file to HTML
    cmdRst2tex     ## convert a reStructuredText file to TeX
    cmdJsondoc
    cmdCtags
    cmdBuildindex
    cmdGendepend
    cmdDump
    cmdInteractive ## start interactive session
    cmdNop
    cmdJsonscript  ## compile a .json build file
    cmdNimfix
    # old unused: cmdInterpret, cmdDef: def feature (find definition for IDEs)

  FilenameOption* = enum
    ## Filename formatting option
    foAbs           ## absolute path, e.g.: /pathto/bar/foo.nim
    foRelProject    ## relative to project path, e.g.: ../foo.nim
    foCanonical     ## canonical module name
    foLegacyRelProj ## legacy, shortest of (foAbs, foRelProject)
    foName          ## lastPathPart, e.g.: foo.nim
    foStacktrace    ## if optExcessiveStackTrace: foAbs else: foName

  TSystemCC* = enum
    ccNone, ccGcc, ccNintendoSwitch, ccLLVM_Gcc, ccCLang, ccBcc, ccVcc,
    ccTcc, ccEnv, ccIcl, ccIcc, ccClangCl

  ExceptionSystem* = enum
    excNone,   ## no exception system selected yet
    excNative, ## use backend native exception handling
    excGoto,   ## exception handling based on goto

  SymbolFilesOption* = enum
    disabledSf   ## disables Rod files and maybe packed AST features
    writeOnlySf  ## not really sure, beyond not reading rod files
    readOnlySf   ## we only read from rod files
    v2Sf         ## who knows, probably a bad idea
    stressTest   ## likely more bad ideas

type
  TBackend* = enum
    ## Target compilation backend
    backendInvalid = "" # for parseEnum
    backendC = "c"
    backendJs = "js"
    backendNimVm = "vm"
    # backendNimscript = "nimscript" # this could actually work
    # backendLlvm = "llvm" # probably not well supported; was cmdCompileToLLVM
  TValidBackend* = range[backendC .. backendJs]

const validBackends*: set[TValidBackend] = {backendC .. backendJs}

type
  # "reports" strikes again, this bit of silliness is to stop reports from
  # infecting the `commands` module among others.
  MsgFormatKind* = enum
    msgFormatText = "text" ## text legacy reports message formatting
    msgFormatSexp = "sexp" ## sexp legacy reports message formatting

type
  Feature* = enum  ## experimental features; DO NOT RENAME THESE!
    implicitDeref,
    dotOperators,
    callOperator,
    destructor,
    notnil,
    vmopsDanger,
    strictFuncs,
    views,
    strictNotNil,
    overloadableEnums,
    strictEffects,
    unicodeOperators

const experimentalFeatures*: set[Feature] = {implicitDeref..unicodeOperators}

type
  ConfNoteSet* = enum
    cnCurrent ## notes after resolving all logic(defaults,
              ## verbosity)/cmdline/configs
    cnMainPackage
    cnForeign
    cnWarnAsError
    cnHintAsError
    cnCmdline ## notes that have been set/unset from cmdline
    cnModifiedy ## notes that have been set/unset from either
                ## cmdline/configs

type
  CurrentConf* = object
    ## Active, 'input' compiler configuration that controls behavior of the
    ## system.
    backend*: TBackend ## set via `nim x` or `nim --backend:x`
    target*: Target       # (+)
    localOptions*: TOptions ## Localized configuration options - they can
    ## be set via command-line or using region-local pragmas.
    globalOptions*: TGlobalOptions ## Global configuration options that can
    ## only be supplied from the command line or the configuration files.
    cppDefines*: HashSet[string] #[ (*) ]# ## C pre-processor defines
    features*: set[Feature]

    symbolFiles*: SymbolFilesOption
    symbols*: StringTableRef ## We need to use a StringTableRef here as
    ## defined symbols are always guaranteed to be style insensitive.
    ## Otherwise hell would break lose.
    prefixDir*: AbsoluteDir
    nimcacheDir*: AbsoluteDir ## Directory to write temporary generated
    ## files to.

    libpath*: AbsoluteDir ## Path to the standard library
    nimblePaths*: seq[AbsoluteDir] ## List of provided `--nimblePath`
    ## directories
    searchPaths*: seq[AbsoluteDir] ## Explicitly added list of the search
    ## paths for modules. Those are queried first.
    lazyPaths*: seq[AbsoluteDir] ## Implicitly constructed list of the
    ## search paths for modules. Updated when `--nimblePath` option is
    ## provided, and consists of explicitly provided nimble paths to the
    ## found package directories. Last part allows to specify directory for
    ## packages and avoid specifying `--path` for every single one of them.


    macrosToExpand*: StringTableRef ## Table of the target macros to expand.
    # Used as set for some reason, probably should actually be a set.
    arcToExpand*: StringTableRef ## Table of function names to expand arc for
    cmd*: Command        ## raw command parsed as enum
    selectedGC*: TGCMode ## the selected GC (+)
    exc*: ExceptionSystem ## Selected exception system
    cCompiler*: TSystemCC ## the used compiler
    filenameOption*: FilenameOption # how to render paths in compiler messages

    noteSets*: array[ConfNoteSet, ReportKinds] ## All note sets used for
    ## compilation. Active note set (`ConfNoteSet.cnCurrent`) can be
    ## swapped (depending on the context - push/pop, target package) or
    ## modified (via user configuration, command-line flags)

    isVmTrace*: bool ## Whether runtime vm tracing is enabled or not

    numberOfProcessors*: int  ## number of processors. Can be set using
    ## `--parallelbuild`, otherwise defaults to number of processors.

    outFile*: RelativeFile
    outDir*: AbsoluteDir
    depfile*: AbsoluteFile

    implicitImports*: seq[string]  ## modules that are to be implicitly
                                   ## imported
    implicitIncludes*: seq[string] ## modules that are to be implicitly
                                   ## included

    cIncludes*: seq[AbsoluteDir]  ## directories to search for included files
    cLibs*: seq[AbsoluteDir]      ## directories to search for lib files
    cLinkedLibs*: seq[string]     ## libraries to link

    dllOverrides*: StringTableRef ## `--dynliboverride`

    # These fields have weird interlinked interactions - none of them are
    # simply 'what was supplied as an argument'. `projectFull` is the
    # closest one to that, but it still goes through the process of
    # canonicialization in `options.setFromProjectName`
    projectPath*: AbsoluteDir       ## holds a path like
    projectName*: string            ## holds a name like 'nim'
    ## /home/alice/projects/nim/compiler/
    projectFull*: AbsoluteFile      ## projectPath/projectName


    linkOptionsCmd*: seq[string] ## options passed from `passl` on the
                                 ## command line.
    compileOptionsCmd*: seq[string] ## `passc` on the command line.
    ## Compilation options that would be used for every single file. They
    ## are placed in front of the file-specific options.

    configVars*: StringTableRef ## Additional configuration variables for
    ## providing extra options for different compiler subsystems.

func flip*[I](s: var set[I], it: I, val: bool) =
  if val: s.incl it else: s.excl it
