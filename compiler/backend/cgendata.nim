#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the data structures for the C code generation phase.

import
  std/[
    intsets,
    tables,
    sets
  ],
  compiler/ast/[
    ast,
    lineinfos,
    ndi
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options
  ],
  compiler/utils/[
    containers,
    ropes,
    pathutils
  ]


type
  SymbolMap*[T] = object
    ## Associates extra location-related data with symbols. This is
    ## temporary scaffolding until each entity (type, local, procedure,
    ## etc.) is consistently represented as an index-like handle in the
    ## code generator, at which point a ``Store`` (or ``SeqMap``) can be
    ## used directly.
    ##
    ## Mapping from a symbol to the associated data currently happens via
    ## ``TSym.locId``.
    store: Store[range[0'u32..high(uint32)-1], T]

  TLocKind* = enum
    locNone,                  ## no location
    locTemp,                  ## temporary location
    locLocalVar,              ## location is a local variable
    locGlobalVar,             ## location is a global variable
    locParam,                 ## location is a parameter
    locField,                 ## location is a record field
    locExpr,                  ## "location" is really an expression
    locProc,                  ## location is a proc (an address of a procedure)
    locData,                  ## location is a constant
    locCall,                  ## location is a call expression
    locOther                  ## location is something other

  TStorageLoc* = enum
    # XXX: ``TStorageLoc`` is obsolete -- remove it
    OnUnknown,                ## location is unknown (stack, heap or static)
    OnStatic,                 ## in a static section
    OnStack,                  ## location is on hardware stack
    OnHeap                    ## location is on heap or global
                              ## (reference counting needed)

  TLoc* = object
    k*: TLocKind              ## kind of location
    storage*: TStorageLoc
    flags*: TLocFlags         ## location's flags
    lode*: PNode              ## Node where the location came from; can be faked
    r*: Rope                  ## rope value of location (code generators)

  TLabel* = Rope              ## for the C generator a label is just a rope
  TCFileSection* = enum       ## the sections a generated C file consists of
    cfsMergeInfo,             ## section containing merge information
    cfsHeaders,               ## section for C include file headers
    cfsFrameDefines           ## section for nim frame macros
    cfsForwardTypes,          ## section for C forward typedefs
    cfsTypes,                 ## section for C typedefs
    cfsSeqTypes,              ## section for sequence types only
                              ## this is needed for strange type generation
                              ## reasons
    cfsFieldInfo,             ## section for field information
    cfsTypeInfo,              ## section for type information (ag ABI checks)
    cfsProcHeaders,           ## section for C procs prototypes
    cfsData,                  ## section for C constant data
    cfsVars,                  ## section for C variable declarations
    cfsProcs,                 ## section for C procs that are not inline
    cfsInitProc,              ## section for the C init proc
    cfsDatInitProc,           ## section for the C datInit proc
    cfsTypeInit1,             ## section 1 for declarations of type information
    cfsTypeInit2,             ## section 2 for init of type information
    cfsTypeInit3,             ## section 3 for init of type information
    cfsDebugInit,             ## section for init of debug information
    cfsDynLibInit,            ## section for init of dynamic library binding
    cfsDynLibDeinit           ## section for deinitialization of dynamic
                              ## libraries
  TCTypeKind* = enum          ## describes the type kind of a C type
    ctVoid, ctChar, ctBool,
    ctInt, ctInt8, ctInt16, ctInt32, ctInt64,
    ctFloat, ctFloat32, ctFloat64, ctFloat128,
    ctUInt, ctUInt8, ctUInt16, ctUInt32, ctUInt64,
    ctArray, ctPtrToArray, ctStruct, ctPtr, ctNimStr, ctNimSeq, ctProc,
    ctNimOpenArray,
    ctCString
  TCFileSections* = array[TCFileSection, Rope] ## represents a generated C file
  TCProcSection* = enum       ## the sections a generated C proc consists of
    cpsLocals,                ## section of local variables for C proc
    cpsInit,                  ## section for init of variables for C proc
    cpsStmts                  ## section of local statements for C proc
  TCProcSections* = array[TCProcSection, Rope] # represents a generated C proc
  BModule* = ref TCGen
  BProc* = ref TCProc
  TBlock* = object
    id*: int                  ## the ID of the label; positive means that it
    label*: Rope              ## generated text for the label
                              ## nil if label is not used
    sections*: TCProcSections ## the code belonging
    isLoop*: bool             ## whether block is a loop
    nestedTryStmts*: int16    ## how many try statements is it nested into
    nestedExceptStmts*: int16 ## how many except statements is it nested into
    frameLen*: int16

  TCProcFlag* = enum
    beforeRetNeeded,
    threadVarAccessed,
    hasCurFramePointer,
    nimErrorFlagAccessed,
    nimErrorFlagDeclared,
    nimErrorFlagDisabled

  TCProc = object             ## represents C proc that is currently generated
    prc*: PSym                ## the Nim proc that this C proc belongs to
    flags*: set[TCProcFlag]
    lastLineInfo*: TLineInfo  ## to avoid generating excessive 'nimln' statements
    currLineInfo*: TLineInfo  ## AST codegen will make this superfluous
    nestedTryStmts*: seq[tuple[fin: PNode, inExcept: bool, label: Natural]]
                              ## in how many nested try statements we are
                              ## (the vars must be volatile then)
                              ## bool is true when are in the except part of a try block
    labels*: Natural          ## for generating unique labels in the C proc
    blocks*: seq[TBlock]      ## nested blocks
    breakIdx*: int            ## the block that will be exited
                              ## with a regular break
    options*: TOptions        ## options that should be used for code
                              ## generation; this is the same as prc.options
                              ## unless prc == nil
    module*: BModule          ## used to prevent excessive parameter passing
    withinLoop*: int          ## > 0 if we are within a loop
    withinTryWithExcept*: int ## required for goto based exception handling
    withinBlockLeaveActions*: int ## complex to explain
    sigConflicts*: CountTable[string]

    locals*: SymbolMap[TLoc]  ## the locs for all locals of the procedure

  TTypeSeq* = seq[PType]
  TypeCache* = Table[SigHash, Rope]
  TypeCacheWithOwner* = Table[SigHash, tuple[str: Rope, owner: int32]]

  CodegenFlag* = enum
    preventStackTrace,  ## true if stack traces need to be prevented
    usesThreadVars,     ## true if the module uses a thread var
    frameDeclared,      ## hack for ROD support so that we don't declare
                        ## a frame var twice in an init proc
    isHeaderFile,       ## C source file is the header file
    includesStringh,    ## C source file already includes ``<string.h>``

  BModuleList* = ref object of RootObj
    mapping*: Rope             ## the generated mapping file (if requested)
    modules*: seq[BModule]     ## list of all compiled modules
    modulesClosed*: seq[BModule] ## list of the same compiled modules, but in the order they were closed
    generatedHeader*: BModule
    typeInfoMarker*: TypeCacheWithOwner
    typeInfoMarkerV2*: TypeCacheWithOwner
    config*: ConfigRef
    graph*: ModuleGraph

    nimtv*: Rope            ## Nim thread vars; the struct body
    nimtvDeps*: seq[PType]  ## type deps: every module needs whole struct
    nimtvDeclared*: IntSet  ## so that every var/field exists only once
                            ## in the struct
                            ## 'nimtv' is incredibly hard to modularize! Best
                            ## effort is to store all thread vars in a ROD
                            ## section and with their type deps and load them
                            ## unconditionally...
                            ## nimtvDeps is VERY hard to cache because it's
                            ## not a list of IDs nor can it be made to be one.

    globals*: SymbolMap[TLoc]
      ## the locs for all alive globals of the program
    consts*: SymbolMap[TLoc]
      ## the locs for all alive constants of the program
    procs*: SymbolMap[tuple[loc: TLoc, params: seq[TLoc]]]
      ## the locs for all alive procedure of the program. Also stores the
      ## locs for the parameters
    fields*: SymbolMap[TLoc]
      ## the locs for all fields

    hooks*: seq[(BModule, PSym)]
      ## late late-dependencies. Generating code for a procedure might lead
      ## to the RTTI setup code for some type from a foreign module (i.e., one
      ## different from the module that acts as the current context) to be
      ## emitted, and this setup code might reference additional procedures.
      ## Written: by the code generator and orchestrator; reset by the
      ##          orchestrator
      ## Read:    by the orchestrator
      # XXX: move emission of RTTI setup into the orchestrator and remove this
      #      facility

  TCGen = object ## represents a C source file
    idgen*: IdGenerator
    s*: TCFileSections        ## sections of the C file
    flags*: set[CodegenFlag]
    module*: PSym
    filename*: AbsoluteFile
    cfilename*: AbsoluteFile  ## filename of the module (including path,
                              ## without extension)
    tmpBase*: Rope            ## base for temp identifier generation
    typeCache*: TypeCache     ## cache the generated types
    typeABICache*: HashSet[SigHash] ## cache for ABI checks; reusing typeCache
                              ## would be ideal but for some reason enums
                              ## don't seem to get cached so it'd generate
                              ## 1 ABI check per occurence in code
    forwTypeCache*: TypeCache ## cache for forward declarations of types
    declaredThings*: IntSet   ## things we have declared in this .c file
    declaredProtos*: IntSet   ## prototypes we have declared in this .c file
    headerFiles*: seq[string] ## needed headers to include
    typeInfoMarker*: TypeCache ## needed for generating type information
    typeInfoMarkerV2*: TypeCache
    typeStack*: TTypeSeq      ## used for type generation
    dataCache*: TNodeTable
    typeNodes*: int ## used for type info generation
    typeNodesName*: Rope ## used for type info generation
    labels*: Natural          ## for generating unique module-scope names
    sigConflicts*: CountTable[SigHash]
    g*: BModuleList
    ndi*: NdiFile

    extra*: seq[PSym]
      ## communicates dependencies introduced by the code-generator
      ## back to the caller. The caller is responsible for clearing the list
      ## after it's done with processing it. The code-generator only ever
      ## appends to it

template config*(m: BModule): ConfigRef = m.g.config
template config*(p: BProc): ConfigRef = p.module.g.config

template procs*(m: BModule): untyped   = m.g.procs
template fields*(m: BModule): untyped  = m.g.fields
template globals*(m: BModule): untyped = m.g.globals
template consts*(m: BModule): untyped  = m.g.consts

template fieldLoc*(p: BProc, field: PSym): TLoc =
  ## Returns the loc for the given `field`.
  p.module.fields[field]

template params*(p: BProc): seq[TLoc] =
  ## Returns the mutable list with the locs of `p`'s
  ## parameters.
  p.module.procs[p.prc].params

proc includeHeader*(this: BModule; header: string) =
  if not this.headerFiles.contains header:
    this.headerFiles.add header

proc s*(p: BProc, s: TCProcSection): var Rope {.inline.} =
  # section in the current block
  result = p.blocks[^1].sections[s]

proc procSec*(p: BProc, s: TCProcSection): var Rope {.inline.} =
  # top level proc sections
  result = p.blocks[0].sections[s]

proc newProc*(prc: PSym, module: BModule): BProc =
  new(result)
  result.prc = prc
  result.module = module
  result.options = if prc != nil: prc.options
                   else: module.config.options
  newSeq(result.blocks, 1)
  result.nestedTryStmts = @[]
  result.sigConflicts = initCountTable[string]()

proc newModuleList*(g: ModuleGraph): BModuleList =
  BModuleList(typeInfoMarker: initTable[SigHash, tuple[str: Rope, owner: int32]](),
    config: g.config, graph: g, nimtvDeclared: initIntSet())

iterator cgenModules*(g: BModuleList): BModule =
  for m in g.modulesClosed:
    # iterate modules in the order they were closed
    yield m

proc put*[T](m: var SymbolMap[T], sym: PSym, it: sink T) {.inline.}  =
  ## Adds `it` to `m` and registers a mapping between the item and
  ## `sym`. `sym` must have no mapping registered yet.
  assert sym.locId == 0, "symbol already registered"
  sym.locId = uint32(m.store.add(it)) + 1

proc forcePut*[T](m: var SymbolMap[T], sym: PSym, it: sink T) {.inline.} =
  ## Adds `it` to `m` and register a mapping between the item and
  ## `sym`, overwriting any existing mappings of `sym`.
  sym.locId = uint32(m.store.add(it)) + 1

func assign*[T](m: var SymbolMap[T], sym: PSym, it: sink T) {.inline.}  =
  ## Sets the value of the item in `m` with which `sym` is associated. This is
  ## only meant as a workaround.
  assert sym.locId > 0
  m.store[sym.locId - 1] = it

func `[]`*[T](m: SymbolMap[T], sym: PSym): lent T {.inline.} =
  m.store[sym.locId - 1]

func `[]`*[T](m: var SymbolMap[T], sym: PSym): var T {.inline.} =
  m.store[sym.locId - 1]

func contains*[T](m: SymbolMap[T], sym: PSym): bool {.inline.} =
  sym.locId > 0 and m.store.nextId().uint32 > sym.locId - 1

iterator items*[T](m: SymbolMap[T]): lent T =
  for it in m.store.items:
    yield it