#
#
#           The Nim Compiler
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains the type definitions for the new evaluation engine.
## An instruction is 1-3 int32s in memory, it is a register based VM.

import std/tables

import
  compiler/ast/[
    ast,
    idents,
    lineinfos,
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/front/[
    options,
  ],
  compiler/utils/[
    debugutils,
  ]

import std/options as std_options

import vm_enums
export vm_enums

type TInstrType* = uint64

const
  regOBits = 8 # Opcode
  regABits = 16
  regBBits = 16
  regCBits = 16
  regBxBits = 24

  byteExcess* = 128 # we use excess-K for immediates

# Calculate register shifts, masks and ranges

const
  regOShift* = 0.TInstrType
  regAShift* = (regOShift + regOBits)
  regBShift* = (regAShift + regABits)
  regCShift* = (regBShift + regBBits)
  regBxShift* = (regAShift + regABits)

  regOMask*  = ((1.TInstrType shl regOBits) - 1)
  regAMask*  = ((1.TInstrType shl regABits) - 1)
  regBMask*  = ((1.TInstrType shl regBBits) - 1)
  regCMask*  = ((1.TInstrType shl regCBits) - 1)
  regBxMask* = ((1.TInstrType shl regBxBits) - 1)

  wordExcess* = 1 shl (regBxBits-1)
  regBxMin* = -wordExcess+1
  regBxMax* =  wordExcess-1

type
  PrgCtr* = int  ## Program Counter, aliased as it's self-documenting and supports
                 ## changing the type in the future (distinct/width)

  TRegister* = range[0..regAMask.int]
  TDest* = range[-1..regAMask.int]
  TInstr* = distinct TInstrType


  TBlock* = object
    label*: PSym
    fixups*: seq[TPosition]

  TEvalMode* = enum           ## reason for evaluation
    emRepl,                   ## evaluate because in REPL mode
    emConst,                  ## evaluate for 'const' according to spec
    emOptimize,               ## evaluate for optimization purposes (same as
                              ## emConst?)
    emStaticExpr,             ## evaluate for enforced compile time eval
                              ## ('static' context)
    emStaticStmt              ## 'static' as an expression
    emStandalone              ## standalone execution separate from
                              ## code-generation

  TSandboxFlag* = enum        ## what the evaluation engine should allow
    allowCast,                ## allow unsafe language feature: 'cast'
    allowInfiniteLoops        ## allow endless loops
  TSandboxFlags* = set[TSandboxFlag]

  TSlotKind* = enum   # We try to re-use slots in a smart way to
                      # minimize allocations; however the VM supports arbitrary
                      # temporary slot usage. This is required for the parameter
                      # passing implementation.
    slotEmpty,        ## slot is unused
    slotFixedVar,     ## slot is used for a fixed var/result (requires copy then)
    slotFixedLet,     ## slot is used for a fixed param/let
    slotTempUnknown,  ## slot but type unknown (argument of proc call)
    slotTempInt,      ## some temporary int
    slotTempFloat,    ## some temporary float
    slotTempStr,      ## some temporary string
    slotTempComplex,  ## some complex temporary (s.node field is used)
    slotTempHandle,   ## some temporary handle into an object/array
    slotTempPerm      ## slot is temporary but permanent (hack)

  CodeInfo* = tuple
    start: int ## The position where the bytecode starts
    regCount: int ## The maximum number of registers required to run the code

  AtomKind* = enum
    akInt
    akFloat
    akPtr
    akSet

    akString
    akSeq
    akRef
    akCallable # TODO: rename to akProcedural or akFuncHandle
    akClosure # XXX: closures could also be represented as an akObject
              #      with two fields. Few problems: You'd need the concept of a
              #      dynamically typed ref (easy); the guest must be prevented
              #      from modifying the closure's fields (possible); extra
              #      detection logic in `opcIndCall` is required

    akDiscriminator

    # TODO: rename to `akNimNode`
    akPNode

    # Pseudo atom kinds
    akObject # currently also includes tuple types
    akArray


  # XXX: `PVmType` will be step-by-step replaced with `VmTypeId`
  PVmType* = ref VmType
  VmTypeId* = #[distinct]# uint32
    ## The unique ID of a `VmType`. Implementation-wise, it's an index into
    ## `TypeInfoCache.types`

  FieldPosition* = distinct uint32
  FieldIndex* = distinct uint32

  BranchListEntryKind* = enum
    blekStart
    blekBranch
    blekEnd

  BranchListEntry* = object
    ## An entry in a walk-list, describing an object variant in a flat manner.
    ## A record-case maps to the following entries:
    ## * a 'start' entry (represents the discriminator)
    ## * one or more 'branch' entries (one for each `of`-branch)
    ## * an 'end' entry
    ##
    ## If a branch contains a sub record-case, the record-case's entries
    ## follow the entry of the surrounding branch (i.e. the layout is stored
    ## depth-first). The first entry in the list is always a 'branch' (called
    ## the master branch) enclosing all fields in the object. The last entry
    ## is always a dedicated 'end' entry.
    ##
    ## The idea behind this way of representing the layout of a variant object,
    ## is to allow for fast linear traversal of active fields without the need
    ## for recursion or auxiliary stack-like data structures
    case kind*: BranchListEntryKind
    of blekStart:
      field*: FieldIndex ## The index of the corresponding discriminator
      numItems*: uint32 ## The number of items following this entry that are
                        ## part of this record-case. Used for the fast skipping
                        ## of inactive branches
      defaultBranch*: uint16 ## The index of the default branch
      numBranches*: uint16 ## The number of branches this record-case has.
                           ## Does not include branches of sub record-cases
    of blekBranch, blekEnd:
      fieldRange*: Slice[FieldIndex] ##
        ## For 'branch' entries, the range of field indices the branch spans.
        ## For `end` entries, the range of fields past the corresponding
        ## record-case that are still part of the surrounding branch.
        ##
        ## In both cases, the slice may be empty, with `fieldRange.a` always
        ## being >= the previous entry's `fieldRange.a`.
        ## For the dedicated 'end' entry, `fieldRange.b` is always 0

  VmType* = object
    ## A `VmType` is a concrete type, holding the information necessary to
    ## interpret the raw bytes of a location. `PType`s are mapped to `VmType`s
    ## in `vmtypegen`.

    # XXX: the layout of `VmType` is not final
    case kind*: AtomKind
    of akInt, akFloat, akPNode: discard
    of akPtr, akRef:
      targetType*: PVmType
    of akSet:
      setLength*: int ## The number of elements in the set
    of akSeq, akString:
      # TODO: remove stride information and merge this branch with
      #       `akPtr`/`akRef`
      seqElemStride*: int
      seqElemType*: PVmType
    of akCallable, akClosure:
      routineSig*: RoutineSigId
    of akDiscriminator:
      # A discriminator consists of two things: the value as seen by the guest
      # and the index of the branch
      numBits*: int ## The amount of bits the discriminator value occupies
    of akObject:
      # TODO: rename to something like `firstFieldPos`
      # TODO: relFieldStart will likely be replaced by an out-of-band solution
      relFieldStart*: uint32 ## Encodes both the position (`PSym.position`) of
                             ## the first field as well as whether the object
                             ## has a base. If > 0, the object has a base.
                             ## Subtracting 1 one from a `> 0` value yields the
                             ## position

      # XXX: `objFields` will likely be made available for all kinds. If the
      #      type is atomic, `fields` would store a single item (the type
      #      itself)
      objFields*: seq[tuple[offset: int, typ: PVmType]]

      branches*: seq[BranchListEntry] ## Empty if the object is no variant

    of akArray:
      elementCount*: int
      # TODO: remove stride information
      elementStride*: int
      elementType*: PVmType

    sizeInBytes*: uint
    alignment*: uint8 ## 1 shl `alignment` == real alignment value

  VmTypeInfo* = object
    ## Stores run-time-type-information (RTTI) for types. Similiar to the
    ## `PNimType` used by the other backends, with the difference that
    ## `VmTypeInfo` is not exposed to user-code.
    ## Only used internally for implementing `repr` and `opcConv`

    internal*: PVmType ## the `VmType` the type maps to
    nimType*: PType

  CallableKind* = enum
    ckDefault ## A normal function
    ckCallback ## A VmCallback

  VmFunctionPtr* = distinct int

  FuncTableEntry* = object
    ## A function table entry. Stores all information necessary to call the
    ## corresponding procedure

    sym*: PSym
    retValDesc*: PVmType ## the return value type (may be empty)
    envParamType*: PVmType ## the type of the hidden environment parameter
    sig*: RoutineSigId

    case kind*: CallableKind
    of ckDefault:
      regCount*: uint16
      start*: int ## the code position where the function starts
    of ckCallback:
      cbOffset*: int ## the index into the callback list

  VmClosure* = object
    fnc*: VmFunctionPtr
    env*: HeapSlotHandle

  # XXX: once first class openArray support is available during bootstrapping,
  #      `MemRegionPtr` could be turned into `openArray[byte]`
  MemRegionPtr* = object
    ## A pointer to a region of memory
    p: ptr UncheckedArray[byte]
    l: int
  VmMemoryRegion* = openArray[byte]

  LocHandle* = object
    ## A handle to a ``location``
    h*: MemRegionPtr
    typ* {.cursor.}: PVmType # ownership of `typ` is not needed

  VmSeq* = object
    length*: int
    data*: MemRegionPtr

  VmString* = distinct VmSeq

  Atom* {.union.} = object
    ## Convenience type to make working with atomic locations easier. Since
    ## the union stores gc'ed memory (PNode), it must never be used on either
    ## side of an assignment and also never used as a variable's type
    ptrVal*: pointer ## akPtr
    strVal*: VmString ## akString
    seqVal*: VmSeq ## akSeq
    refVal*: HeapSlotHandle ## akRef
    callableVal*: VmFunctionPtr ## akCallable
    closureVal*: VmClosure ## akClosure

    nodeVal*: PNode ## akPNode


  TFullReg* = object
    case kind*: TRegisterKind
    of rkNone: discard
    of rkInt: intVal*: BiggestInt
    of rkFloat: floatVal*: BiggestFloat
    of rkAddress:
      addrVal*: MemRegionPtr
      addrTyp*: PVmType
    of rkLocation, rkHandle:
      handle*: LocHandle
    of rkNimNode:
      nimNode*: PNode

    of rkRegAddr:
      regAddr*: ptr TFullReg


  # XXX should probably be a distinct int
  HeapSlotHandle* = int

  HeapSlot* = object
    handle*: LocHandle
    refCount*: int

  VmHeap* = object
    ## `VmHeap` manages all ref-counted locations. These are used for `new`'ed
    ## values as well as globals.
    ##
    ## Once a slot's ref-count reaches zero, the
    ## slot is not cleaned up and freed immediately, but is added to the
    ## `pending` list first. Pending slots are currently only freed at the end
    ## of a VM invocation, with cyclic references leading to memory leaks
    ## (no cycle detection is performed)
    slots*: seq[HeapSlot]
    pending*: seq[int] ## the indices of the slots that are pending clean-up

  ConstantKind* = enum
    cnstInt
    cnstFloat
    cnstString
    cnstNode ## AST, type literals

    # slice-lists are used for implementing `opcBranch` (branch for case stmt)
    cnstSliceListInt
    cnstSliceListFloat
    cnstSliceListStr

  ConstantId* = int ## The ID of a `VmConstant`. Currently just an index into
                    ## `PCtx.constants`

  VmConstant* = object
    ## `VmConstant`s are used for passing constant data from `vmgen` to the
    ## execution engine. This includes constants for both internal use as well
    ## as user-defined literal values (e.g. string literals).

    case kind*: ConstantKind
    of cnstInt:
      intVal*: BiggestInt
    of cnstFloat:
      floatVal*: BiggestFloat
    of cnstString:
      strVal*: string
    of cnstNode:
      node*: PNode

    of cnstSliceListInt:
      # XXX: always using `BiggestInt` is inredibly wasteful for when the
      #      values are small (e.g. `char`)
      intSlices*: seq[Slice[BiggestInt]]
    of cnstSliceListFloat:
      floatSlices*: seq[Slice[BiggestFloat]]
    of cnstSliceListStr:
      strSlices*: seq[Slice[ConstantId]] ## Stores the ids of string constants
                                         ## as a storage optimization

  RegInfo* = object
    refCount*: uint16
    locReg*: uint16 ## if the register stores a handle, `locReg` is the
                    ## register storing the backing location
    kind*: TSlotKind

  PProc* = ref object
    blocks*: seq[TBlock]    # blocks; temp data structure
    sym*: PSym
    regInfo*: seq[RegInfo]

    # XXX: the value's type should be `TRegister`, but we need a sentinel
    #      value (-1) for `getOrDefault`, so it has to be `int`
    locals*: Table[int, int] ## symbol-id -> register index. Used for looking
                             ## up the corresponding register slot of each
                             ## local (including parameters and `result`)

  VmArgs* = object
    ra*, rb*, rc*: Natural
    slots*: ptr UncheckedArray[TFullReg]
    currentException*: HeapSlotHandle
    currentLineInfo*: TLineInfo

    # XXX: These are only here as a temporary measure until callback handling
    #      gets an overhaul
    typeCache*: ptr TypeInfoCache
    mem*: ptr VmMemoryManager
    heap*: ptr VmHeap

  IdentPattern* = distinct string ## A matcher pattern for a fully qualified
                                  ## symbol identifier
  VmCallback* = proc (args: VmArgs) {.closure.}

  VmAllocator* = object
    # XXX: the current implementation of the allocator is overly simple and
    #      only temporary
    # XXX: support for memory regions going past the 0..2^63 (or 0..2^31)
    #      region is very fuzzy at the moment due to the mixing of int
    #      and uint
    regions*: seq[tuple[typ: PVmType, count: int, start: int, len: int]]
    byteType*: PVmType ## The VM type for `byte`

  VmMemoryManager* = object
    allocator*: VmAllocator
    heap*: VmHeap

  # XXX: should be a `distinct uint32`. More than 2^32 different routine
  #      signatures is highly unlikely
  RoutineSigId* = distinct int ## Routine signature ID. Each different
    ## routine (proc, func, etc.) signature gets mapped to a unique ID.

  # We use a distinct here since we need a different `==` and `hash`
  # operator than the one used for `PType`
  RoutineSig* = distinct PType

  TypeTableEntry* = tuple[hcode: int, typ: VmTypeId]
  TypeTable* = object
    ## A partial hash-table overlay for `TypeInfoCache.types`. Not all types
    ## present in the latter need to be present in the table
    data*: seq[TypeTableEntry]
    counter*: int

  TypeInfoCache* = object
    ## An append-only cache for everything type related
    lut*: Table[ItemId, PVmType] ## `PType`-id -> `PVmType` mappings
    genericInsts*: Table[ItemId, seq[(PType, PVmType)]] ##
      ## 'generic type' -> 'known instantiations' mappings. Needed to make
      ## sure that all same instantations map to the same VmType

    structs*: TypeTable ## All structural types created by ``vmtypegen``

    signatures*: Table[RoutineSig, RoutineSigId]
    nextSigId*: RoutineSigId ## The ID to use for a new `signatures` entry

    types*: seq[PVmType] ## all generated types (including those created
                         ## during VM setup)

    staticInfo*: array[AtomKind, tuple[size, align: uint8]]
      ## size and alignment information for atoms where this information is
      ## the same for every instance

    # XXX: if `tyBool..tyFloat128` would only include all numeric types (plus
    #      bool and char), we could use the `numericTypes` below instead
    #numericTypes*: array[tyBool..tyFloat128, PVmType] ## A lookup table for all numeric types (ints/float)
    boolType*: PVmType
    charType*: PVmType
    stringType*: PVmType
    pointerType*: PVmType
    nodeType*: PVmType
    emptyType*: PVmType

    # TODO: merge these arrays into one. Handle `tyFloat128` somehow
    intTypes*: array[tyInt..tyInt64, PVmType]
    uintTypes*: array[tyUInt..tyUInt64, PVmType]
    floatTypes*: array[tyFloat..tyFloat64, PVmType]

  FunctionIndex* = distinct int

  # XXX: TCtx's contents should be separated into five parts (separate object
  #      types):
  #      - 'execution state': stack frames, program counter, etc.; everything
  #        that makes up a single VM invocation. Mutated during execution
  #      - 'shared execution state': allocator, managed slots, etc.; state that
  #        is shared across VM invocations. Mutated during execution
  #      - 'execution environment': types, globals, constants, functions, etc.;
  #        populated by code-gen (vmgen) or the compilerapi and possibly
  #        reused across compiler invocations (relevant for IC). Not mutated
  #        by the execution engine
  #      - code and debug information
  #      - 'vmgen' state: auxiliary data used during code generation, reused
  #        across vmgen invocations for efficiency

  CodeGenFlag* = enum
    cgfAllowMeta ## If not present, type or other meta expressions are
                 ## disallowed in imperative contexts and code-gen for meta
                 ## function arguments (e.g. `typedesc`) is suppressed

  LinkIndex* = uint32 ## Depending on the context: `FunctionIndex`; index
    ## into `TCtx.globals`; index into `TCtx.complexConsts`

  # XXX: design of `CodeGenInOut` is not yet final
  CodeGenInOut* = object
    ## Mutable global state for the code-generator (`vmgen`) that is not
    ## directly related to the code being generated.
    ##
    ## The `newX` seqs accumulate the symbols added to `symToIndexTbl` during
    ## code-gen, with a seq for each link-item kind. They're never reset by
    ## `vmgen`.
    ##
    ## The `nextX` values hold the `LinkIndex` to use for new entries
    ## added to `symToIndexTbl`. They're incremented after a respective
    ## new entry is added.
    newProcs*: seq[PSym]
    newGlobals*: seq[PSym]
    newConsts*: seq[PSym]

    nextProc*: LinkIndex
    nextGlobal*: LinkIndex
    nextConst*: LinkIndex

    flags*: set[CodeGenFlag] ## input
  
  VmGenDiagKind* = enum
    # has no extra data
    vmGenDiagBadExpandToAstArgRequired
    vmGenDiagBadExpandToAstCallExprRequired
    vmGenDiagTooManyRegistersRequired
    vmGenDiagCannotFindBreakTarget
    # has ast data
    vmGenDiagNotUnused
    vmGenDiagNotAFieldSymbol
    vmGenDiagCannotGenerateCode
    vmGenDiagCannotEvaluateAtComptime
    vmGenDiagInvalidObjectConstructor
    # has magic data
    vmGenDiagMissingImportcCompleteStruct
    vmGenDiagCodeGenUnhandledMagic
    # has sym data
    vmGenDiagCodeGenGenericInNonMacro
    vmGenDiagCodeGenUnexpectedSym
    vmGenDiagCannotImportc
    vmGenDiagTooLargeOffset
    vmGenDiagNoClosureIterators
    vmGenDiagCannotCallMethod
    # has type mismatch data
    vmGenDiagCannotCast
  
  VmGenDiagKindAstRelated* =
    range[vmGenDiagNotUnused..vmGenDiagInvalidObjectConstructor]
    # TODO: this is a somewhat silly type, the range allows creating type safe
    #       diag construction functions -- see: `vmgen.fail`
  
  VmGenDiagKindSymRelated* =
    range[vmGenDiagCodeGenGenericInNonMacro..vmGenDiagCannotCallMethod]
    # TODO: this is a somewhat silly type, the range allows creating type safe
    #       diag construction functions -- see: `vmgen.fail`
  
  VmGenDiagKindMagicRelated* =
    range[vmGenDiagMissingImportcCompleteStruct..vmGenDiagCodeGenUnhandledMagic]
    # TODO: this is a somewhat silly type, the range allows creating type safe
    #       diag construction functions -- see: `vmgen.fail`

  VmTypeMismatch* = object
    actualType*, formalType*: PType

  VmGenDiag* = object
    ## `Diag`nostic data from VM Gen, mostly errors
    # TODO: rework fields and enum order so they form sensible categories,
    #       introducing overlapping field types across variants is fine.
    location*: TLineInfo        ## diagnostic location
    instLoc*: InstantiationInfo ## instantiation in VM Gen's source
    case kind*: VmGenDiagKind
      of vmGenDiagCodeGenGenericInNonMacro,
          vmGenDiagCodeGenUnexpectedSym,
          vmGenDiagCannotImportc,
          vmGenDiagTooLargeOffset,
          vmGenDiagNoClosureIterators,
          vmGenDiagCannotCallMethod:
        sym*: PSym
      of vmGenDiagCannotCast:
        typeMismatch*: VmTypeMismatch
      of vmGenDiagMissingImportcCompleteStruct,
          vmGenDiagCodeGenUnhandledMagic:
        magic*: TMagic
      of vmGenDiagNotUnused,
          vmGenDiagNotAFieldSymbol,
          vmGenDiagCannotGenerateCode,
          vmGenDiagCannotEvaluateAtComptime,
          vmGenDiagInvalidObjectConstructor:
        ast*: PNode
      of vmGenDiagBadExpandToAstArgRequired,
          vmGenDiagBadExpandToAstCallExprRequired,
          vmGenDiagTooManyRegistersRequired,
          vmGenDiagCannotFindBreakTarget:
        discard

  VmEventKind* = enum
    vmEvtOpcParseExpectedExpression
    vmEvtUserError
    vmEvtUnhandledException
    vmEvtCannotCast
    vmEvtCallingNonRoutine
    vmEvtCannotModifyTypechecked
    vmEvtNilAccess
    vmEvtAccessOutOfBounds
    vmEvtAccessTypeMismatch
    vmEvtAccessNoLocation
    vmEvtErrInternal
    vmEvtIndexError
    vmEvtOutOfRange
    vmEvtOverOrUnderflow
    vmEvtDivisionByConstZero
    vmEvtArgNodeNotASymbol
    vmEvtNodeNotASymbol
    vmEvtNodeNotAProcSymbol
    vmEvtIllegalConv
    vmEvtMissingCacheKey
    vmEvtCacheKeyAlreadyExists
    vmEvtFieldNotFound
    vmEvtNotAField
    vmEvtFieldUnavailable
    vmEvtCannotSetChild
    vmEvtCannotAddChild
    vmEvtCannotGetChild
    vmEvtNoType
    vmEvtTooManyIterations

  VmEventKindAccessError* = range[vmEvtAccessOutOfBounds .. vmEvtAccessNoLocation]

  VmEvent* = object
    ## Event data from a VM instance, mostly errors
    instLoc*: InstantiationInfo    ## instantiation in VM's source
    case kind*: VmEventKind
      of vmEvtUserError:
        errLoc*: TLineInfo
        errMsg*: string
      of vmEvtArgNodeNotASymbol:
        callName*: string
        argAst*: PNode
        argPos*: int
      of vmEvtCannotCast:
        typeMismatch*: VmTypeMismatch
      of vmEvtIndexError:
        indexSpec*: tuple[usedIdx, minIdx, maxIdx: Int128]
      of vmEvtErrInternal, vmEvtNilAccess, vmEvtIllegalConv,
          vmEvtFieldUnavailable, vmEvtFieldNotFound,
          vmEvtCacheKeyAlreadyExists, vmEvtMissingCacheKey:
        msg*: string
      of vmEvtCannotSetChild, vmEvtCannotAddChild, vmEvtCannotGetChild,
          vmEvtUnhandledException, vmEvtNoType, vmEvtNodeNotASymbol:
        ast*: PNode
      of vmEvtNotAField:
        sym*: PSym
      else:
        discard

  VmExecTraceKind* = enum
    vmTraceMin  ## minimal data execution trace, lighter/faster
    vmTraceFull ## full data execution trace, more info/heavier likely slower

  VmExecTrace* = object
    pc*: PrgCtr
    case kind*: VmExecTraceKind:
      of vmTraceMin:
        discard
      of vmTraceFull:
        ra*, rb*, rc*: TRegisterKind

  TraceHandler* = proc(c: TCtx, t: VmExecTrace): void

  VmStackTrace* = object
    # xxx: if possible remove `currentExceptionA` and `currentExceptionB` as
    #      they're not queried.
    currentExceptionA*, currentExceptionB*: PNode
    stacktrace*: seq[tuple[sym: PSym, location: TLineInfo]]
    skipped*: int

  PCtx* = ref TCtx
  TCtx* = object of TPassContext
    # XXX: TCtx stores three different things:
    #  - VM execution state
    #  - VM environment (code, constants, type data, etc.)
    #  - vmgen context/state
    # These should be encapsulated into standalone types

    code*: seq[TInstr]
    debug*: seq[TLineInfo]  # line info for every instruction; kept separate
                            # to not slow down interpretation
    sframes*: seq[TStackFrame] ## The stack of the currently running code # XXX: rename to `stack`?
    globals*: seq[HeapSlotHandle] ## Stores each global's corresponding heap slot
    constants*: seq[VmConstant] ## constant data
    complexConsts*: seq[LocHandle] ## complex constants (i.e. everything that
                                   ## is not a int/float/string literal)

    typeInfoCache*: TypeInfoCache ## manages the types used by the VM

    rtti*: seq[VmTypeInfo] ## run-time-type-information as needed by
                           ## conversion and `repr`
    functions*: seq[FuncTableEntry] ## the function table. Contains an entry
      ## for each procedure known to the VM. Indexed by `FunctionIndex`
    memory*: VmMemoryManager

    # linker state:
    symToIndexTbl*: Table[int, LinkIndex] ## keeps track of all known
      ## dependencies. Expanded during code-generation and used for looking
      ## up the link-index (e.g. `FunctionIndex`) of a symbol

    codegenInOut*: CodeGenInOut ## Input and outputs to vmgen
    # TODO: `codegenInOut` shouldn't be part of `TCtx` but rather a `var`
    #       parameter for the various codegen functions

    currentExceptionA*, currentExceptionB*: HeapSlotHandle
    exceptionInstr*: int # index of instruction that raised the exception
    prc*: PProc
    module*: PSym
    callsite*: PNode
    mode*: TEvalMode
    features*: TSandboxFlags
    traceActive*: bool
    loopIterations*: int
    comesFromHeuristic*: TLineInfo # Heuristic for better macro stack traces
    callbacks*: seq[VmCallback]
    callbackKeys*: seq[IdentPattern] ## The matcher patterns corresponding to
      ## each entry in `callbacks`. Written during VM environment setup or
      ## inbetween invocations. Read during code-generation.
    cache*: IdentCache
    config*: ConfigRef
    graph*: ModuleGraph
    oldErrorCount*: int
    profiler*: Profiler
    templInstCounter*: ref int # gives every template instantiation a unique ID, needed here for getAst
    vmstateDiff*: seq[(PSym, PNode)] # we remember the "diff" to global state here (feature for IC)
    vmTraceHandler*: TraceHandler ## handle trace output from an executing vm

  StackFrameIndex* = int

  TStackFrame* = object
    prc*: PSym                 # current prc; proc that is evaluated
    slots*: seq[TFullReg]      # parameters passed to the proc + locals;
                              # parameters come first
    next*: StackFrameIndex         # for stacking
    comesFrom*: int
    safePoints*: seq[int]      # used for exception handling
                              # XXX 'break' should perform cleanup actions
                              # What does the C backend do for it?
  Profiler* = object
    tEnter*: float
    sframe*: StackFrameIndex   ## The current stack frame

  TPosition* = distinct int

  PEvalContext* = PCtx

func `<`*(a, b: FieldIndex): bool {.borrow.}
func `<=`*(a, b: FieldIndex): bool {.borrow.}
func `==`*(a, b: FieldIndex): bool {.borrow.}
template `-`*(a: FieldIndex, b: int): FieldIndex =
  let n = int(a) - b
  rangeCheck(n >= 0)
  FieldIndex(n)

func `==`*(a, b: FunctionIndex): bool {.borrow.}

template fieldAt*(x: VmType, i: FieldIndex): untyped =
  x.objFields[i.int]

template fieldAt*(x: PVmType, i: FieldIndex): untyped =
  x.objFields[i.int]

template fpos*(x: int): FieldPosition =
  FieldPosition(x)

template heap*(c: TCtx): untyped =
  c.memory.heap

template allocator*(c: TCtx): untyped =
  c.memory.allocator

template types*(c: TCtx): untyped =
  ## Transition helper
  c.typeInfoCache.types

proc init*(cache: var TypeInfoCache) =
  template mkDesc(ak, s, a): untyped =
    PVmType(kind: ak, sizeInBytes: uint(s), alignment: a)

  template addType(n, ak, s, a) =
    cache.n = mkDesc(ak, s, a)
    cache.types.add(cache.n)

  template addTypeA(k, n, ak, s, a) =
    cache.n[k] = mkDesc(ak, s, a)
    cache.types.add(cache.n[k])

  func vmAlignof(t: typedesc): uint8 {.compileTime.} =
    let align = uint64(alignof(t))
    # Simple log2 for integers
    for i in 0..63:
      if align == (1'u64 shl i):
        return uint8(i)
    doAssert false # alignof(t) is not a power-of-two (this shouldn't happen)

  template setInfo(k, t) =
    cache.staticInfo[k] = (sizeof(t).uint8, static vmAlignof(t))

  setInfo(akSeq, VmSeq)
  setInfo(akString, VmString)
  setInfo(akPtr, ptr Atom)
  setInfo(akRef, HeapSlotHandle)
  setInfo(akCallable, VmFunctionPtr)
  setInfo(akClosure, VmClosure)
  setInfo(akPNode, PNode)

  # Add a `nil` at index '0' so that type-id '0' means none/nil/invalid
  cache.types.add(nil)

  # Too many things would break if emptyType had a size of 0
  cache.emptyType = PVmType(kind: akObject, sizeInBytes: 1) # an empty tuple
  cache.types.add(cache.emptyType)

  addType(boolType, akInt, 1, 0)
  addType(charType, akInt, 1, 0)
  cache.stringType =
    PVmType(kind: akString,
            sizeInBytes: sizeof(VmString).uint,
            alignment: static vmAlignof(VmString),
            seqElemType: cache.charType,
            seqElemStride: 1)
  cache.types.add(cache.stringType)
  addType(pointerType, akPtr, sizeof(ptr Atom), static vmAlignof(ptr Atom)) # untyped pointer
  addType(nodeType, akPNode, sizeof(PNode), static vmAlignof(PNode))

  # TODO: sizes for `int` and `float` should be configurable

  # int types
  addTypeA(tyInt, intTypes, akInt, 8, 3)
  addTypeA(tyInt8, intTypes, akInt, 1, 0)
  addTypeA(tyInt16, intTypes, akInt, 2, 1)
  addTypeA(tyInt32, intTypes, akInt, 4, 2)
  addTypeA(tyInt64, intTypes, akInt, 8, 3)

  # uint types
  addTypeA(tyUInt, uintTypes, akInt, 8, 3)
  addTypeA(tyUInt8, uintTypes, akInt, 1, 0)
  addTypeA(tyUInt16, uintTypes, akInt, 2, 1)
  addTypeA(tyUInt32, uintTypes, akInt, 4, 2)
  addTypeA(tyUInt64, uintTypes, akInt, 8, 3)

  # float types
  addTypeA(tyFloat, floatTypes, akFloat, 8, 3)
  addTypeA(tyFloat32, floatTypes, akFloat, 4, 2)
  addTypeA(tyFloat64, floatTypes, akFloat, 8, 3)

# `Atom` must never ever be used in assignments
proc `=`(a: var Atom, b: Atom) {.error.}

func toFuncPtr*(x: FunctionIndex): VmFunctionPtr =
  VmFunctionPtr(int(x) + 1)

func toFuncIndex*(x: VmFunctionPtr): FunctionIndex =
  assert int(x) > 0 # the caller needs to make sure that x != 0
  FunctionIndex(int(x) - 1)

template isNil*(x: VmFunctionPtr): bool = int(x) == 0

func `==`*(a, b: RoutineSigId): bool {.borrow.}

proc defaultTracer(c: TCtx, t: VmExecTrace) =
  echo "default echo tracer" & $t

proc newCtx*(module: PSym; cache: IdentCache; g: ModuleGraph;
             idgen: IdGenerator, tracer: TraceHandler = defaultTracer): PCtx =
  result = PCtx(
    code: @[],
    debug: @[],
    globals: @[],
    constants: @[],
    prc: PProc(blocks: @[]),
    module: module,
    loopIterations: g.config.maxLoopIterationsVM,
    comesFromHeuristic: unknownLineInfo,
    callbacks: @[],
    cache: cache,
    config: g.config,
    graph: g,
    idgen: idgen,
    vmtraceHandler: tracer
  )

  # The first slot (index 0) is reserved so that index == 0 means nil access
  result.memory.heap.slots.add HeapSlot()

  result.typeInfoCache.init()
  result.memory.allocator.byteType = result.typeInfoCache.charType

func refresh*(c: var TCtx, module: PSym; idgen: IdGenerator) =
  addInNimDebugUtils(c.config, "refresh")
  c.module = module
  c.prc = PProc(blocks: @[])
  c.loopIterations = c.config.maxLoopIterationsVM
  c.idgen = idgen

proc registerCallback*(c: var TCtx; name: string; callback: VmCallback): int {.discardable.} =
  result = c.callbacks.len
  c.callbacks.add(callback)
  # XXX: for backwards compatibility, `name` is still a `string`
  c.callbackKeys.add(IdentPattern(name))

template registerCallback*(c: PCtx; name: string; callback: VmCallback): int {.deprecated.} =
  ## A transition helper. Use the `registerCallback` proc that takes
  ## `var TCtx` instead
  registerCallback(c[], name, callback)


const pseudoAtomKinds* = {akObject, akArray}
const realAtomKinds* = {low(AtomKind)..high(AtomKind)} - pseudoAtomKinds

const
  slotSomeTemp* = slotTempUnknown

# flag is used to signal opcSeqLen if node is NimNode.
const nimNodeFlag* = 16

template opcode*(x: TInstr): TOpcode = TOpcode(x.TInstrType shr regOShift and regOMask)
template regA*(x: TInstr): TRegister = TRegister(x.TInstrType shr regAShift and regAMask)
template regB*(x: TInstr): TRegister = TRegister(x.TInstrType shr regBShift and regBMask)
template regC*(x: TInstr): TRegister = TRegister(x.TInstrType shr regCShift and regCMask)
template regBx*(x: TInstr): int = (x.TInstrType shr regBxShift and regBxMask).int

template jmpDiff*(x: TInstr): int = regBx(x) - wordExcess

func makeMemPtr*(p: pointer, sizeInBytes: uint): MemRegionPtr {.inline.} =
  MemRegionPtr(p: cast[ptr UncheckedArray[byte]](p), l: sizeInBytes.int)

func makeLocHandle*(p: MemRegionPtr, typ: PVmType): LocHandle {.inline.} =
  assert typ != nil
  assert typ.sizeInBytes <= uint(p.l)
  LocHandle(h: MemRegionPtr(p: p.p, l: int(typ.sizeInBytes)), typ: typ)

func makeLocHandle*(h: pointer, typ: PVmType): LocHandle {.inline.} =
  LocHandle(h: makeMemPtr(h, typ.sizeInBytes), typ: typ)

template isNil*(h: MemRegionPtr): bool = h.p == nil
template isNil*(h: HeapSlotHandle): bool = int(h) == 0

const noneType*: PVmType = nil

template nilMemPtr*: untyped = MemRegionPtr(p: nil, l: 0)
template invalidLocHandle*: untyped = (h: nilMemPtr, typ: nil)

template isValid*(t: PVmType): bool = t != nil

template rawPointer*(h: MemRegionPtr): untyped =
  h.p

template len*(h: MemRegionPtr): untyped = h.l

template byteView*(h: MemRegionPtr): untyped =
  toOpenArray(h.p, 0, h.l-1)

template subView*[T](x: openArray[T], off, len): untyped =
  x.toOpenArray(off, int(uint(off) + uint(len) - 1))


func newVmString*(data: MemRegionPtr, len: Natural): VmString {.inline.} =
  VmString(VmSeq(data: data, length: len))

func len*(s: VmString): int {.inline.} = VmSeq(s).length

func data*(s: VmString): MemRegionPtr {.inline.} = VmSeq(s).data


template isValid*(h: MemRegionPtr, len: uint): bool = h.p != nil and int(len) <=% h.l
template isValid*(handle: LocHandle): bool =
  let x = handle
  isValid(x.h, x.typ.sizeInBytes)

# TODO: move `overlap` and it's tests somewhere else
func overlap*(a, b: int, aLen, bLen: Natural): bool =
  ## Tests if the ranges `[a, a+aLen)` and `[b, b+bLen)` have elements
  ## in common
  let ar = a + aLen - 1
  let br = b + bLen - 1
  let x = ar - b
  let y = br - a
  if x >= 0 and y >= 0:
    return true

func overlap*(a, b: pointer, aLen, bLen: Natural): bool =
  ## Test if the memory regions overlap
  overlap(cast[int](a), cast[int](b), aLen, bLen)

static:
  func test(a, al, b, bl: int, invert = false) =
    if not invert:
      assert overlap(a, b, al, bl)
      assert overlap(b, a, bl, al)
    else:
      assert not overlap(a, b, al, bl)
      assert not overlap(b, a, bl, al)

  test(0, 10, 0, 10) # same range
  test(0, 5, 4, 1) # sharing a single element 1
  test(0, 5, 4, 6) # sharing a single element 2
  test(0, 5, 5, 5, true) # disjoint
  test(0, 10, 2, 6) # a contains b


# TODO: move `safeCopyMem` and `safeZeroMem` somewhere else

func safeCopyMem*(dest: var openArray[byte|char], src: openArray[byte|char], numBytes: Natural) {.inline.} =
  # TODO: Turn the asserts into range checks instead
  assert numBytes <= dest.len
  assert numBytes <= src.len
  if numBytes > 0:
    # Calling `safeCopyMem` with empty src/dest would erroneously raise without the > 0 check
    assert not overlap(addr dest[0], unsafeAddr src[0], dest.len, src.len)
    copyMem(addr dest[0], unsafeAddr src[0], numBytes)

func safeCopyMemSrc*(dest: var openArray[byte], src: openArray[byte]) {.inline.} =
  # TODO: turn assert into a range check
  assert src.len <= dest.len
  if src.len > 0:
    assert not overlap(addr dest[0], unsafeAddr src[0], dest.len, src.len)
    copyMem(addr dest[0], unsafeAddr src[0], src.len)

func safeZeroMem*(dest: var openArray[byte], numBytes: Natural) =
  assert numBytes <= dest.len
  if numBytes > 0:
    # Calling `safeZeroMem` with empty `dest` would erroneously raise without this check
    zeroMem(addr dest[0], numBytes)
