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

import
  std/[
    intsets,
    tables
  ],
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
    idioms
  ]

import std/options as std_options

from compiler/vm/vmlinker import LinkerData

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
  TInstr* = distinct TInstrType

  NumericConvKind* = enum
    ## Identifies the numeric conversion kind.
    ## I = signed; U = unsigned; F = float;
    nckFToI, nckFToU
    nckIToF, nckUToF
    nckFToF ## float-to-float
    nckToB  ## float or int to bool

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
    of akCallable:
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
    isClosure*: bool     ## whether the closure calling convention is used
    sig*: RoutineSigId

    case kind*: CallableKind
    of ckDefault:
      regCount*: uint16
      start*: int ## the code position where the function starts
    of ckCallback:
      cbOffset*: int ## the index into the callback list

  VmMemPointer* = distinct ptr UncheckedArray[byte]
    ## A pointer into a memory region managed by the VM (i.e. guest memory)
  VmMemoryRegion* = openArray[byte]

  CellId* = int
  CellPtr* = distinct ptr UncheckedArray[byte]
    ## A pointer that either is nil or points to the start of a VM memory cell

  LocHandle* = object
    ## A handle to a VM memory location. Since looking up the cell a location
    ## is part of is a very common operation (due to access checks), the cell's
    ## ID is also stored here.
    cell*: CellId
      ## the ID of the cell that owns the location
    p*: VmMemPointer
      ## the memory address of the reference location, in host address space
    typ* {.cursor.}: PVmType
      ## the type that the handles views the location as

  VmSlice* = object
    ## A loaded slice. Stores all information that the VM needs to efficiently
    ## access the referenced guest memory. A `VmSlice` is not meant to be
    ## stored in guest memory, but rather for internal use by the VM.
    cell*:  CellId           ## the cell of which the locations are part of
    start*: VmMemPointer     ## start of the slice (or ``nil``)
    len*:   int              ## the number of items
    typ* {.cursor.}: PVmType ## the item type

  VmSeq* = object
    # XXX: consider storing the cell's ID instead of the pointer. Doing so
    #      would significantly speed up seq/string access, as no costly
    #      pointer-to-id mapping operation has to take place then
    length*: int
    data*: CellPtr

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

    nodeVal*: PNode ## akPNode


  TFullReg* = object
    case kind*: TRegisterKind
    of rkNone: discard
    of rkInt: intVal*: BiggestInt
    of rkFloat: floatVal*: BiggestFloat
    of rkAddress:
      addrVal*: pointer
      addrTyp*: PVmType
    of rkLocation, rkHandle:
      handle*: LocHandle
    of rkNimNode:
      nimNode*: PNode

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
                    ## `TCtx.constants`

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

  VmArgs* = object
    ra*, rb*, rc*: Natural
    slots*: ptr UncheckedArray[TFullReg]
    # TODO: rework either the callback or exception handling (or both) so that
    #       no pointer is required here
    currentExceptionPtr*: ptr HeapSlotHandle
    currentLineInfo*: TLineInfo

    # XXX: These are only here as a temporary measure until callback handling
    #      gets an overhaul
    typeCache*: ptr TypeInfoCache
    mem*: ptr VmMemoryManager
    heap*: ptr VmHeap

    # compiler interfacing:
    graph*: ModuleGraph
    config*: ConfigRef
    currentModule*: PSym ## module currently being compiled
    cache*: IdentCache
    idgen*: IdGenerator

  VmCallback* = proc (args: VmArgs) {.closure.}

  VmCell* = object
    ## Stores information about a memory cell allocated by the VM's allocator
    count*: int
      ## stores the number of locations for cells that are in-use; for free
      ## cells this is the next pointer
      # TODO: once ``VmCell`` is part of the ``vmmemory`` module, don't export
      #       `count` nor access it directly -- use accessors instead
    sizeInBytes*: int
      ## the total number of *guest-accesible* bytes the cell occupies

    p*: ptr UncheckedArray[byte]
    typ* {.cursor.}: PVmType

  # ---------- Future work --------------
  #
  # 1) use a separate cell type and list for stack cells. They don't require
  #    the `count` field and have a different usage pattern, i.e. push/pop
  #    (not yet but eventually).
  #
  # 2) merge ``VmHeap`` into ``VmAllocator``. Neither a separate cell type
  #    (i.e. ``HeapSlot``) nor the separate list is required for refcounted
  #    cell anymore. Merging them will simplify the allocator and speed up
  #    ``ref``s.
  #    The ``count`` field of ``VmCell`` should be used for storing the
  #    refcounter for refcounted cells.
  #
  # 3) don't use a linear search for mapping an address to a cell. In addition,
  #    don't consider free cells during the search.

  VmAllocator* = object
    # XXX: support for memory regions going past the 0..2^63 (or 0..2^31)
    #      region is very fuzzy at the moment due to the mixing of int
    #      and uint
    cells*: seq[VmCell]
      ## the underlying storage for the cell slots. Both stack and
      ## (non-refcounted) heap slots are included here

    # the intrusive linked list storing all free cells:
    freeHead*, freeTail*: int

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
    # future work: split everything related to the *production* of types into
    # a type placed in ``vmtypegen``
    lut*: Table[ItemId, PVmType] ## `PType`-id -> `PVmType` mappings

    structs*: TypeTable ## All structural types created by ``vmtypegen``

    signatures*: Table[RoutineSig, RoutineSigId]
    nextSigId*: RoutineSigId ## The ID to use for a new `signatures` entry

    types*: seq[PVmType] ## all generated types (including those created
                         ## during VM setup)

    staticInfo*: array[AtomKind, tuple[size, align: uint8]]
      ## size and alignment information for atoms where this information is
      ## the same for every instance

    # XXX: if `tyBool..tyFloat64` would only include all numeric types (plus
    #      bool and char), we could use the `numericTypes` below instead
    #numericTypes*: array[tyBool..tyFloat64, PVmType] ## A lookup table for all numeric types (ints/float)
    boolType*: PVmType
    charType*: PVmType
    stringType*: PVmType
    pointerType*: PVmType
    nodeType*: PVmType
    emptyType*: PVmType

    intTypes*: array[tyInt..tyInt64, PVmType]
    uintTypes*: array[tyUInt..tyUInt64, PVmType]
    floatTypes*: array[tyFloat..tyFloat64, PVmType]

    rootRef*: PVmType
      ## the VM type corresponding to ``ref RootObj``.
      # XXX: this is a temporary workaround, the type cache shouldn't need to
      #      know nor care about ``RootObj``. Can be removed once closure types
      #      are lowered earlier

  FunctionIndex* = distinct int

  # XXX: TCtx's contents should be separated into five parts (separate object
  #      types):
  #      - (DONE) 'execution state': stack frames, program counter, etc.;
  #        everything that makes up a single VM invocation. Mutated during
  #        execution
  #      - 'shared execution state': allocator, managed slots, etc.; state that
  #        is shared across VM invocations. Mutated during execution
  #      - 'execution environment': types, globals, constants, functions, etc.;
  #        populated by code-gen (vmgen) or the compilerapi and possibly
  #        reused across compiler invocations (relevant for IC). Not mutated
  #        by the execution engine
  #      - code and debug information
  #      - (DONE) 'vmgen' state: auxiliary data used during code generation,
  #        reused across vmgen invocations for efficiency

  CodeGenFlag* = enum
    cgfAllowMeta ## If not present, type or other meta expressions are
                 ## disallowed in imperative contexts and code-gen for meta
                 ## function arguments (e.g. `typedesc`) is suppressed

  VmGenDiagKind* = enum
    # has no extra data
    vmGenDiagTooManyRegistersRequired
    # has ast data
    vmGenDiagNotUnused
    vmGenDiagCannotEvaluateAtComptime
    # has magic data
    vmGenDiagMissingImportcCompleteStruct
    vmGenDiagCodeGenUnhandledMagic
    # has sym data
    vmGenDiagCannotImportc
    vmGenDiagTooLargeOffset
    vmGenDiagCannotCallMethod
    # has type mismatch data
    vmGenDiagCannotCast

  VmGenDiagKindAstRelated* =
    range[vmGenDiagNotUnused..vmGenDiagCannotEvaluateAtComptime]
    # TODO: this is a somewhat silly type, the range allows creating type safe
    #       diag construction functions -- see: `vmgen.fail`

  VmGenDiagKindSymRelated* =
    range[vmGenDiagCannotImportc..vmGenDiagCannotCallMethod]
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
      of vmGenDiagCannotImportc,
          vmGenDiagTooLargeOffset,
          vmGenDiagCannotCallMethod:
        sym*: PSym
      of vmGenDiagCannotCast:
        typeMismatch*: VmTypeMismatch
      of vmGenDiagMissingImportcCompleteStruct,
          vmGenDiagCodeGenUnhandledMagic:
        magic*: TMagic
      of vmGenDiagNotUnused,
          vmGenDiagCannotEvaluateAtComptime:
        ast*: PNode
      of vmGenDiagTooManyRegistersRequired:
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
    vmEvtIllegalConvFromXToY
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
      of vmEvtCannotCast, vmEvtIllegalConvFromXToY:
        typeMismatch*: VmTypeMismatch
      of vmEvtIndexError:
        indexSpec*: tuple[usedIdx, minIdx, maxIdx: Int128]
      of vmEvtErrInternal, vmEvtNilAccess, vmEvtIllegalConv,
          vmEvtFieldUnavailable, vmEvtFieldNotFound,
          vmEvtCacheKeyAlreadyExists, vmEvtMissingCacheKey:
        msg*: string
      of vmEvtCannotSetChild, vmEvtCannotAddChild, vmEvtCannotGetChild,
         vmEvtNoType, vmEvtNodeNotASymbol:
        ast*: PNode
      of vmEvtUnhandledException:
        exc*: PNode
        trace*: VmRawStackTrace
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

  VmRawStackTrace* = seq[tuple[sym: PSym, pc: PrgCtr]]

  HandlerTableEntry* = tuple
    offset: uint32 ## instruction offset
    instr:  uint32 ## position of the EH instruction to spawn a thread with

  EhOpcode* = enum
    ehoExcept
      ## unconditional exception handler
    ehoExceptWithFilter
      ## conditionl exception handler. If the exception is a subtype or equal
      ## to the specified type, the handler is entered
    ehoFinally
      ## enter the ``finally`` handler
    ehoNext
      ## relative jump to another instruction
    ehoLeave
      ## abort the parent thread
    ehoEnd
      ## ends the thread without treating the exception as handled

  EhInstr* = tuple
    ## Exception handling instruction. 8-byte in size.
    opcode: EhOpcode
    a: uint16 ## meaning depends on the opcode
    b: uint32 ## meaning depends on the opcode

  TCtx* = object
    code*: seq[TInstr]
    debug*: seq[TLineInfo]  # line info for every instruction; kept separate
                            # to not slow down interpretation
    ehTable*: seq[HandlerTableEntry]
      ## stores the instruction-to-EH mappings. Used to look up the EH
      ## instruction to start exception handling with in case of a normal
      ## instruction raising
    ehCode*: seq[EhInstr]
      ## stores the instructions for the exception handling (EH) mechanism
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

    flags*: set[CodeGenFlag] ## flags that alter the behaviour of the code
      ## generator. Initialized by the VM's callsite and queried by the JIT.
    # XXX: ^^ make this a part of the JIT state as soon as possible

    linking*: LinkerData
    # XXX: ^^ should be made part of the JIT state but ``vmcompilerserdes``
    #      currently blocks that

    module*: PSym
    callsite*: PNode
    mode*: TEvalMode
    features*: TSandboxFlags
    traceActive*: bool
    comesFromHeuristic*: TLineInfo # Heuristic for better macro stack traces
    callbacks*: seq[VmCallback]
    cache*: IdentCache
    config*: ConfigRef
    graph*: ModuleGraph
    idgen*: IdGenerator
    profiler*: Profiler
    templInstCounter*: ref int # gives every template instantiation a unique ID, needed here for getAst
    vmstateDiff*: seq[(PSym, PNode)] # we remember the "diff" to global state here (feature for IC)
    vmTraceHandler*: TraceHandler ## handle trace output from an executing vm

  StackFrameIndex* = int

  TStackFrame* = object
    prc*: PSym                 # current prc; proc that is evaluated
    slots*: seq[TFullReg]      # parameters passed to the proc + locals;
                              # parameters come first
    eh*: HOslice[int]
      ## points to the active list of instruction-to-EH mappings
    baseOffset*: PrgCtr
      ## the instruction that all offsets in the instruction-to-EH list are
      ## relative to. Only valid when `eh` is not empty

    comesFrom*: int

  ProfileInfo* = object
    ## Profiler data for a single procedure.
    time*: float ## the time spent on executing instructions (inclusive)
    count*: int  ## the number of instructions executed (exclusive)

  Profiler* = object
    # XXX: move this type to the ``vmprofiler`` module once possible
    enabled*: bool ## whether profiling is enabled
    tEnter*: float ## the point-in-time when the active measurment started

    data*: Table[PSym, ProfileInfo]
      ## maps the symbol of a procedure to the associated data gathered by the
      ## profiler

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

proc initCtx*(module: PSym; cache: IdentCache; g: ModuleGraph;
             idgen: IdGenerator, tracer: TraceHandler = defaultTracer): TCtx =
  result = TCtx(
    code: @[],
    debug: @[],
    globals: @[],
    constants: @[],
    module: module,
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

  result.profiler.enabled = optProfileVM in g.config.globalOptions

func refresh*(c: var TCtx, module: PSym; idgen: IdGenerator) =
  addInNimDebugUtils(c.config, "refresh")
  c.module = module
  c.idgen = idgen

const pseudoAtomKinds* = {akObject, akArray}
const realAtomKinds* = {low(AtomKind)..high(AtomKind)} - pseudoAtomKinds

# flag is used to signal opcSeqLen if node is NimNode.
const nimNodeFlag* = 16

template opcode*(x: TInstr): TOpcode = TOpcode(x.TInstrType shr regOShift and regOMask)
template regA*(x: TInstr): TRegister = TRegister(x.TInstrType shr regAShift and regAMask)
template regB*(x: TInstr): TRegister = TRegister(x.TInstrType shr regBShift and regBMask)
template regC*(x: TInstr): TRegister = TRegister(x.TInstrType shr regCShift and regCMask)
template regBx*(x: TInstr): int = (x.TInstrType shr regBxShift and regBxMask).int

template jmpDiff*(x: TInstr): int = regBx(x) - wordExcess

template isNil*(p: VmMemPointer | CellPtr): bool = p.rawPointer == nil
template isNil*(h: HeapSlotHandle): bool = ord(h) == 0
template isNotNil*(h: HeapSlotHandle): bool = ord(h) != 0

const noneType*: PVmType = nil

template isValid*(t: PVmType): bool = t != nil

template rawPointer*(p: VmMemPointer | CellPtr): untyped =
  (ptr UncheckedArray[byte])(p)

template rawPointer*(h: LocHandle): untyped =
  h.p.rawPointer

template subView*[T](x: openArray[T], off, len): untyped =
  x.toOpenArray(off, int(uint(off) + uint(len) - 1))

template subView*[T](x: openArray[T], off): untyped =
  x.toOpenArray(int off, x.high)

template byteView*(x: ptr UncheckedArray[byte], t: PVmType; num: int): untyped =
  x.toOpenArray(0, (t.alignedSize.int * num) - 1)

template byteView*(x: VmMemPointer | CellPtr, t: PVmType; num: int): untyped =
  var p = x.rawPointer
  byteView(p, t, num)

func applyOffset*(p: VmMemPointer | CellPtr, offset: uint): VmMemPointer {.inline.} =
  cast[VmMemPointer](unsafeAddr p.rawPointer[offset])

func newVmString*(data: CellPtr, len: Natural): VmString {.inline.} =
  VmString(VmSeq(data: data, length: len))

func len*(s: VmString): int {.inline.} = VmSeq(s).length

func data*(s: VmString): CellPtr {.inline.} = VmSeq(s).data

template isValid*(handle: LocHandle): bool =
  ## Checks if `handle` is a valid handle, that is, whether it stores non-nil
  ## pointer and type information. **NOTE**: this is only meant for debugging
  ## and assertions -- the procedure does **not** check whether accessing the
  ## referenced memory location is legal
  let x = handle
  x.p.rawPointer != nil and x.typ != nil

template currentException*(a: VmArgs): HeapSlotHandle =
  ## A temporary workaround for the exception handle being stored as a pointer
  a.currentExceptionPtr[]

template `currentException=`*(a: VmArgs, h: HeapSlotHandle) =
  ## A temporary workaround for the exception handle being stored as a pointer
  a.currentExceptionPtr[] = h

func unpackedConvDesc*(info: uint16
                      ): tuple[op: NumericConvKind, dstbytes, srcbytes: int] =
  ## Unpacks the numeric conversion description from `info`.
  result.op = NumericConvKind(info and 0x7)
  # note: add 1 to undo the shifting done during packing
  result.dstbytes = 1 + int((info shr 3) and 0x7)
  result.srcbytes = 1 + int((info shr 6) and 0x7)

func packedConvDesc*(op: NumericConvKind, dstbytes, srcbytes: range[1..8]): uint16 =
  ## Packs the numeric conversion description into a single ``uint16``.
  template pack(s: int): uint16 =
    # substract one from the size values so that they fit into 3 bit
    uint16(s - 1)
  result = uint16(op) or (pack(dstbytes) shl 3) or (pack(srcbytes) shl 6)

# TODO: move `overlap` and its tests somewhere else
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

func `$`*(x: ptr UncheckedArray[byte]): string =
  $cast[int](x)

func `$`*(x: PVmType): string =
  result = "PVmType(" & $x.kind & ")"

# TODO: move `safeCopyMem` and `safeZeroMem` somewhere else

func safeCopyMem*(dest: var openArray[byte|char], src: openArray[byte|char], numBytes: Natural) {.inline.} =
  # TODO: Turn the asserts into range checks instead
  assert numBytes <= dest.len
  assert numBytes <= src.len, $numBytes & ", " & $src.len
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
