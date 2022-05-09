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

import tables

import
  compiler/ast/[
    ast,
    idents,
    lineinfos,
    reports,
  ],
  compiler/modules/[
    modulegraphs,
  ],
  compiler/front/[
    options,
  ],
  compiler/utils/[
    debugutils
  ]

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

  TSandboxFlag* = enum        ## what the evaluation engine should allow
    allowCast,                ## allow unsafe language feature: 'cast'
    allowInfiniteLoops        ## allow endless loops
  TSandboxFlags* = set[TSandboxFlag]

  TSlotKind* = enum   # We try to re-use slots in a smart way to
                      # minimize allocations; however the VM supports arbitrary
                      # temporary slot usage. This is required for the parameter
                      # passing implementation.
    slotEmpty,        # slot is unused
    slotFixedVar,     # slot is used for a fixed var/result (requires copy then)
    slotFixedLet,     # slot is used for a fixed param/let
    slotTempUnknown,  # slot but type unknown (argument of proc call)
    slotTempInt,      # some temporary int
    slotTempFloat,    # some temporary float
    slotTempStr,      # some temporary string
    slotTempComplex,  # some complex temporary (s.node field is used)
    slotTempPerm      # slot is temporary but permanent (hack)


  TFullReg* = object  # with a custom mark proc, we could use the same
                      # data representation as LuaJit (tagged NaNs).
    case kind*: TRegisterKind
    of rkNone: nil
    of rkInt: intVal*: BiggestInt
    of rkFloat: floatVal*: BiggestFloat
    of rkNode: node*: PNode
    of rkRegisterAddr: regAddr*: ptr TFullReg
    of rkNodeAddr: nodeAddr*: ptr PNode

  PProc* = ref object
    blocks*: seq[TBlock]    # blocks; temp data structure
    sym*: PSym
    regInfo*: seq[tuple[inUse: bool, kind: TSlotKind]]

  VmArgs* = object
    ra*, rb*, rc*: Natural
    slots*: ptr UncheckedArray[TFullReg]
    currentException*: PNode
    currentLineInfo*: TLineInfo
  VmCallback* = proc (args: VmArgs) {.closure.}

  PCtx* = ref TCtx
  TCtx* = object of TPassContext # code gen context
    code*: seq[TInstr]
    debug*: seq[TLineInfo]  # line info for every instruction; kept separate
                            # to not slow down interpretation
    sframes*: seq[TStackFrame] ## The stack of the currently running code
    globals*: PNode         #
    constants*: PNode       # constant data
    types*: seq[PType]      # some instructions reference types (e.g. 'except')
    currentExceptionA*, currentExceptionB*: PNode
    exceptionInstr*: int # index of instruction that raised the exception
    prc*: PProc
    module*: PSym
    callsite*: PNode
    mode*: TEvalMode
    features*: TSandboxFlags
    traceActive*: bool
    loopIterations*: int
    comesFromHeuristic*: TLineInfo # Heuristic for better macro stack traces
    callbacks*: seq[tuple[key: string, value: VmCallback]]
    errorFlag*: Report
    cache*: IdentCache
    config*: ConfigRef
    graph*: ModuleGraph
    oldErrorCount*: int
    profiler*: Profiler
    templInstCounter*: ref int # gives every template instantiation a unique ID, needed here for getAst
    vmstateDiff*: seq[(PSym, PNode)] # we remember the "diff" to global state here (feature for IC)
    procToCodePos*: Table[int, int]

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

proc newCtx*(module: PSym; cache: IdentCache; g: ModuleGraph; idgen: IdGenerator): PCtx =
  PCtx(
    code: @[],
    debug: @[],
    globals: newNode(nkStmtListExpr),
    constants: newNode(nkStmtList),
    types: @[],
    prc: PProc(blocks: @[]),
    module: module,
    loopIterations: g.config.maxLoopIterationsVM,
    comesFromHeuristic: unknownLineInfo,
    callbacks: @[],
    errorFlag: reportEmpty,
    cache: cache,
    config: g.config,
    graph: g,
    idgen: idgen
  )

func refresh*(c: var TCtx, module: PSym; idgen: IdGenerator) =
  addInNimDebugUtils(c.config, "refresh")
  c.module = module
  c.prc = PProc(blocks: @[])
  c.loopIterations = c.config.maxLoopIterationsVM
  c.idgen = idgen

proc registerCallback*(c: var TCtx; name: string; callback: VmCallback): int {.discardable.} =
  result = c.callbacks.len
  c.callbacks.add((name, callback))

template registerCallback*(c: PCtx; name: string; callback: VmCallback): int {.deprecated.} =
  ## A transition helper. Use the `registerCallback` proc that takes
  ## `var TCtx` instead
  registerCallback(c[], name, callback)

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
