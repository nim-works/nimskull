## Implements the (mostly) target-language agnostic IR/IL used as the input
## for the code generators. Abstraction wise, it's a little higher level than
## C, but a lot simpler and with less quirks.

import
  std/[
    tables
  ],
  std/private/[
    containers
  ],
  compiler/ic/[
    bitabs
  ]

# it's likely that the module is imported into modules where `mirtrees` is
# also present, so to remove the need for qualification, the `StringId` type
# from `mirtrees` is simply reused/reexported
from compiler/mir/mirtrees import StringId, `==`

export StringId, LitId, `==`

type
  CgNodeKind* = enum
    cnkInvalid       ## the node is uninitialized
    # terminals:
    cnkBool   ## bool value
    cnkInt    ## signless packed integer value
    cnkFloat  ## packed float value
    cnkString ## packed interned string value
    cnkLabel  ## label name (32-bit uint)
    cnkType   ## reference to a type (by name)
    cnkDatum  ## reference to an anonymous (by ID)
    cnkProc   ## reference to procedure (by name)
    cnkGlobal ## reference to global (by name)
    cnkLocal  ## reference to local (by name)
    # non-terminals:
    cnkUnwind
    cnkUnknown
    cnkNilLit
    cnkValue
    cnkPath
    cnkExtField
    cnkNeg
    cnkAdd, cnkSub, cnkMul, cnkDiv, cnkMod
    cnkCheckedAdd, cnkCheckedSub, cnkCheckedMul
    cnkNot
    cnkEq, cnkLe, cnkLt
    cnkBitNot
    cnkBitAnd, cnkBitOr, cnkBitXor
    cnkShl, cnkShr
    cnkUse
    cnkLoad, cnkAddr
    cnkBitcast, cnkPtrCast, cnkConv
    cnkZext, cnkSext, cnkTrunc,
    cnkPromote, cnkDemote
    cnkFToI, cnkFToU, cnkUToF, cnkIToF
    cnkSizeof, cnkAlignof, cnkOffsetof
    cnkUnlikely
    cnkEmit, cnkAsm
    cnkStmtList
    cnkBlock
    cnkScope
    cnkTry
    cnkIf
    cnkDispatch
    cnkTarget
    cnkWhile
    cnkBreak, cnkRaise, cnkReturn, cnkUnreachable
    cnkDef
    cnkAsgn, cnkStore
    cnkDrop
    cnkCheckedCall
    cnkCheckedCallAsgn
    cnkTailCall
    cnkCall
    cnkConstr, cnkRecConstr, cnkFieldInit
    cnkParams, cnkParam
    cnkProcImp, cnkProcDef, cnkProcExp
    cnkGlobalImp, cnkGlobalDef, cnkGlobalExp
    cnkVoidTy, cnkBoolTy, cnkCharTy, cnkUIntTy, cnkIntTy, cnkFloatTy
    cnkOpaqueTy, cnkStructTy, cnkUnionTy, cnkArrayTy
    cnkPtrTy, cnkProcTy
    cnkField
    cnkVarargs

  CgTypeKind* = range[cnkVoidTy..cnkProcTy]

  CgNode* = object
    ## A single packed node in an abstract syntax tree.
    kindInfo*: uint32
      ## 8-bit kind; 24-bit info index (0 meaning 'none')
    val*: uint32
      ## for terminals, meaning depends on the kind
      ## for non-terminals, the number of child nodes

  Ast* = seq[CgNode]
    ## Packed storage for AST.
  NodeIndex* = distinct uint32
    ## Index of a node in an `Ast`.
  Datum* = distinct uint32
    ## ID of an anonymous constant.

  SourceLoc* = object
    ## Source location information.
    line*: uint16   ## line number; '0' means invalid
    column*: uint16 ## column number. '0' means "full line"
    file*: StringId ## full path of the source file

  AsmMode* = enum
    ## What an inline asm statement represents.
    asmMsvc ## MSVC inline assembly syntax
    asmGnu  ## GNU inline assembly syntax
    asmJs   ## inline JavaScript

  CgStorage* {.pure.} = enum
    Normal ## mutable global storage
    Thread ## thread-local storage
    Const  ## constant global storage

  CgProcAttrib* {.pure.} = enum
    Inline   ## try to inline the body where possible
    NoInline ## never inline the body at a callsite

  CgParamAttrib* {.pure.} = enum
    ## Parameter attributes.
    NoAlias ## C-specific attribute; may be ignored

  CgLocAttrib* {.pure.} = enum
    ## Attributes for locals, globals, and fields.
    NoAlias  = 0 ## C-specific attribute; may be ignored
    Register = 1 ## C-specific attribute; may be ignored
    Volatile = 2 ## C-specific attribute; may be ignored

  CgCallConv* {.pure.} = enum
    ## Calling convention.
    Default   ## decided by the implementation
    Nimcall   ## calling convention for NimSkull procedures
    Stdcall,  ## C stdcall
    Cdecl,    ## C cdecl
    Safecall, ## C safecall
    Syscall,  ## C syscall
    Fastcall  ## C fastcall

  CgModule* = object
    ## A CGIR module using an in-memory representation meant for
    ## easy processing.
    tast*: Ast
      ## all AST for type bodies
    ast*: Ast
      ## all AST for everything that's not types
    data*: Store[Datum, NodeIndex]
      ## ID of constant -> index of construction expression AST in `ast`
      ## representing the constant/datum
    types*: Table[StringId, NodeIndex]
      ## type name -> index of corresponding body in `tast`
    globals*: Table[StringId, NodeIndex]
      ## name of global -> index of corresponding declaration in `ast`
    procs*: Table[StringId, NodeIndex]
      ## name of proc -> index of coressponding declaration in `ast`
    infos*: seq[SourceLoc]
      ## source location information stored out-of-band
    strings*: BiTable[string]
      ## interned string values
    numbers*: BiTable[uint64]
      ## out-of-band bit patterns, backing large int, uint, and float values

const
  FirstNonTerminal = cnkUnwind
  AllNodes*  = {low(CgNodeKind)..high(CgNodeKind)}
  cnkExprs*  = {cnkNilLit .. cnkOffsetof, cnkCall} - {cnkExtField}
  cnkBlocks* = {cnkBlock, cnkTry, cnkScope}
  cnkStmts*  = {cnkEmit .. cnkTailCall, cnkCall} - {cnkTarget} - cnkBlocks
  cnkSyms*   = {cnkLocal, cnkGlobal, cnkProc, cnkDatum, cnkUnknown}

  OverflowBit = 1'u32 shl 31
    ## the bit indicating whether the value stored in a node is
    ## stored out-of-band

template kind*(n: CgNode): CgNodeKind =
  CgNodeKind(uint8(n.kindInfo))

template info*(n: CgNode): uint32 =
  (n.kindInfo shr 8)

template isLeaf*(n: CgNode): bool =
  ## Whether `n` is a terminal node.
  ord(n.kind) < ord(FirstNonTerminal)

template node*(k: CgNodeKind): CgNode =
  CgNode(kindInfo: uint32(k))

template node*(k: CgNodeKind, v: uint32): CgNode =
  CgNode(kindInfo: uint32(k), val: v)

template node*(k: CgNodeKind, info, v: uint32): CgNode =
  CgNode(kindInfo: uint32(k) or (info shl 8), val: v)

# ---- tree queries and traversal ----

func len*(ast: Ast, n: NodeIndex): int {.inline.} =
  ## Returns the number of direct child nodes the node at `n` has.
  assert not isLeaf(ast[ord n])
  ast[ord n].val.int

func next*(ast: Ast, n: NodeIndex): NodeIndex {.inline.} =
  ## Returns the index of the following sibling node, or, if there's no
  ## sibling, the index of the parent node's sibling.
  var i = n.ord
  var last = i
  while i <= last:
    let node = ast[i]
    if not isLeaf(node):
      inc last, node.val.int
    inc i
  result = NodeIndex(i)

func child*(ast: Ast, n: NodeIndex, i: SomeInteger): NodeIndex {.inline.} =
  ## Returns the index of the `i`-th child node of `n`.
  result = NodeIndex(ord(n) + 1)
  for _ in 0..<i:
    result = ast.next(result)

func last*(ast: Ast, n: NodeIndex): NodeIndex {.inline.} =
  ## Returns the index of the last child node of `n`.
  ast.child(n, ast.len(n) - 1)

func `[]`*(ast: Ast, n: NodeIndex): CgNode {.inline.} =
  ast[ord n]

func `[]`*(ast: Ast, n: NodeIndex, i: SomeInteger): CgNode {.inline.} =
  ast[ast.child(n, i)]

func span*(ast: Ast, n: NodeIndex): int =
  ## Computes the number of nodes the sub-tree at `n` spans.
  var n = n.uint32
  let start = n
  var fin = start + 1
  while n < fin:
    if not isLeaf(ast[n]):
      fin += ast[n].val
    inc n
  result = int(n - start)

# ---- modification ----

func append*(dst: var Ast, src: Ast, n: NodeIndex): NodeIndex =
  ## Appends the sub-tree at `n` in `src` to the `dst`.
  let count = span(src, n)
  let start = dst.len
  dst.setLen(start + count)
  copyMem(addr dst[start], addr src[n.ord], sizeof(CgNode) * count)
  result = NodeIndex(start)

func append*(dst: var Ast, src: Ast): NodeIndex =
  ## Appends `src` to `dst`.
  let start = dst.len
  dst.add(src)
  result = NodeIndex(start)

# ---- packing/unpacking ----

func put*(m: var CgModule, val: sink string): StringId =
  cast[StringId](m.strings.getOrIncl(val))

func get*(m: CgModule, s: StringId): lent string {.inline.} =
  ## Retrieves a string previously added to the module via
  ## `put <#put,CgModule,sinkstring>`_.
  m.strings[cast[LitId](s)]

func pack*(m: var CgModule, i: int64): uint32 {.inline.} =
  ## Packs `i` and returns the packed representation.
  if i >= 0 and i <= high(int32):
    cast[uint32](i)
  else:
    cast[uint32](m.numbers.getOrIncl(cast[uint64](i))) or OverflowBit

func unpackInt*(m: CgModule, p: uint32): int64 {.inline.} =
  ## Retrieves the signed interpretation for the packed int previously added
  ## to `m` via `pack <#pack,CgModule,int64>`_.
  if (p shr 31) == 1:
    cast[int64](m.numbers[LitId(p and not OverflowBit)])
  else:
    cast[int64](p)

func unpackUInt*(m: CgModule, p: uint32): uint64 {.inline.} =
  ## Retrieves the unsigned interpretation for the packed int previously added
  ## to `m` via `pack <#pack,CgModule,int64>`_.
  if (p shr 31) == 1:
    m.numbers[LitId(p and not OverflowBit)]
  else:
    uint64(p)

func pack*(m: var CgModule, f: float64): uint32 {.inline.} =
  ## Packs `f` and returns the packed representation.
  cast[uint32](m.numbers.getOrIncl(cast[uint64](f)))

func unpackFloat*(m: CgModule, p: uint32): float64 {.inline.} =
  ## Retrieves the float64 for the packed float previously added to `m`
  ## via `pack <#pack,CgModule,float64>`_.
  cast[float64](m.numbers[LitId(p)]) # always packed

# ---- 'is' routines ----

{.push inline.}

func isBool*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid bool terminal.
  n.kind == cnkBool and n.val in {0, 1}

func isInt*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid int terminal.
  n.kind == cnkInt and
    ((n.val and OverflowBit) == 0 or
     m.numbers.hasLitId(LitId(n.val and not OverflowBit)))

func isFloat*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid float terminal.
  n.kind == cnkFloat and m.numbers.hasLitId(cast[LitId](n.val))

func isString*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid string terminal.
  n.kind == cnkString and m.strings.hasLitId(LitId(n.val))

func isLocal*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid local terminal (whether the local actually exists
  ## is not considered).
  n.kind == cnkLocal and m.strings.hasLitId(LitId(n.val))

func isGlobal*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid global terminal (whether the global actually exists
  ## is not considered).
  n.kind == cnkGlobal and m.strings.hasLitId(LitId(n.val))

func isProc*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid proc terminal (whether the proc actually exists
  ## is not considered).
  n.kind == cnkProc and m.strings.hasLitId(LitId(n.val))

func isType*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid type terminal (whether the type actually exists
  ## is not considered).
  n.kind == cnkType and m.strings.hasLitId(LitId(n.val))

func isDatum*(m: CgModule, n: CgNode): bool =
  ## Whether `n` is a valid datum terminal (whether the datum actually exists
  ## is not considered).
  n.kind == cnkDatum

{.pop.}
