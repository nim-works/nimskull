discard """
  description: '''
    Test to make sure that `vmbackend.writeToFile` is compatible with
    `vmbackend.readFromFile`
  '''
  joinable: false
"""

import std/[os, tables]

import compiler/ast/[ast_types, lineinfos]
import compiler/ic/bitabs
import compiler/utils/pathutils
import compiler/vm/vmdef
import compiler/vm/packed_env {.all.}

func addAll[T](x: var BiTable[T], arr: varargs[T]) =
  for it in arr.items:
    discard x.getOrIncl(it)

func `==`(a, b: BranchListEntry): bool =
  if a.kind != b.kind: return false

  case a.kind
  of blekStart:
    a.field == b.field and a.numItems == b.numItems and
    a.defaultBranch == b.defaultBranch and a.numBranches == b.numBranches
  of blekBranch, blekEnd:
    a.fieldRange == b.fieldRange

func `==`(a, b: TInstr): bool {.borrow.}
func `==`(a, b: packed_env.NodeId): bool {.borrow.}

var env: PackedEnv
var enc: PackedEncoder

# fill `env` with random non-zero data
block:
  let
    typId1 = VmTypeId(1)
    typId2 = VmTypeId(2)

  env.strings.addAll("a", "b")
  env.numbers.addAll(1, 2)
  env.files = @["a", "b"]

  env.infos.addAll:
    [TLineInfo(fileIndex: FileIndex(1), line: 2, col: 3),
     TLineInfo(fileIndex: FileIndex(2), line: 3, col: 4)]

  env.dbgSyms = @[(LitId(1), LitId(2)), (LitId(3), LitId(4))]

  env.nodes = @[PackedDataNode(kind: pdkInt), PackedDataNode(kind: pdkFloat)]
  env.consts = @[(cnstFloat, 1'u32), (cnstString, 2'u32)]
  env.cconsts = @[(typId1, 6'u32)]


  let vmTyp1 = PVmType(kind: akSet, sizeInBytes: 2, alignment: 3,
                       setLength: 4)
  let vmTyp2 = PVmType(kind: akObject, sizeInBytes: 4, alignment: 1,
                       objFields: @[(2, vmTyp1)],
                       branches: @[BranchListEntry(kind: blekStart),
                                   BranchListEntry(kind: blekEnd)])

  enc.typeMap[vmTyp1] = typId1
  enc.typeMap[vmTyp2] = typId2

  env.types.add enc.storeVmType(env, vmTyp1)
  env.types.add enc.storeVmType(env, vmTyp2)

  env.globals = @[typId2]
  env.functions =
    @[(4'u32, RoutineSigId(18), typId1, typId2, ckCallback, 3'u32, 2'u32)]
  env.callbacks = @["cb1", "cb2"]

  env.code = @[TInstr(14), TInstr(15)]
  env.debug = @[128'u32, 256'u32]

  # the content doesn't matter. Just make sure that each field in the
  # destination `PackedNodeLite`, `PackedSymLit`, and `PackedTypeLite` objects
  # has a non-zero value
  let typ = PType(kind: tyProc, flags: {tfNoSideEffect}, sons: @[PType(nil)],
                  n: PNode(kind: nkStmtList), callConv: ccClosure)
  let sym = PSym(kind: skProc, magic: mSlice, name: PIdent(s: "slice"),
                 typ: typ,  offset: 1, position: 2)
  let typ2 = PType(kind: tyString, flags: {tfVarargs}, sons: @[typ],
                   n: PNode(kind: nkStrLit, flags: {nfBase2}, typ: typ),
                   sym: sym)

  let typeInfo = VmTypeInfo(nimType: typ2, internal: vmTyp2)

  # put the types into `env`
  storeTypeInfos(enc, env, [typeInfo])


let file = AbsoluteFile(getTempDir() / "tmp.nimbc")

doAssert writeToFile(env, file) == RodFileError.ok

var inEnv: PackedEnv
doAssert readFromFile(inEnv, file) == RodFileError.ok

# the data we've read must be the same as the one we wrote
doAssert env == inEnv