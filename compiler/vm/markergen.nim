## This module implements the generation of marker procedures that are meant
## to be used by the various GCs.
##
## A marker procedure is responsible for applying GC related operations to the
## locations owned by a cell (garbage collected location). In case a marker
## procedure is not specified for a GC'ed type, the GC uses RTTI-based cell
## traversal, so specialized marker procs are only an optimization.

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_types
  ],
  compiler/vm/[
    bitsetutils,
    irpasses,
    pass_helpers,
    vmir
  ]

from compiler/vm/vmdef import unreachable
from compiler/vm/cpasses import genMatch

type
  GenMarkerCtx = object
    op: IRIndex ## passed to the ``nimGCvisit`` calls

    gcLookup {.cursor.}: BitSet[TypeId] # not mutated

    envPtr: ptr TypeEnv # XXX: ``lent`` would be much better here
    pe: PassEnv

template env(c: GenMarkerCtx): untyped = c.envPtr[]

func genMarkerAux(c: GenMarkerCtx, cr: var IrCursor, data: var LiteralData, it: var RecordIter, path: IRIndex)

func genMarkerType(c: GenMarkerCtx, cr: var IrCursor, data: var LiteralData, path: IRIndex, id: TypeId) =
  template visit(p: IRIndex) =
    cr.insertCompProcCall(c.pe, "nimGCvisit", p, c.op)

  let typ = c.env[id]
  case typ.kind
  of tnkArray:
    # visit all items in the array
    genForLoop(cr, data, c.pe, cr.insertLit(data, typ.length)):
      let acc = cr.insertPathArr(path, counter)
      genMarkerType(c, cr, data, acc, typ.base)

  of tnkRecord:
    # visit the fields of the base type first
    if typ.base != NoneType:
      genMarkerType(c, cr, data, path, typ.base)

    var it = initRecordIter(c.env, id)
    genMarkerAux(c, cr, data, it, path)

  of tnkSeq, tnkString, tnkRef:
    # XXX: seqsv2 are not handled here. Should they?
    visit(path)

  of tnkClosure:
    visit(cr.insertMagicCall(c.pe, mAccessEnv, tyPointer, path))

  else:
    discard "not relevant"

func genMarkerField(c: GenMarkerCtx, cr: var IrCursor, data: var LiteralData, path: IRIndex, pos: int32, id: TypeId) =
  # don't generate anything for the field if it's type doesn't contain
  # GC'ed memory
  if id in c.gcLookup:
    let acc = cr.insertPathObj(path, pos.int16)
    genMarkerType(c, cr, data, acc, id)


func genMarkerAux(c: GenMarkerCtx, cr: var IrCursor, data: var LiteralData, it: var RecordIter, path: IRIndex) =
  # TODO: add a ``RecordIter`` that's basically just a ``RecordNodeIndex`` and
  #       use it here instead
  let n = next(c.env, it)

  case n.kind
  of rnkFields:
    for f in n.slice.items:
      let id = it.field(f)
      genMarkerField(c, cr, data, path, it.fieldPos(f), c.env[id].typ)

  of rnkList:
    for _ in 0..<n.len:
      genMarkerAux(c, cr, data, it, path)

  of rnkCase:
    let
      fn = next(c.env, it)
      discr = it.fieldPos(fn.slice.a)

    let
      val = cr.insertPathObj(path, discr.int16)

    let exit = cr.newJoinPoint()
    var next = exit

    # generate a case statement
    for i in 1..<n.len:
      let
        bId = nextId(c.env, it)
        b = c.env.rawBranch(bId)

      if i > 1:
        # the join after the previous branch
        cr.insertJoin(next)

      if i + 1 < n.len:
        # there exists another branch after this one
        next = cr.newJoinPoint()
      else:
        next = exit

      if b != NoneLit:
        # XXX: inserting a `bcMatch` here and letting a subsequent pass lower
        #      it would be cleaner. The problem: running the ``lowerMatchPass``
        #      is tricky at the place where the call to ``generateMarkerProcs``
        #      is currently located. This is nothing that should be of concern
        #      to ``genMarkerAux`` however
        cr.insertBranch(cr.insertMagicCall(c.pe, mNot, tyBool,
                                           genMatch(val, c.env[it.field(fn.slice.a)].typ, c.env.rawBranch(bId), data, c.env, c.pe, cr)),
                        next)
      else:
        discard "else branch"

      # the marker logic for the content of this branch:
      genMarkerAux(c, cr, data, it, path)
      cr.insertGoto(exit) # XXX: not necessary if it's the last branch

    cr.insertJoin(exit)

  of rnkBranch:
    unreachable("explictly handled in the `rnkCase` branch")
  of rnkEmpty:
    discard


func generateMarkerProcs*(typeInfos: Table[TypeId, SymId], pe: PassEnv,
                          gcLookup: BitSet[TypeId], types: TypeEnv, name: PIdent,
                          procs: var ProcedureEnv, data: var LiteralData,
                          impls: var seq[IrStore3]): Table[TypeId, ProcId] =
  ## Generates the marker procedure for each `ref` and `seq` type in
  ## `typeInfos` and returns a table that associates the types with their
  ## marker procedures
  # XXX: `typeInfos` should be an ``openArray`` instead of a ``Table``, but
  #      with how this proc is currently used, that would require the
  #      allocation of a `seq`. An even better solution would be to move the
  #      core logic (i.e. the contents of the loop below) into it's own
  #      procedure and use that at the callsite instead.
  #      Splitting up this procedure is a good idea in general - it requires
  #      too many parameters

  var c = GenMarkerCtx(pe: pe, envPtr: unsafeAddr types)
  # neither the source nor destination `gcLookup` is mutated, so as an
  # optimization, a shallow copy is used
  when defined(gcDestructors):
    c.gcLookup = gcLookup
  else:
    shallowCopy(c.gcLookup, gcLookup)

  for id in typeInfos.keys:
    if types.kind(id) in {tnkRef, tnkSeq}:
      # the marker procs has a signature, minus the parameter names, of
      # ``proc(p: point, op: int)``
      let prc = procs.add(pe.sysTypes[tyVoid], name, keepName=false)
      procs.mget(prc).params.add (nil, pe.sysTypes[tyPointer])
      procs.mget(prc).params.add (nil, pe.sysTypes[tyInt])

      # TODO: reuse the cursor's memory across iterations
      var cr: IrCursor
      let
        elemType = types.base(id)
        param = cr.insertDeref(cr.insertCast(id, cr.insertParam(0)))

      c.op = cr.insertParam(1)

      # if the element type is a type that doesn't need traversal (i.e. is
      # irrelevant to the GC), the marker proc is a no-op
      if elemType in gcLookup:
        case types.kind(id)
        of tnkRef:
          genMarkerType(c, cr, data, param, elemType)
        of tnkSeq:
          genForLoop(cr, data, c.pe, cr.insertPathObj(param, 0)):
            genMarkerType(c, cr, data, cr.insertPathArr(cr.insertPathObj(param, 2), counter), elemType)
        else:
          unreachable()

      # commit the procedure's body to the environment:
      sync(impls, procs)
      update(impls[prc.toIndex], cr)
      impls[prc.toIndex].owner = prc

      result[id] = prc