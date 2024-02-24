## Implements the MIR pass for:
## * replacing copy and move assignments with the ``=copy`` or ``=sink``
##   hook (if available for the type).
## * replacing destroy operations with calls to the ``=destroy`` hook (if
##   available for the type)

import
  std/[
    tables
  ],
  compiler/ast/[
    ast_query,
    ast_types,
    lineinfos
  ],
  compiler/front/[
    msgs,
    options
  ],
  compiler/mir/[
    mirbodies,
    mirchangesets,
    mirconstr,
    mirenv,
    mirtrees
  ],
  compiler/modules/[
    modulegraphs
  ],
  compiler/sem/[
    sighashes
  ],
  compiler/utils/[
    idioms
  ]

# XXX: reports are a code smell meaning data types are misplaced
from compiler/ast/reports_sem import SemReport
from compiler/ast/report_enums import ReportKind

# XXX: temporary dependency until switch assignments are lowered differently
from compiler/sem/injectdestructors import buildVoidCall

from compiler/sem/liftdestructors import boolLit, cyclicType

type
  LocalDiagKind = enum
    ldkPassCopyToSink       ## a copy is introduced in a consume context
    ldkUnavailableTypeBound ## a type-bound operator is requested but not
                            ## available

  LocalDiag = object
    ## A temporary diagnostic representation that is later turned into a
    ## ``SemReport``
    pos: NodePosition ## the location of the report
    case kind: LocalDiagKind
    of ldkUnavailableTypeBound:
      op: TTypeAttachedOp
    of ldkPassCopyToSink:
      discard

const
  skipAliases = {tyGenericInst, tyAlias, tySink}

proc getOp*(g: ModuleGraph, t: PType, kind: TTypeAttachedOp): PSym =
  ## Returns the symbol for the `kind` type-bound hook for `t` (or nil, if
  ## there's none).
  let t = t.skipTypes(skipForHooks)
  result = getAttachedOp(g, t, kind)
  if result == nil or result.ast.isGenericRoutine:
    # give up and find the canonical type instead:
    let h = sighashes.hashType(t, {CoType, CoDistinct})
    let canon = g.canonTypes.getOrDefault(h)
    if canon != nil:
      result = getAttachedOp(g, canon, kind)

proc isUsedForSink(tree: MirTree, stmt: NodePosition): bool =
  ## Computes whether the definition statement is something produced for
  ## sink parameter handling.
  assert tree[stmt].kind in {mnkDef, mnkDefUnpack}
  let def = tree.operand(stmt, 0)
  if tree[def].kind != mnkTemp:
    # only temporaries are used for sink handling
    return

  # look for whether the temporary is used as a 'consume' node's operand,
  # but do reduce the amount of work by not searching beyond the
  # temporary's lifetime
  # HACK: this detection relies on the code shapes ``mirgen`` currently
  #       emits for sink parameters and is thus very brittle. The proper
  #       solution is to mark through a side channel the statement as being
  #       generated for a sink parameter
  var
    n = tree.sibling(stmt)
    depth = 0
  while n < NodePosition tree.len:
    case tree[n].kind
    of mnkConsume:
      let x = tree.operand(n)
      if tree[x].kind == mnkTemp and tree[x].temp == tree[def].temp:
        # the temporary is used for sink parameter passing
        result = true
        break
    of mnkScope:
      inc depth
    of mnkEnd:
      if tree[n].kind == mnkScope:
        dec depth
        if depth < 0:
          # the end of the temporary's surrounding scope is reached
          break
    else:
      discard

    inc n

proc reportDiagnostics(g: ModuleGraph, body: MirBody,
                       owner: PSym, diags: var seq[LocalDiag]) =
  ## Reports all diagnostics in `diags` as ``SemReport``s and clear the list
  for diag in diags.items:
    let ast = body.sourceFor(diag.pos)
    let rep =
      case diag.kind
      of ldkUnavailableTypeBound:
        SemReport(kind: rsemUnavailableTypeBound,
                  typ: body[diag.pos].typ,
                  str: AttachedOpToStr[diag.op],
                  ast: ast,
                  sym: owner)
      of ldkPassCopyToSink:
        SemReport(kind: rsemCopiesToSink, ast: ast)

    localReport(g.config, ast.info, rep)

func couldIntroduceCycle(tree: MirTree, dest: NodePosition): bool =
  # copies to locals or globals can't introduce cyclic structures, as
  # both are standlone and not part of any other structure
  tree[dest].kind notin {mnkLocal, mnkTemp, mnkParam, mnkGlobal}

template genCopy(bu: var MirBuilder, graph: ModuleGraph, env: var MirEnv,
             op: PSym, tree: MirTree, dst: NodePosition,
             maybeCyclic: bool, src: untyped) =
  ## Emits a ``=copy`` hook call with `dst` and `src` as the arguments. If ORC
  ## is enabled, an additional bool value is passed to the hook, informing
  ## whether a reference cycle might be created at run-time.
  bu.buildVoidCall(env, op):
    bu.emitByName ekMutate:
      bu.emitFrom(tree, dst)
    bu.subTree mnkArg:
      src

    if graph.config.selectedGC == gcOrc and
       cyclicType(tree[dst].typ.skipTypes(skipAliases + {tyDistinct}), graph):
      # pass whether the copy can potentially introduce cycles as the third
      # parameter:
      let c = maybeCyclic and couldIntroduceCycle(tree, dest)
      bu.emitByVal literal(boolLit(graph, unknownLineInfo, c))

proc genDestroy*(bu: var MirBuilder, graph: ModuleGraph, env: var MirEnv,
                 target: Value) =
  ## Emits a destructor call with `target` as the argument.
  let destr = getOp(graph, target.typ, attachedDestructor)
  bu.buildVoidCall(env, destr):
    bu.emitByName(target, ekMutate)

proc injectHooks*(body: MirBody, graph: ModuleGraph, env: var MirEnv,
                  owner: PSym, changes: var Changeset) =
  ## Replaces all copy and move assignments for locations with lifetime hooks
  ## to the types' respective hook.
  var diags: seq[LocalDiag]
  template tree: MirTree = body.code

  for i, n in tree.pairs:
    case n.kind
    of mnkCopy:
      let
        stmt = tree.parent(i)
        typ  = tree[stmt, 0].typ

      if not hasDestructor(typ):
        # nothing to insert
        continue

      let
        dest = tree.child(stmt, 0)
        src  = tree.child(i, 0)
        op   = getOp(graph, typ, attachedAsgn)

      if sfError in op.flags:
        # emit an error if the hook is not available, but still continue
        diags.add LocalDiag(pos: src, kind: ldkUnavailableTypeBound,
                            op: attachedAsgn)

      if tree[stmt].kind == mnkDef and isUsedForSink(tree, stmt):
        # emit a warning for copies-to-sink:
        diags.add LocalDiag(pos: src, kind: ldkPassCopyToSink)

      case tree[stmt].kind
      of mnkDef, mnkDefUnpack:
        # turn a ``def x = copy a.b`` into:
        #   def x
        #   =copy(name x, arg a.b)
        changes.replace(tree, i): MirNode(kind: mnkNone)
        changes.insert(tree, tree.sibling(stmt), i, bu):
          # the destination is a local; the assignment thus cannot introduce a
          # cycle
          genCopy(bu, graph, env, op, tree, dest, false):
            bu.emitFrom(tree, src)
      of mnkInit:
        # we know the destination cannot overlap with the source. Replace
        # ``x := copy a.b`` with:
        #   =copy(name x, arg a.b)
        changes.replaceMulti(tree, stmt, bu):
          genCopy(bu, graph, env, op, tree, dest, true):
            bu.emitFrom(tree, src)
      of mnkAsgn:
        # the source and destination could overlap. Replace ``x = copy a.b``
        # with:
        #   def_cursor _1 = a.b
        #   =copy(name x, arg _1)
        # XXX: the temporary could be omitted in more cases by using proper
        #      alias analysis
        changes.replaceMulti(tree, stmt, bu):
          let tmp = bu.inline(tree, src)
          genCopy(bu, graph, env, op, tree, dest, true):
            bu.use(tmp)
      else:
        unreachable(tree[stmt].kind)

    of mnkMove:
      let
        stmt = tree.parent(i)
        typ  = tree[stmt, 0].typ

      if not hasDestructor(typ) or
         tree[stmt].kind in {mnkDef, mnkDefUnpack, mnkInit}:
        # nothing to do if:
        # * the type has no hooks
        # * it's guaranteed that there's no value in the destination
        continue

      let
        dest = tree.child(stmt, 0)
        src  = tree.child(i, 0)
        op   = getOp(graph, typ, attachedSink)

      # note: the move analyzer has to make sure that the source operand
      # doesn't overlap with the destination, so no temporary for the source is
      # needed
      if op != nil:
        # replace ``x = move a.b`` with:
        #   =sink(name x, arg a.b)
        changes.replaceMulti(tree, stmt, bu):
          bu.buildVoidCall(env, op):
            bu.subTree mnkName:
              bu.subTree MirNode(kind: mnkTag, effect: ekMutate):
                bu.emitFrom(tree, dest)
            bu.subTree mnkArg:
              bu.emitFrom(tree, src)
      else:
        # no sink hook exists, rewrite ``x.y = move a.b`` into:
        #   bind_mut _1 = x.y
        #   =destroy(name _1)
        #   _1 = move a.b
        var loc: Value
        changes.insert(tree, stmt, dest, bu):
          loc = bu.bindMut(tree, dest)
          genDestroy(bu, graph, env, loc)
        changes.replaceMulti(tree, dest, bu):
          bu.use loc

    of mnkDestroy:
      let destr = getOp(graph, tree[tree.operand(i)].typ, attachedDestructor)
      changes.replaceMulti(tree, i, bu):
        bu.buildVoidCall(env, destr):
          # XXX: the by-name passing and usage of ``ekMutate`` is not really
          #      correct. For all intents and purposes, a destructor
          #      *consumes* the value (and then effectively voids it), meaning
          #      that ``mnkConsume`` should actually be used. However, this
          #      would require changing the signature of ``=destroy`` to use
          #      ``sink``
          bu.emitByName ekMutate:
            bu.emitFrom(tree, tree.child(i, 0))

    else:
      discard "nothing to do"

  # turn the collected diagnostics into reports and report them:
  reportDiagnostics(graph, body, owner, diags)

proc injectHooks*(body: var MirBody, graph: ModuleGraph, env: var MirEnv,
                  owner: PSym) =
  ## Adapter for the legacy pass-application pipeline. Once possible, the pass
  ## needs to be treated as just another MIR pass.
  var c = initChangeset(body.code)
  injectHooks(body, graph, env, owner, c)
  apply(body.code, prepare(c))
