## This module implements a simple MIR-path-based alias analysis. Path-based
## means that the analysis is *local* and thus that pointer indirections are
## not followed.
##
## MIR aliases are followed, however. So that the comparison can be
## implemented efficiently, the path expression is first compiled into a
## ``Path`` object via `computePath <#computePath,MirTree,NodePosition>`_.
##
## Two compiled ``Path`` objects can then be passed to
## `comparePaths <#comparePaths,MirTree,Path,Path>`_. Since there are
## oftentimes multiple facts one wants to know about when comparing two paths,
## ``comparePaths`` only gathers the necessary information for deriving all
## the facts.
##
## Whether two lvalues refer to overlapping locations is not always statically
## known (e.g. when accessing an array with a value only known at run-time) --
## if it is not, the uncertainity is communicated via the ``maybe`` result.

import
  compiler/ast/[
    ast
  ],
  compiler/mir/[
    mirtrees
  ],
  compiler/utils/[
    idioms
  ]

type
  Ternary* = enum
    no, maybe, yes

  CmpLocsResult* = object
    endA, endB: bool ## whether the `last` operation of the provided sequences
                     ## was reached
    overlaps: Ternary

  PathInstrKind = enum
    pikNamed
    pikPos
    pikIndex

  PathInstr = object
    kind: PathInstrKind
    node: NodePosition
      ## the node describing the operand

  Path* = object
    ## Acceleration structure for representing a path expression. This allows
    ## for more efficient comparison of paths.
    root*: NodePosition
      ## the root node of the path.

    short: array[6, PathInstr]
      ## short-sequence-optimization: no dynamic sequence is allocated for
      ## short paths (which many of them are).
    shortLen: int
      ## the number of items in `short`. Zero if the dynamic sequence is used.
    long: seq[PathInstr]

const
  Roots = SymbolLike + {mnkTemp, mnkCall, mnkMagic, mnkDeref, mnkDerefView}
  PathOps = {mnkPathPos, mnkPathNamed, mnkPathArray, mnkPathConv,
             mnkPathVariant}

func isSameRoot(an, bn: MirNode): bool =
  if an.kind != bn.kind:
    return false

  case an.kind
  of SymbolLike:
    result = an.sym.id == bn.sym.id
  of mnkTemp:
    result = an.temp == bn.temp
  of Roots - SymbolLike - {mnkTemp}:
    result = false
  else:
    unreachable(an.kind)

func sameIndex*(a, b: MirNode): Ternary =
  if a.kind != b.kind or a.kind != mnkLiteral:
    maybe
  else:
    if a.lit.intVal == b.lit.intVal:
      yes
    else:
      no

func len(p: Path): int =
  if p.shortLen > 0: p.shortLen
  else:              p.long.len

func `[]`(p: Path, i: int): PathInstr =
  # note: the instructions were added in reverse, which we account for
  # here
  if p.shortLen > 0: p.short[p.shortLen - 1 - i]
  else:              p.long[^i]

proc getRoot*(tree: MirTree, n: OpValue): OpValue =
  ## If `n` points doesn't point to a path expression, returns `n`, the root
  ## of the path otherwise. Aliases are followed.
  var pos = n
  while tree[pos].kind in PathOps:
    pos = tree.operand(NodePosition pos, 0)

  if tree[pos].kind == mnkAlias:
    result = getRoot(tree, tree.operand(findDef(tree, NodePosition pos), 1))
  else:
    result = pos

func isCursor*(tree: MirTree, path: Path): bool =
  ## Returns whether the path `n` denotes a cursor location.
  # XXX: this is an intermediate solution. ``mirgen`` is going to handle
  #      all cursor-related behaviour in the future, which will make this
  #      procedure obsolete
  for i in 0..<path.len:
    if path[i].kind == pikNamed and sfCursor in tree[path[i].node].field.flags:
      result = true
      break

proc computePath*(tree: MirTree, at: NodePosition): Path =
  ## Computes the ``Path`` for the given expression. The expression not being
  ## a path expression is allowed too.
  ##
  ## Locals view are followed as far as possible. Given:
  ##   bind _1 = x.a
  ##   y = _1.b
  ## the collected path will be ``x.a.b``.
  result = Path()

  var
    isShort = true
    i = 0

  template add(k: PathInstrKind, p: NodePosition) =
    let instr = PathInstr(kind: k, node: p)
    if isShort:
      if i == len(result.short):
        # the short buffer would overflow
        result.long = @(result.short)
        result.long.add instr
        isShort = false
      else:
        result.short[i] = instr
        inc i
    else:
      result.long.add instr

  var pos = at
  while true:
    case tree[pos].kind
    of mnkPathNamed, mnkPathVariant:
      add pikNamed, pos
    of mnkPathConv:
      discard "ignore"
    of mnkPathPos:
      add pikPos, pos
    of mnkPathArray:
      add pikIndex, NodePosition tree.operand(pos, 1)
    of mnkAlias:
      # skip to the path that the alias is created of
      pos = findDef(tree, pos)
      pos = NodePosition tree.operand(pos, 1)
      # continue collecting the path instructions
      continue
    of AllNodeKinds - PathOps - {mnkAlias}:
      # end of path
      break

    inc pos

  result.root = pos
  result.shortLen = if isShort: i else: 0

proc compare*(body: MirTree, a, b: Path): CmpLocsResult =
  ## Compares the paths `a` and `b` and returns the information to later
  ## derive the facts from (e.g.: is A a part B).
  if not isSameRoot(body[a.root], body[b.root]):
    # no need to compare the paths if they don't share the same root
    return

  # now compare both paths:
  var
    overlaps = yes # until proven otherwise
    i = 0

  while i < min(a.len, b.len):
    let
      instrA = a[i]
      instrB = b[i]

    if instrA.kind != instrB.kind:
      overlaps = no
      break

    let
      na = body[instrA.node]
      nb = body[instrB.node]

    case instrA.kind
    of pikNamed:
      if na.field != nb.field:
        overlaps = no
        break

    of pikPos:
      if na.position != nb.position:
        overlaps = no
        break

    of pikIndex:
      overlaps = sameIndex(na, nb)
      if overlaps == no:
        break

    inc i

  result = CmpLocsResult(endA: i == a.len, endB: i == b.len,
                         overlaps: overlaps)

func isAPartOfB*(r: CmpLocsResult): Ternary {.inline.} =
  result = r.overlaps
  if result != no and not r.endB and r.endA:
    # B either is or maybe is a part of A, but A is not a part of B
    result = no

func isBPartOfA*(r: CmpLocsResult): Ternary {.inline.} =
  result = r.overlaps
  if result != no and not r.endA and r.endB:
    result = no

func isSame*(r: CmpLocsResult): bool {.inline.} =
  ## Returns whether A and B refer to the exact same location
  ## (ignoring the type)
  result = r.overlaps == yes and r.endA and r.endB

template overlaps*(r: CmpLocsResult): Ternary =
  r.overlaps

func overlaps*(tree: MirTree, a, b: Path): Ternary {.inline.} =
  ## Convenience wrapper
  compare(tree, a, b).overlaps

func isPartOf*(tree: MirTree, a, b: Path): Ternary {.inline.} =
  ## Computes if the location named by `a` is part of `b`. Also evaluates to
  ## 'yes' if both name the exact same location
  let cmp =  compare(tree, a, b)
  result = isAPartOfB(cmp)