discard """
  description: '''
    Tests for the control-flow graph creation as well as the traversal
    algorithms implemented by the ``mirexec.nim``
  '''
  target: native
"""

import std/[strutils, strscans, macros]

include compiler/sem/mirexec

## To make the tests easier to understand and write, we use mini programs
## where the control-flow instruction are interleaved with 'def'/'use'
## instructions that we later use to comprehend which paths were traversed and
## in what order

type
  POpcode = enum
    def, use,
    cflow ## some control-flow related instruction

  PInstr = object
    op: POpcode
    id: int

  Program = object
    cfg: ControlFlowGraph
    code: seq[PInstr]
    map: Table[int, NodePosition]
      ## maps a def/use ID the position of the corresponding instruction

func parseOp(input: string, r: var Opcode, start: int): int =
  for e, s in [opFork: "fork", opGoto: "goto", opLoop: "loop"]:
    if input.substr(start).startsWith(s):
      r = e
      result = s.len
      break

proc parseCfg(str: string): ControlFlowGraph =
  ## Creat a CFG object from its text representation
  var
    list: seq[Instr]
    map: seq[InstrPos]
    nameToId: Table[int, uint32]

  template getId(name: int): JoinId =
    nameToId.mgetOrPut(name, nameToId.len.JoinId)

  for line in splitLines(str):
    if scanf(line, "$s$."):
      # skip empty lines
      continue

    var
      name: int
      op: Opcode

    if scanf(line, "$s$i$s:$sjoin", name):
      let id = getId(name)
      list.add Instr(op: opJoin, id: id)
      map.setLen(id+1)
      map[id] = InstrPos(list.high)
    elif scanf(line, "$s${parseOp} $i", op, name):
      list.add Instr(op: op)
      list[^1].dest = getId(name)
    else:
      raise ValueError.newException("syntax error in line: " & line)

    let pos = find(line, "-")
    var val: int
    if pos != -1 and not scanf(line.substr(pos), "->$s$i", val):
      raise ValueError.newException("syntax error at end: " & line)

    list[^1].node = NodePosition(val)

  result = ControlFlowGraph(instructions: list, map: map)

proc parse2(str: string): Program =
  ## Parses a ``Program`` object from its text representation
  var
    nameToId: Table[int, uint32]

  func parseOp2(input: string, r: var POpcode, start: int): int =
    for e, s in [def: "def", use: "use"]:
      if input.substr(start).startsWith(s):
        r = e
        result = s.len
        break

  template getId(name: int): JoinId =
    nameToId.mgetOrPut(name, nameToId.len.JoinId)

  template cfgCode: untyped = result.cfg.instructions
  template map: untyped = result.cfg.map
  template code: untyped = result.code

  for line in splitLines(str):
    if scanf(line, "$s$."):
      # skip empty lines
      continue

    var
      name: int
      op: Opcode
      op2: POpcode

    if scanf(line, "$s$i$s:$sjoin", name):
      code.add PInstr(op: cflow)

      let id = getId(name)
      cfgCode.add Instr(op: opJoin, id: id)

      if int(id + 1) > map.len:
        map.setLen(id+1)
      map[id] = InstrPos(cfgCode.high)
    elif scanf(line, "$s${parseOp} $i", op, name):
      code.add PInstr(op: cflow)
      cfgCode.add Instr(op: op)
      cfgCode[^1].dest = getId(name)
    elif scanf(line, "$s${parseOp2} :$i", op2, name):
      code.add PInstr(op: op2, id: name)
      result.map[name] = NodePosition(code.high)
      continue
    else:
      raise ValueError.newException("syntax error in line: " & line)

    cfgCode[^1].node = NodePosition(code.high)

func `==`(a, b: Instr): bool =
  if a.op != b.op or a.node != b.node:
    return false

  result =
    case a.op
    of opFork, opGoto, opLoop: a.dest == b.dest
    of opJoin:                 a.id == b.id

func `==`(a, b: ControlFlowGraph): bool =
  ## Compares two CFGs for structural equality. Differing join IDs are ignored
  ## as long as they point to the same instruction
  if a.instructions.len != b.instructions.len:
    return false

  for i in 0..<a.instructions.len:
    let
      an = a[i]
      bn = b[i]

    if an.op != bn.op or an.node != bn.node:
      return false

    result =
      case an.op
      of opFork, opGoto, opLoop:
        a.map[an.dest] == b.map[bn.dest]
      of opJoin:
        a.map[an.id] == b.map[bn.id]

    if not result:
      return


# -------------- CFG creation tests

block:
  # test CFG creation for ``while true: break``
  let tree = @[
    MirNode(kind: mnkStmtList),
    MirNode(kind: mnkRepeat),
    MirNode(kind: mnkBreak, label: NoLabel),
    MirNode(kind: mnkEnd),
    MirNode(kind: mnkReturn),
    MirNode(kind: mnkEnd)]
  let cfg = computeCfg(tree)

  doAssert cfg == parseCfg("""
    0: join -> 1
    goto 1  -> 2
    loop 0  -> 3
    1: join -> 3
    goto 2  -> 4
    2: join -> 6
  """)

# -------------- test for the traversal routines

# TODO: also test forward traversal and backward traversal from all exits

func isConnected(p: Program, defId, useId: int): bool =
  ## Computes and returns whether the 'use' with id `useId` is connected to
  ## the 'def' with id `defId`. Also validates that each instruction is really
  ## only visited once.
  var
    tree = newSeq[MirNode](p.code.len)
    visited = newSeq[bool](p.code.len)
    exit = false

  result = false
  for i, _ in traverseReverse(tree, p.cfg, NodePosition(0)..NodePosition(tree.high), p.map[useId], exit):
    doAssert not visited[int i]
    visited[int i] = true

    case p.code[int i].op
    of def:
      if p.code[int i].id == defId:
        result = true
    of use, cflow:
      discard "ignore"

proc useChain(p: Program, defId, start: int): seq[int] =
  ## Computes and returns the 'use's connected to the 'use' with ID `start`.
  ## Reaching `defId` breaks the chain. The list is sorted in post-order.
  ## Also validates that each instruction is really only visited once.
  # TODO: remove `tree` once traversal is separated from the MIR
  var
    tree = newSeq[MirNode](p.code.len)
    visited = newSeq[bool](p.code.len)
    exit = false

  for i, _ in traverseReverse(tree, p.cfg, NodePosition(0)..NodePosition(tree.high), p.map[start], exit):
    doAssert not visited[int i],
             "instruction already visited; either the algorithm or CFG is broken"
    visited[int i] = true

    let instr = p.code[int i]
    case instr.op
    of def:
      if instr.id == defId:
        # found the def; quit the path
        exit = true
    of use:
      result.add instr.id
    of cflow:
      discard "ignore"

block infinite_loop:
  # while true:
  #   discard
  let p = parse2("""
    def :1
    0: join
    use :2
    use :3
    loop 0
    use :4
  """)

  doAssert(isConnected(p, 1, 2))
  doAssert(not isConnected(p, 1, 4))
  doAssert useChain(p, 1, 3) == [2, 3]
  doAssert useChain(p, 1, 4) == []

block nested_loop:
  # while cond1:
  #   while cond2:
  #     discard
  let p = parse2("""
    0: join
      def :1
      1: join
        use :2
        fork 2
          use :3
          goto 3
        2: join
        use :4
        loop 1
      3: join
      use :5
      loop 0
  """)

  doAssert useChain(p, 1, start = 5) == [3, 2, 4]

block two_nested_loops:
  # two nested loops inside a single loop:
  #
  # while true:
  #   while cond2:
  #     discard
  #   while cond3:
  #     discard

  let p = parse2("""
    def :1
    0: join
      1: join
        use :2
        fork 2
          use :3
          goto 3
        2: join
        use :4
        loop 1
      3: join
      use :5
      4: join
        use :6
        fork 5
          use :7
          goto 6
        5: join
        use :8
        loop 4
      6: join
      use :9
      loop 0
  """)

  doAssert useChain(p, 1, start = 3) == [2, 4, 9, 7, 6, 8, 5, 3]
  doAssert useChain(p, 1, start = 4) == [2, 4, 9, 7, 6, 8, 5, 3]
  doAssert useChain(p, 1, start = 5) == [3, 2, 4, 9, 7, 6, 8, 5]

block nested_loop_exit:
  # block exit:
  #   while cond1:
  #     while true:
  #       break exit
  let p = parse2("""
    def :1
    0: join
      use :4
      fork 1
        use :2
        goto 3
      1: join
      2: join
        use :3
        goto 3
        loop 2
      loop 0
    3: join
    use :5
  """)

  doAssert useChain(p, 1, 2) == [4]
  doAssert useChain(p, 1, 3) == [4]
  doAssert useChain(p, 1, 4) == []
  doAssert useChain(p, 1, 5) == [3, 2, 4]

block nested_infinite_loop:
  # while cond:
  #   while true:
  #     discard

  let p = parse2("""
    def :1
    0: join
      use :2
      fork 1
        use :3
        goto 2
      1: join
        use :4
        loop 1
      loop 0
    2: join
    use :5
  """)

  doAssert useChain(p, 1, 2) == []
  doAssert useChain(p, 1, 3) == [2]
  doAssert useChain(p, 1, 4) == [4, 2]
  doAssert useChain(p, 1, 5) == [3, 2]

block nested_if_in_loop:
  # while true:
  #   if cond:
  #     if cond2:
  #       break
  let p = parse2("""
    def :1
    0: join
      use :2
      fork 1
        use :3
        fork 2
          use :4
          goto 3
        2: join
      1: join
      use :5
      loop 0
    3: join
    use :6
  """)

  doAssert useChain(p, 1, 2) == [5, 3, 2]
  doAssert useChain(p, 1, 3) == [2, 5, 3]
  doAssert useChain(p, 1, 4) == [3, 2, 5]
  doAssert useChain(p, 1, 5) == [3, 2, 5]
  doAssert useChain(p, 1, 6) == [4, 3, 2, 5]

block try_in_loop:
  # while true:
  #   try:
  #     if a:
  #       continue
  #     if b:
  #       break
  #   finally:
  #     discard
  let p = parse2("""
    def :1
    0: join
      use :2
      fork 1
        use :3
        goto 3 # continue
      1: join

      fork 2
        use :4
        goto 3
      2: join

      goto 5
      3: join
      use :5
      fork 5
      goto 6

      5: join
      use :6
      loop 0
    6: join
    use :7
  """)

  doAssert useChain(p, 1, 2) == [6, 5, 4, 3, 2]
  doAssert useChain(p, 1, 3) == [2, 6, 5, 4, 3]
  doAssert useChain(p, 1, 4) == [2, 6, 5, 4, 3]
  doAssert useChain(p, 1, 5) == [4, 3, 2, 6, 5]
  doAssert useChain(p, 1, 6) == [5, 4, 3, 2, 6]
  doAssert useChain(p, 1, 7) == [5, 4, 3, 2, 6]
