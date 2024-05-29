discard """
  output: '''(repo: "", package: "meo", ext: "")
doing shady stuff...
3
6
(@[1], @[2])
192.168.0.1
192.168.0.1
192.168.0.1
192.168.0.1
0'''
  cmd: '''nim c --gc:arc --expandArc:newTarget --expandArc:delete --expandArc:p1 --expandArc:tt --hint:Performance:off --assertions:off --expandArc:extractConfig --expandArc:mergeShadowScope --expandArc:check --expandArc:treturn $file'''
  nimout: '''--expandArc: newTarget

scope:
  def splat: tuple[dir: string, name: string, ext: string] = splitFile(arg path) -> [L0, Resume]
  bind_mut _7: string = splat.0
  def _3: string = move _7
  wasMoved(name _7)
  bind_mut _8: string = splat.1
  def _4: string = move _8
  wasMoved(name _8)
  bind_mut _9: string = splat.2
  def _5: string = move _9
  wasMoved(name _9)
  def _6: Target = (consume _3, consume _4, consume _5)
  result := move _6
  =destroy(name splat)
  goto [L1]
finally (L0):
  continue {}
L1:
-- end of expandArc ------------------------
--expandArc: delete

scope:
  def_cursor _3: Node = target[]
  def_cursor _4: Node = _3[].parent
  def sibling: Node
  =copy(name sibling, arg _4[].left)
  def_cursor _6: Node = sibling
  def saved: Node
  =copy(name saved, arg _6[].right)
  def_cursor _7: Node = sibling
  def_cursor _8: Node = saved
  def_cursor _10: Node = _8[].left
  =copy(name _7[].right, arg _10)
  def_cursor _9: Node = sibling
  =sink(name _9[].parent, arg saved)
  =destroy(name sibling)
-- end of expandArc ------------------------
--expandArc: p1

scope:
  def lresult: seq[int] = @[consume 123]
  def lvalue: seq[int]
  def lnext: string
  def _5: seq[int] = move lresult
  def _: (seq[int], string) = (consume _5, consume ";")
  bind_mut _7: seq[int] = _.0
  lvalue := move _7
  wasMoved(name _7)
  bind_mut _8: string = _.1
  lnext := move _8
  wasMoved(name _8)
  def _6: seq[int] = move(name lvalue)
  result.value := move _6
  =destroy(name _)
  =destroy(name lnext)
  =destroy(name lvalue)
-- end of expandArc ------------------------
--expandArc: tt

scope:
  def_cursor it: KeyValue = x
  def _4: seq[int]
  =copy(name _4, arg it.0)
  def _5: seq[int]
  =copy(name _5, arg it.1)
  def a: (seq[int], seq[int]) = (consume _4, consume _5)
  def_cursor _6: (seq[int], seq[int]) = a
  def _7: string = $(arg _6) -> [L0, Resume]
  echo(arg type(array[0..0, string]), arg _7) -> [L1, L0, Resume]
  goto [L1, L0, L2]
  finally (L1):
    =destroy(name _7)
    continue {L0}
  finally (L0):
    =destroy(name a)
    continue {L2}
  L2:
-- end of expandArc ------------------------
--expandArc: extractConfig

scope:
  def lan_ip: string = ""
  scope:
    def_cursor a: seq[string] = txt
    def i: int = 0
    def_cursor _5: seq[string] = a
    def L: int = lengthSeq(arg _5)
    scope:
      while true:
        scope:
          def_cursor _7: int = i
          def :tmp: bool = ltI(arg _7, arg L)
          scope:
            def_cursor _8: bool = :tmp
            def _9: bool = not(arg _8)
            if _9:
              scope:
                goto [L2]
          scope:
            def_cursor _11: int = i
            def line: lent string = borrow a[_11]
            def_cursor _13: string = line[]
            def splitted: seq[string] = split(arg _13, arg " ", arg -1) -> [L3, Resume]
            scope:
              def_cursor _14: string = splitted[0]
              def _15: bool = eqStr(arg _14, arg "opt")
              if _15:
                scope:
                  def_cursor _18: string = splitted[1]
                  =copy(name lan_ip, arg _18)
            def_cursor _16: string = lan_ip
            echo(arg type(array[0..0, string]), arg _16) -> [L4, L3, Resume]
            def_cursor _17: string = splitted[1]
            echo(arg type(array[0..0, string]), arg _17) -> [L4, L3, Resume]
            goto [L4, L6]
            finally (L4):
              =destroy(name splitted)
              continue {L3, L6}
            L6:
          i = addI(arg i, arg 1) -> [L3, Resume]
    L2:
  goto [L3, L7]
  finally (L3):
    =destroy(name lan_ip)
    continue {L7}
  L7:
--expandArc: mergeShadowScope

scope:
  def shadowScope: Scope
  =copy(name shadowScope, arg c[].currentScope)
  rawCloseScope(arg c) -> [L0, Resume]
  scope:
    def_cursor _4: Scope = shadowScope
    def_cursor a: seq[Symbol] = _4[].symbols
    def i: int = 0
    def_cursor _7: seq[Symbol] = a
    def L: int = lengthSeq(arg _7)
    scope:
      while true:
        scope:
          def_cursor _9: int = i
          def :tmp: bool = ltI(arg _9, arg L)
          scope:
            def_cursor _10: bool = :tmp
            def _11: bool = not(arg _10)
            if _11:
              scope:
                goto [L3]
          scope:
            def_cursor _13: int = i
            def sym: lent Symbol = borrow a[_13]
            def _14: Symbol
            =copy(name _14, arg sym[])
            addInterfaceDecl(arg c, consume _14) -> [L4, L0, Resume]
            goto [L4, L5]
            finally (L4):
              continue {L0, L5}
            L5:
          i = addI(arg i, arg 1) -> [L0, Resume]
    L3:
  goto [L0, L6]
  finally (L0):
    =destroy(name shadowScope)
    continue {L6}
  L6:
-- end of expandArc ------------------------
--expandArc: treturn

scope:
  def x: sink string
  scope:
    def_cursor _2: sink string = x
    def _3: int = lengthStr(arg _2)
    def _4: bool = eqI(arg _3, arg 2)
    if _4:
      scope:
        result := move x
        wasMoved(name x)
        goto [L1, L2]
  def_cursor _5: sink string = x
  def _6: int = lengthStr(arg _5)
  def _7: string = $(arg _6) -> [L1, L3, Resume]
  echo(arg type(array[0..0, string]), arg _7) -> [L4, L1, L3, Resume]
  goto [L4, L1, L5]
  finally (L4):
    =destroy(name _7)
    continue {L1}
  finally (L1):
    =destroy(name x)
    continue {L2, L3, L5}
  L5:
goto [L2]
finally (L3):
  =destroy(name result)
  continue {}
L2:

-- end of expandArc ------------------------
--expandArc: check

scope:
  def_cursor _2: string = this[].value
  this[].isValid = fileExists(arg _2) -> [Resume]
  def _4: tuple[dir: string, front: string]
  scope:
    def_cursor _5: string = this[].value
    def _6: bool = dirExists(arg _5) -> [Resume]
    if _6:
      scope:
        def _7: string
        =copy(name _7, arg this[].value)
        _4 := (consume _7, consume "")
        goto [L1]
  scope:
    def_cursor _8: string = this[].value
    def _9: string = parentDir(arg _8) -> [Resume]
    def _10: string
    =copy(name _10, arg this[].value)
    def _11: tuple[head: string, tail: string] = splitPath(consume _10) -> [L2, Resume]
    bind_mut _19: string = _11.1
    def _12: string = move _19
    wasMoved(name _19)
    _4 := (consume _9, consume _12)
    wasMoved(name _9)
    =destroy(name _11)
    goto [L2, L3]
    finally (L2):
      =destroy(name _9)
      continue {L3}
    L3:
  L1:
  def par: tuple[dir: string, front: string] = move _4
  scope:
    def_cursor _13: string = par.0
    def _14: bool = dirExists(arg _13) -> [L4, Resume]
    if _14:
      scope:
        def_cursor _15: string = par.0
        def_cursor _16: string = par.1
        def _17: seq[string] = getSubDirs(arg _15, arg _16) -> [L4, Resume]
        =sink(name this[].matchDirs, arg _17)
        goto [L6]
  scope:
    def _18: seq[string] = @[]
    =sink(name this[].matchDirs, arg _18)
  L6:
  goto [L4, L7]
  finally (L4):
    =destroy(name par)
    continue {L7}
  L7:

-- end of expandArc ------------------------'''
"""

import os

type Target = tuple[repo, package, ext: string]

proc newTarget*(path: string): Target =
  let splat = path.splitFile
  result = (repo: splat.dir, package: splat.name, ext: splat.ext)

echo newTarget("meo")

type
  Node = ref object
    left, right, parent: Node
    value: int

proc delete(target: var Node) =
  var sibling = target.parent.left # b3
  var saved = sibling.right # b3.right -> r4

  sibling.right = saved.left # b3.right -> r4.left = nil
  sibling.parent = saved # b3.parent -> r5 = r4

  #[after this proc:
        b 5
      /   \
    b 3     b 6
  ]#


#[before:
      r 5
    /   \
  b 3    b 6 - to delete
  /    \
empty  r 4
]#
proc main =
  var five = Node(value: 5)

  var six = Node(value: 6)
  six.parent = five
  five.right = six

  var three = Node(value: 3)
  three.parent = five
  five.left = three

  var four = Node(value: 4)
  four.parent = three
  three.right = four

  echo "doing shady stuff..."
  delete(six)
  # need both of these echos
  echo five.left.value
  echo five.right.value

# make sure that the expandArc output has the expected order by directly
# referencing ``delete`` here
let _ = topt_no_cursor.delete

main()

type
  Maybe = object
    value: seq[int]

proc p1(): Maybe =
  let lresult = @[123]
  var lvalue: seq[int]
  var lnext: string
  (lvalue, lnext) = (lresult, ";")

  result.value = move lvalue

proc tissue15130 =
  doAssert p1().value == @[123]

let _ = topt_no_cursor.p1

tissue15130()

type
  KeyValue = tuple[key, val: seq[int]]

proc tt(x: KeyValue) =
  var it = x
  let a = (it.key, it.val)
  echo a

proc encodedQuery =
  var query: seq[KeyValue]
  query.add (key: @[1], val: @[2])

  for elem in query:
    elem.tt()

let _ = topt_no_cursor.tt

encodedQuery()

# bug #15147

proc s(input: string): (string, string) =
  result = (";", "")

proc charmatch(input: string): (string, string) =
  result = ("123", input[0 .. input.high])

proc plus(input: string) =
  var
    lvalue, rvalue: string # cursors
    lnext: string # must be cursor!!!
    rnext: string # cursor
  let lresult = charmatch(input)
  (lvalue, lnext) = lresult

  let rresult = s(lnext)
  (rvalue, rnext) = rresult

plus("123;")

func substrEq(s: string, pos: int, substr: string): bool =
  var i = 0
  var length = substr.len
  while i < length and pos+i < s.len and s[pos+i] == substr[i]:
    inc i
  return i == length

template stringHasSep(s: string, index: int, sep: string): bool =
  s.substrEq(index, sep)

template splitCommon(s, sep, maxsplit, sepLen) =
  var last = 0
  var splits = maxsplit

  while last <= len(s):
    var first = last
    while last < len(s) and not stringHasSep(s, last, sep):
      inc(last)
    if splits == 0: last = len(s)
    yield substr(s, first, last-1)
    if splits == 0: break
    dec(splits)
    inc(last, sepLen)

iterator split(s: string, sep: string, maxsplit = -1): string =
  splitCommon(s, sep, maxsplit, sep.len)

template accResult(iter: untyped) =
  result = @[]
  for x in iter: add(result, x)

func split*(s: string, sep: string, maxsplit = -1): seq[string] =
  accResult(split(s, sep, maxsplit))


let txt = @["opt 192.168.0.1", "static_lease 192.168.0.1"]

# bug #17033

proc extractConfig() =
  var lan_ip = ""

  for line in txt:
    let splitted = line.split(" ")
    if splitted[0] == "opt":
      lan_ip = splitted[1] # "borrow" is conditional and inside a loop.
      # Not good enough...
      # we need a flag that live-ranges are disjoint
    echo lan_ip
    echo splitted[1] # Without this line everything works

extractConfig()


type
  Symbol = ref object
    name: string

  Scope = ref object
    parent: Scope
    symbols: seq[Symbol]

  PContext = ref object
    currentScope: Scope

proc rawCloseScope(c: PContext) =
  c.currentScope = c.currentScope.parent

proc addInterfaceDecl(c: PContext; s: Symbol) =
  c.currentScope.symbols.add s

proc mergeShadowScope*(c: PContext) =
  let shadowScope = c.currentScope
  c.rawCloseScope
  for sym in shadowScope.symbols:
    c.addInterfaceDecl(sym)

mergeShadowScope(PContext(currentScope: Scope(parent: Scope())))

proc treturn(x: sink string): string =
  if x.len == 2:
    result = x # last use of `x` -- it can be moved
    return

  # further uses don't affect whether the above can use a move
  echo x.len

discard treturn("")

type
  Foo = ref object
    isValid*: bool
    value*: string
    matchDirs*: seq[string]

proc getSubDirs(parent, front: string): seq[string] = @[]

method check(this: Foo) {.base.} =
  this.isValid = fileExists(this.value)
  let par = if dirExists(this.value): (dir: this.value, front: "")
            else: (dir: parentDir(this.value), front: splitPath(this.value).tail)
  if dirExists(par.dir):
    this.matchDirs = getSubDirs(par.dir, par.front)
  else:
    this.matchDirs = @[]

check(Foo())
