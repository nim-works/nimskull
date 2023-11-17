discard """
  output: '''(repo: "", package: "meo", ext: "")
doing shady stuff...
3
6
(@[1], @[2])
192.168.0.1
192.168.0.1
192.168.0.1
192.168.0.1'''
  cmd: '''nim c --gc:arc --expandArc:newTarget --expandArc:delete --expandArc:p1 --expandArc:tt --hint:Performance:off --assertions:off --expandArc:extractConfig --expandArc:mergeShadowScope --expandArc:check $file'''
  nimout: '''--expandArc: newTarget

scope:
  def splat: tuple[dir: string, name: string, ext: string] = splitFile(arg path)
  bind_mut _3: string = splat.0
  def _0: string = _3
  wasMoved(name _3)
  bind_mut _4: string = splat.1
  def _1: string = _4
  wasMoved(name _4)
  bind_mut _5: string = splat.2
  def _2: string = _5
  wasMoved(name _5)
  result = construct (consume _0, consume _1, consume _2)
  =destroy(name splat)
-- end of expandArc ------------------------
--expandArc: delete

scope:
  def_cursor _0: Node = target[]
  def_cursor _1: Node = _0[].parent
  def sibling: Node
  def _6: Node = _1[].left
  =copy(name sibling, arg _6)
  def_cursor _2: Node = sibling
  def saved: Node
  def _7: Node = _2[].right
  =copy(name saved, arg _7)
  def_cursor _3: Node = sibling
  def_cursor _4: Node = saved
  bind_mut _8: Node = _3[].right
  def _9: Node = _4[].left
  =copy(name _8, arg _9)
  def_cursor _5: Node = sibling
  bind_mut _10: Node = _5[].parent
  =sink(name _10, arg saved)
  =destroy(name sibling)
-- end of expandArc ------------------------
--expandArc: p1

scope:
  def _0: array[0..0, int] = construct (consume 123)
  def lresult: seq[int] = @(consume _0)
  def lvalue: seq[int]
  def lnext: string
  def _1: seq[int] = lresult
  def _: (seq[int], string) = construct (consume _1, consume ";")
  bind_mut _2: seq[int] = _.0
  lvalue = _2
  wasMoved(name _2)
  bind_mut _3: string = _.1
  lnext = _3
  wasMoved(name _3)
  result.value = move(name lvalue)
  =destroy(name _)
  =destroy(name lnext)
  =destroy(name lvalue)
-- end of expandArc ------------------------
--expandArc: tt

scope:
  try:
    def_cursor it: KeyValue = x
    def _0: seq[int]
    def _4: seq[int] = it.0
    =copy(name _0, arg _4)
    def _1: seq[int]
    def _5: seq[int] = it.1
    =copy(name _1, arg _5)
    def a: (seq[int], seq[int]) = construct (consume _0, consume _1)
    def_cursor _2: (seq[int], seq[int]) = a
    def _3: string = $(arg _2)
    echo(arg type(array[0..0, string]), arg _3)
  finally:
    =destroy(name _3)
    =destroy(name a)
-- end of expandArc ------------------------
--expandArc: extractConfig

scope:
  try:
    def lan_ip: string = ""
    scope:
      def_cursor a: seq[string] = txt
      def i: int = 0
      def_cursor _0: seq[string] = a
      def L: int = len(arg _0)
      block L0:
        scope:
          while true:
            scope:
              def_cursor _1: int = i
              def_cursor _2: int = L
              def _3: bool = <(arg _1, arg _2)
              def _4: bool = not(arg _3)
              if _4:
                scope:
                  break L0
              scope:
                scope:
                  try:
                    def_cursor _5: int = i
                    def line: lent string = borrow a[_5]
                    def_cursor _6: string = line[]
                    def splitted: seq[string] = split(arg _6, arg " ", arg -1)
                    def_cursor _7: string = splitted[0]
                    def _8: bool = ==(arg _7, arg "opt")
                    if _8:
                      scope:
                        def _11: string = splitted[1]
                        =copy(name lan_ip, arg _11)
                    def_cursor _9: string = lan_ip
                    echo(arg type(array[0..0, string]), arg _9)
                    def_cursor _10: string = splitted[1]
                    echo(arg type(array[0..0, string]), arg _10)
                  finally:
                    =destroy(name splitted)
                inc(name i, arg 1)
  finally:
    =destroy(name lan_ip)
--expandArc: mergeShadowScope

scope:
  try:
    def shadowScope: Scope
    def _8: Scope = c[].currentScope
    =copy(name shadowScope, arg _8)
    rawCloseScope(arg c)
    scope:
      def_cursor _0: Scope = shadowScope
      def_cursor a: seq[Symbol] = _0[].symbols
      def i: int = 0
      def_cursor _1: seq[Symbol] = a
      def L: int = len(arg _1)
      block L0:
        scope:
          while true:
            scope:
              def_cursor _2: int = i
              def_cursor _3: int = L
              def _4: bool = <(arg _2, arg _3)
              def _5: bool = not(arg _4)
              if _5:
                scope:
                  break L0
              scope:
                scope:
                  def_cursor _6: int = i
                  def sym: lent Symbol = borrow a[_6]
                  def _7: Symbol
                  def _9: Symbol = sym[]
                  =copy(name _7, arg _9)
                  addInterfaceDecl(arg c, consume _7)
                inc(name i, arg 1)
  finally:
    =destroy(name shadowScope)
-- end of expandArc ------------------------
--expandArc: check

scope:
  try:
    def_cursor _0: string = this[].value
    this[].isValid = fileExists(arg _0)
    def _1: tuple[dir: string, front: string]
    block L0:
      def_cursor _2: string = this[].value
      def _3: bool = dirExists(arg _2)
      if _3:
        scope:
          def _4: string
          def _14: string = this[].value
          =copy(name _4, arg _14)
          _1 := construct (consume _4, consume "")
          break L0
      scope:
        try:
          def_cursor _5: string = this[].value
          def _6: string = parentDir(arg _5)
          def _7: string
          def _15: string = this[].value
          =copy(name _7, arg _15)
          def _8: tuple[head: string, tail: string] = splitPath(consume _7)
          bind_mut _16: string = _8.1
          def _9: string = _16
          wasMoved(name _16)
          _1 := construct (consume _6, consume _9)
          wasMoved(name _6)
        finally:
          =destroy(name _8)
          =destroy(name _6)
    def par: tuple[dir: string, front: string] = _1
    block L1:
      def_cursor _10: string = par.0
      def _11: bool = dirExists(arg _10)
      if _11:
        scope:
          def_cursor _12: string = par.0
          def_cursor _13: string = par.1
          def _17: seq[string] = getSubDirs(arg _12, arg _13)
          bind_mut _18: seq[string] = this[].matchDirs
          =sink(name _18, arg _17)
          break L1
      scope:
        def _19: seq[string] = []
        bind_mut _20: seq[string] = this[].matchDirs
        =sink(name _20, arg _19)
  finally:
    =destroy(name par)
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
