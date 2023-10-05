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

var :aux_3
var :aux_4
var :aux_5
var splat
splat = splitFile(path)
result = (
  :aux_3 = splat.dir
  wasMoved(splat.dir)
  :aux_3,
  :aux_4 = splat.name
  wasMoved(splat.name)
  :aux_4,
  :aux_5 = splat.ext
  wasMoved(splat.ext)
  :aux_5)
=destroy(splat)
-- end of expandArc ------------------------
--expandArc: delete

var sibling
var :aux_3 = target[].parent[].left
=copy(sibling, :aux_3)
var saved
var :aux_5 = sibling[].right
=copy(saved, :aux_5)
var :aux_6 = sibling[].right
var :aux_7 = saved[].left
=copy(:aux_6, :aux_7)
var :aux_8 = sibling[].parent
=sink(:aux_8, saved)
=destroy(sibling)
-- end of expandArc ------------------------
--expandArc: p1

var lresult
lresult = @([123])
var lvalue
var lnext
var :aux_4
:aux_4 = (lresult, ";")
lvalue = :aux_4[0]
wasMoved(:aux_4[0])
lnext = :aux_4[1]
wasMoved(:aux_4[1])
result.value = move(lvalue)
=destroy(:aux_4)
=destroy_1(lnext)
=destroy_2(lvalue)
-- end of expandArc ------------------------
--expandArc: tt

var :aux_5
var :aux_6
var a
var :aux_3
try:
  var it_cursor = x
  a = (
    :aux_5 = default()
    =copy(:aux_5, it_cursor.key)
    :aux_5,
    :aux_6 = default()
    =copy(:aux_6, it_cursor.val)
    :aux_6)
  echo([
    var :aux_7 = $(a)
    :aux_3 = :aux_7
    :aux_3])
finally:
  =destroy(:aux_3)
  =destroy_1(a)
-- end of expandArc ------------------------
--expandArc: extractConfig

var lan_ip
try:
  lan_ip = ""
  var a_cursor = txt
  var i = 0
  var L = len(a_cursor)
  block :label_0:
    while true:
      if not(<(i, L)):
        break :label_0
      var splitted
      try:
        var line = a_cursor[i]
        splitted = split(line, " ", -1)
        if ==(splitted[0], "opt"):
          var :aux_7 = splitted[1]
          =copy(lan_ip, :aux_7)
        echo([lan_ip])
        echo([splitted[1]])
      finally:
        =destroy(splitted)
      inc(i, 1)
finally:
  =destroy_1(lan_ip)
--expandArc: mergeShadowScope

var shadowScope
try:
  var :aux_3 = c[].currentScope
  =copy(shadowScope, :aux_3)
  rawCloseScope(c)
  var a_cursor = shadowScope[].symbols
  var i = 0
  var L = len(a_cursor)
  block :label_0:
    while true:
      if not(<(i, L)):
        break :label_0
      var :aux_9
      var sym = a_cursor[i]
      addInterfaceDecl(c,
        var :aux_8 = sym
        :aux_9 = default()
        =copy_1(:aux_9, :aux_8)
        :aux_9)
      inc(i, 1)
finally:
  =destroy(shadowScope)
-- end of expandArc ------------------------
--expandArc: check

var par
try:
  this[].isValid = fileExists(this[].value)
  block :label_0:
    if dirExists(this[].value):
      var :aux_4
      par = [type node]((
        var :aux_3 = this[].value
        :aux_4 = default()
        =copy(:aux_4, :aux_3)
        :aux_4, ""))
      break :label_0
    var :aux_6
    var :aux_7
    var :aux_8
    par = [type node]((parentDir(this[].value),
      :aux_7 = splitPath(
        var :aux_5 = this[].value
        :aux_6 = default()
        =copy(:aux_6, :aux_5)
        :aux_6)
      :aux_8 = :aux_7.tail
      wasMoved(:aux_7.tail)
      :aux_8))
    =destroy(:aux_7)
  block :label_0:
    if dirExists(par.dir):
      var :aux_9 = this[].matchDirs
      var :aux_10 = getSubDirs(par.dir, par.front)
      =sink(:aux_9, :aux_10)
      break :label_0
    var :aux_11 = this[].matchDirs
    var :aux_12 = []
    =sink(:aux_11, :aux_12)
finally:
  =destroy(par)
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
