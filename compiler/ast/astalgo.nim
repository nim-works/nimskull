#
#
#           The Nim Compiler
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Algorithms for the abstract syntax tree: hash tables, lists
## and sets of nodes are supported. Efficiency is important as
## the data structures here are used in various places of the compiler.

import
  compiler/ast/[
    ast,
    idents,
    renderer
  ],
  std/[
    hashes,
    intsets,
    strutils,
  ],
  compiler/utils/[
    ropes,
  ]

proc hashNode*(p: RootRef): Hash

# --------------------------- ident tables ----------------------------------
proc idTableGet*(t: TIdTable, key: PIdObj): RootRef
proc idTableGet*(t: TIdTable, key: int): RootRef
proc idTablePut*(t: var TIdTable, key: PIdObj, val: RootRef)
proc idTableHasObjectAsKey*(t: TIdTable, key: PIdObj): bool
  # checks if `t` contains the `key` (compared by the pointer value, not only
  # `key`'s id)
proc idNodeTableGet*(t: TIdNodeTable, key: PIdObj): PNode
proc idNodeTablePut*(t: var TIdNodeTable, key: PIdObj, val: PNode)

# ---------------------------------------------------------------------------

proc lookupInRecord*(n: PNode, field: PIdent): PSym
proc mustRehash*(length, counter: int): bool
proc nextTry*(h, maxHash: Hash): Hash {.inline.}

# ------------- table[int, int] ---------------------------------------------
const
  InvalidKey* = low(int)

type
  TIIPair*{.final.} = object
    key*, val*: int

  TIIPairSeq* = seq[TIIPair]
  TIITable*{.final.} = object # table[int, int]
    counter*: int
    data*: TIIPairSeq


proc initIiTable*(x: var TIITable)
proc iiTableGet*(t: TIITable, key: int): int
proc iiTablePut*(t: var TIITable, key, val: int)

# implementation

proc skipConvCastAndClosure*(n: PNode): PNode =
  result = n
  while true:
    case result.kind
    of nkObjUpConv, nkObjDownConv, nkChckRange, nkChckRangeF, nkChckRange64,
       nkClosure:
      result = result[0]
    of nkHiddenStdConv, nkHiddenSubConv, nkConv, nkCast:
      result = result[1]
    else: break

proc sameValue*(a, b: PNode): bool =
  result = false
  case a.kind
  of nkCharLit..nkUInt64Lit:
    if b.kind in {nkCharLit..nkUInt64Lit}: result = getInt(a) == getInt(b)
  of nkFloatLit..nkFloat64Lit:
    if b.kind in {nkFloatLit..nkFloat64Lit}: result = a.floatVal == b.floatVal
  of nkStrLit..nkTripleStrLit:
    if b.kind in {nkStrLit..nkTripleStrLit}: result = a.strVal == b.strVal
  else:
    # don't raise an internal error for 'nim check':
    #InternalError(a.info, "SameValue")
    discard

proc leValue*(a, b: PNode): bool =
  # a <= b?
  result = false
  case a.kind
  of nkCharLit..nkUInt64Lit:
    if b.kind in {nkCharLit..nkUInt64Lit}: result = getInt(a) <= getInt(b)
  of nkFloatLit..nkFloat64Lit:
    if b.kind in {nkFloatLit..nkFloat64Lit}: result = a.floatVal <= b.floatVal
  of nkStrLit..nkTripleStrLit:
    if b.kind in {nkStrLit..nkTripleStrLit}: result = a.strVal <= b.strVal
  else:
    # don't raise an internal error for 'nim check':
    #InternalError(a.info, "leValue")
    discard

proc weakLeValue*(a, b: PNode): TImplication =
  if a.kind notin nkLiterals or b.kind notin nkLiterals:
    result = impUnknown
  else:
    result = if leValue(a, b): impYes else: impNo

proc lookupInRecord(n: PNode, field: PIdent): PSym =
  result = nil
  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      result = lookupInRecord(n[i], field)
      if result != nil: return
  of nkRecCase:
    if (n[0].kind != nkSym): return nil
    result = lookupInRecord(n[0], field)
    if result != nil: return
    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        result = lookupInRecord(lastSon(n[i]), field)
        if result != nil: return
      else: return nil
  of nkSym:
    if n.sym.name.id == field.id: result = n.sym
  else: return nil

proc getModule*(s: PSym): PSym =
  result = s
  assert((result.kind == skModule) or (result.owner != result))
  while result != nil and result.kind != skModule: result = result.owner

proc fromSystem*(op: PSym): bool {.inline.} = sfSystemModule in getModule(op).flags
proc getSymFromList*(list: PNode, ident: PIdent, start: int = 0): PSym =
  for i in start..<list.len:
    if list[i].kind == nkSym:
      result = list[i].sym
      if result.name.id == ident.id: return
    else: return nil
  result = nil

proc sameIgnoreBacktickGensymInfo(a, b: string): bool =
  if a[0] != b[0]: return false
  var alen = a.len - 1
  while alen > 0 and a[alen] != '`': dec(alen)
  if alen <= 0: alen = a.len

  var i = 1
  var j = 1
  while true:
    while i < alen and a[i] == '_': inc i
    while j < b.len and b[j] == '_': inc j
    var aa = if i < alen: toLowerAscii(a[i]) else: '\0'
    var bb = if j < b.len: toLowerAscii(b[j]) else: '\0'
    if aa != bb: return false

    # the characters are identical:
    if i >= alen:
      # both cursors at the end:
      if j >= b.len: return true
      # not yet at the end of 'b':
      return false
    elif j >= b.len:
      return false
    inc i
    inc j

proc getNamedParamFromList*(list: PNode, ident: PIdent): PSym =
  ## Named parameters are special because a named parameter can be
  ## gensym'ed and then they have '\`<number>' suffix that we need to
  ## ignore, see compiler / evaltempl.nim, snippet:
  ##
  ## .. code-block:: nim
  ##
  ##   result.add newIdentNode(getIdent(c.ic, x.name.s & "\`gensym" & $x.id),
  ##            if c.instLines: actual.info else: templ.info)
  for i in 1..<list.len:
    let it = list[i].sym
    if it.name.id == ident.id or
        sameIgnoreBacktickGensymInfo(it.name.s, ident.s): return it

proc hashNode(p: RootRef): Hash =
  result = hash(cast[pointer](p))

proc mustRehash(length, counter: int): bool =
  assert(length > counter)
  result = (length * 2 < counter * 3) or (length - counter < 4)

proc rspaces(x: int): Rope =
  # returns x spaces
  result = rope(spaces(x))

proc nextTry(h, maxHash: Hash): Hash =
  result = ((5 * h) + 1) and maxHash
  # For any initial h in range(maxHash), repeating that maxHash times
  # generates each int in range(maxHash) exactly once (see any text on
  # random-number generation for proof).

proc objectSetContains*(t: TObjectSet, obj: RootRef): bool =
  # returns true whether n is in t
  var h: Hash = hashNode(obj) and high(t.data) # start with real hash value
  while t.data[h] != nil:
    if t.data[h] == obj:
      return true
    h = nextTry(h, high(t.data))
  result = false

proc objectSetRawInsert(data: var TObjectSeq, obj: RootRef) =
  var h: Hash = hashNode(obj) and high(data)
  while data[h] != nil:
    assert(data[h] != obj)
    h = nextTry(h, high(data))
  assert(data[h] == nil)
  data[h] = obj

proc objectSetEnlarge(t: var TObjectSet) =
  var n: TObjectSeq
  newSeq(n, t.data.len * GrowthFactor)
  for i in 0..high(t.data):
    if t.data[i] != nil: objectSetRawInsert(n, t.data[i])
  swap(t.data, n)

proc objectSetIncl*(t: var TObjectSet, obj: RootRef) =
  if mustRehash(t.data.len, t.counter): objectSetEnlarge(t)
  objectSetRawInsert(t.data, obj)
  inc(t.counter)

proc objectSetContainsOrIncl*(t: var TObjectSet, obj: RootRef): bool =
  # returns true if obj is already in the string table:
  var h: Hash = hashNode(obj) and high(t.data)
  while true:
    var it = t.data[h]
    if it == nil: break
    if it == obj:
      return true             # found it
    h = nextTry(h, high(t.data))
  if mustRehash(t.data.len, t.counter):
    objectSetEnlarge(t)
    objectSetRawInsert(t.data, obj)
  else:
    assert(t.data[h] == nil)
    t.data[h] = obj
  inc(t.counter)
  result = false

proc strTableContains*(t: TStrTable, n: PSym): bool =
  var h: Hash = n.name.h and high(t.data) # start with real hash value
  while t.data[h] != nil:
    if (t.data[h] == n):
      return true
    h = nextTry(h, high(t.data))
  result = false

proc strTableRawInsert(data: var seq[PSym], n: PSym) =
  var h: Hash = n.name.h and high(data)
  while data[h] != nil:
    if data[h] == n:
      # allowed for 'export' feature:
      #InternalError(n.info, "StrTableRawInsert: " & n.name.s)
      return
    h = nextTry(h, high(data))
  assert(data[h] == nil)
  data[h] = n

proc symTabReplaceRaw(data: var seq[PSym], prevSym: PSym, newSym: PSym) =
  assert prevSym.name.h == newSym.name.h
  var h: Hash = prevSym.name.h and high(data)
  while data[h] != nil:
    if data[h] == prevSym:
      data[h] = newSym
      return
    h = nextTry(h, high(data))
  assert false

proc symTabReplace*(t: var TStrTable, prevSym: PSym, newSym: PSym) =
  symTabReplaceRaw(t.data, prevSym, newSym)

proc strTableEnlarge(t: var TStrTable) =
  var n: seq[PSym]
  newSeq(n, t.data.len * GrowthFactor)
  for i in 0..high(t.data):
    if t.data[i] != nil: strTableRawInsert(n, t.data[i])
  swap(t.data, n)

proc strTableAdd*(t: var TStrTable, n: PSym) =
  if mustRehash(t.data.len, t.counter): strTableEnlarge(t)
  strTableRawInsert(t.data, n)
  inc(t.counter)

proc strTableInclReportConflict*(t: var TStrTable, n: PSym;
                                 onConflictKeepOld = false): PSym =
  # if `t` has a conflicting symbol (same identifier as `n`), return it
  # otherwise return `nil`. Incl `n` to `t` unless `onConflictKeepOld = true`
  # and a conflict was found.
  assert n.name != nil
  var h: Hash = n.name.h and high(t.data)
  var replaceSlot = -1
  while true:
    var it = t.data[h]
    if it == nil: break
    # Semantic checking can happen multiple times thanks to templates
    # and overloading: (var x=@[]; x).mapIt(it).
    # So it is possible the very same sym is added multiple
    # times to the symbol table which we allow here with the 'it == n' check.
    if it.name.id == n.name.id:
      if it == n: return nil
      replaceSlot = h
    h = nextTry(h, high(t.data))
  if replaceSlot >= 0:
    result = t.data[replaceSlot] # found it
    if not onConflictKeepOld:
      t.data[replaceSlot] = n # overwrite it with newer definition!
    return result # but return the old one
  elif mustRehash(t.data.len, t.counter):
    strTableEnlarge(t)
    strTableRawInsert(t.data, n)
  else:
    assert(t.data[h] == nil)
    t.data[h] = n
  inc(t.counter)
  result = nil

proc strTableIncl*(t: var TStrTable, n: PSym;
                   onConflictKeepOld = false): bool {.discardable.} =
  result = strTableInclReportConflict(t, n, onConflictKeepOld) != nil

proc strTableGet*(t: TStrTable, name: PIdent): PSym =
  var h: Hash = name.h and high(t.data)
  while true:
    result = t.data[h]
    if result == nil: break
    if result.name.id == name.id: break
    h = nextTry(h, high(t.data))


type
  TIdentIter* = object # iterator over all syms with same identifier
    h*: Hash           # current hash
    name*: PIdent

proc nextIdentIter*(ti: var TIdentIter, tab: TStrTable): PSym =
  var h = ti.h and high(tab.data)
  var start = h
  result = tab.data[h]
  while result != nil:
    if result.name.id == ti.name.id: break
    h = nextTry(h, high(tab.data))
    if h == start:
      result = nil
      break
    result = tab.data[h]
  ti.h = nextTry(h, high(tab.data))

proc initIdentIter*(ti: var TIdentIter, tab: TStrTable, s: PIdent): PSym =
  ti.h = s.h
  ti.name = s
  if tab.counter == 0: result = nil
  else: result = nextIdentIter(ti, tab)

proc nextIdentExcluding*(ti: var TIdentIter, tab: TStrTable,
                         excluding: IntSet): PSym =
  var h: Hash = ti.h and high(tab.data)
  var start = h
  result = tab.data[h]
  while result != nil:
    if result.name.id == ti.name.id and not contains(excluding, result.id):
      break
    h = nextTry(h, high(tab.data))
    if h == start:
      result = nil
      break
    result = tab.data[h]
  ti.h = nextTry(h, high(tab.data))
  if result != nil and contains(excluding, result.id): result = nil

proc firstIdentExcluding*(ti: var TIdentIter, tab: TStrTable, s: PIdent,
                          excluding: IntSet): PSym =
  ti.h = s.h
  ti.name = s
  if tab.counter == 0: result = nil
  else: result = nextIdentExcluding(ti, tab, excluding)

type
  TTabIter* = object
    h: Hash

proc nextIter*(ti: var TTabIter, tab: TStrTable): PSym =
  # usage:
  # var
  #   i: TTabIter
  #   s: PSym
  # s = InitTabIter(i, table)
  # while s != nil:
  #   ...
  #   s = NextIter(i, table)
  #
  result = nil
  while (ti.h <= high(tab.data)):
    result = tab.data[ti.h]
    inc(ti.h)                 # ... and increment by one always
    if result != nil: break

proc initTabIter*(ti: var TTabIter, tab: TStrTable): PSym =
  ti.h = 0
  if tab.counter == 0:
    result = nil
  else:
    result = nextIter(ti, tab)

iterator items*(tab: TStrTable): PSym =
  var it: TTabIter
  var s = initTabIter(it, tab)
  while s != nil:
    yield s
    s = nextIter(it, tab)

proc hasEmptySlot(data: TIdPairSeq): bool =
  for h in 0..high(data):
    if data[h].key == nil:
      return true
  result = false

proc idTableRawGet(t: TIdTable, key: int): int =
  var h: Hash
  h = key and high(t.data)    # start with real hash value
  while t.data[h].key != nil:
    if t.data[h].key.id == key:
      return h
    h = nextTry(h, high(t.data))
  result = - 1

proc idTableHasObjectAsKey(t: TIdTable, key: PIdObj): bool =
  var index = idTableRawGet(t, key.id)
  if index >= 0: result = t.data[index].key == key
  else: result = false

proc idTableGet(t: TIdTable, key: PIdObj): RootRef =
  var index = idTableRawGet(t, key.id)
  if index >= 0: result = t.data[index].val
  else: result = nil

proc idTableGet(t: TIdTable, key: int): RootRef =
  var index = idTableRawGet(t, key)
  if index >= 0: result = t.data[index].val
  else: result = nil

iterator pairs*(t: TIdTable): tuple[key: int, value: RootRef] =
  for i in 0..high(t.data):
    if t.data[i].key != nil:
      yield (t.data[i].key.id, t.data[i].val)

proc idTableRawInsert(data: var TIdPairSeq, key: PIdObj, val: RootRef) =
  var h: Hash
  h = key.id and high(data)
  while data[h].key != nil:
    assert(data[h].key.id != key.id)
    h = nextTry(h, high(data))
  assert(data[h].key == nil)
  data[h].key = key
  data[h].val = val

proc idTablePut(t: var TIdTable, key: PIdObj, val: RootRef) =
  var
    index: int
    n: TIdPairSeq
  index = idTableRawGet(t, key.id)
  if index >= 0:
    assert(t.data[index].key != nil)
    t.data[index].val = val
  else:
    if mustRehash(t.data.len, t.counter):
      newSeq(n, t.data.len * GrowthFactor)
      for i in 0..high(t.data):
        if t.data[i].key != nil:
          idTableRawInsert(n, t.data[i].key, t.data[i].val)
      assert(hasEmptySlot(n))
      swap(t.data, n)
    idTableRawInsert(t.data, key, val)
    inc(t.counter)

iterator idTablePairs*(t: TIdTable): tuple[key: PIdObj, val: RootRef] =
  for i in 0..high(t.data):
    if not isNil(t.data[i].key): yield (t.data[i].key, t.data[i].val)

proc idNodeTableRawGet(t: TIdNodeTable, key: PIdObj): int =
  var h: Hash
  h = key.id and high(t.data) # start with real hash value
  while t.data[h].key != nil:
    if t.data[h].key.id == key.id:
      return h
    h = nextTry(h, high(t.data))
  result = - 1

proc idNodeTableGet(t: TIdNodeTable, key: PIdObj): PNode =
  var index: int
  index = idNodeTableRawGet(t, key)
  if index >= 0: result = t.data[index].val
  else: result = nil

proc idNodeTableRawInsert(data: var TIdNodePairSeq, key: PIdObj, val: PNode) =
  var h: Hash
  h = key.id and high(data)
  while data[h].key != nil:
    assert(data[h].key.id != key.id)
    h = nextTry(h, high(data))
  assert(data[h].key == nil)
  data[h].key = key
  data[h].val = val

proc idNodeTablePut(t: var TIdNodeTable, key: PIdObj, val: PNode) =
  var index = idNodeTableRawGet(t, key)
  if index >= 0:
    assert(t.data[index].key != nil)
    t.data[index].val = val
  else:
    if mustRehash(t.data.len, t.counter):
      var n: TIdNodePairSeq
      newSeq(n, t.data.len * GrowthFactor)
      for i in 0..high(t.data):
        if t.data[i].key != nil:
          idNodeTableRawInsert(n, t.data[i].key, t.data[i].val)
      swap(t.data, n)
    idNodeTableRawInsert(t.data, key, val)
    inc(t.counter)

iterator pairs*(t: TIdNodeTable): tuple[key: PIdObj, val: PNode] =
  for i in 0..high(t.data):
    if not isNil(t.data[i].key): yield (t.data[i].key, t.data[i].val)

proc initIITable(x: var TIITable) =
  x.counter = 0
  newSeq(x.data, StartSize)
  for i in 0..<StartSize: x.data[i].key = InvalidKey

proc iiTableRawGet(t: TIITable, key: int): int =
  var h: Hash
  h = key and high(t.data)    # start with real hash value
  while t.data[h].key != InvalidKey:
    if t.data[h].key == key: return h
    h = nextTry(h, high(t.data))
  result = -1

proc iiTableGet(t: TIITable, key: int): int =
  var index = iiTableRawGet(t, key)
  if index >= 0: result = t.data[index].val
  else: result = InvalidKey

proc iiTableRawInsert(data: var TIIPairSeq, key, val: int) =
  var h: Hash
  h = key and high(data)
  while data[h].key != InvalidKey:
    assert(data[h].key != key)
    h = nextTry(h, high(data))
  assert(data[h].key == InvalidKey)
  data[h].key = key
  data[h].val = val

proc iiTablePut(t: var TIITable, key, val: int) =
  var index = iiTableRawGet(t, key)
  if index >= 0:
    assert(t.data[index].key != InvalidKey)
    t.data[index].val = val
  else:
    if mustRehash(t.data.len, t.counter):
      var n: TIIPairSeq
      newSeq(n, t.data.len * GrowthFactor)
      for i in 0..high(n): n[i].key = InvalidKey
      for i in 0..high(t.data):
        if t.data[i].key != InvalidKey:
          iiTableRawInsert(n, t.data[i].key, t.data[i].val)
      swap(t.data, n)
    iiTableRawInsert(t.data, key, val)
    inc(t.counter)

proc isAddrNode*(n: PNode): bool =
  case n.kind
    of nkAddr, nkHiddenAddr: true
    of nkCallKinds:
      if n[0].kind == nkSym and n[0].sym.magic == mAddr: true
      else: false
    else: false

proc listSymbolNames*(symbols: openArray[PSym]): string =
  for sym in symbols:
    if result.len > 0:
      result.add ", "
    result.add sym.name.s

proc isDiscriminantField*(n: PNode): bool =
  if n.kind == nkCheckedFieldExpr: sfDiscriminant in n[0][1].sym.flags
  elif n.kind == nkDotExpr: sfDiscriminant in n[1].sym.flags
  else: false
