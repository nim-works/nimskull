discard """
  output: "ok"
  matrix: "--deepcopy:on"
"""

import tables, lists

type
  ListTable[K, V] = object
    valList: DoublyLinkedList[V]
    table: Table[K, DoublyLinkedNode[V]]

  ListTableRef*[K, V] = ref ListTable[K, V]

proc initListTable*[K, V](initialSize = 64): ListTable[K, V] =
  result.valList = initDoublyLinkedList[V]()
  result.table = initTable[K, DoublyLinkedNode[V]]()

proc newListTable*[K, V](initialSize = 64): ListTableRef[K, V] =
  new(result)
  result[] = initListTable[K, V](initialSize)

proc `[]=`*[K, V](t: var ListTable[K, V], key: K, val: V) =
  if key in t.table:
    t.table[key].value = val
  else:
    let node = newDoublyLinkedNode(val)
    t.valList.append(node)
    t.table[key] = node

proc `[]`*[K, V](t: ListTable[K, V], key: K): var V {.inline.} =
  result = t.table[key].value

proc len*[K, V](t: ListTable[K, V]): Natural {.inline.} =
  result = t.table.len

iterator values*[K, V](t: ListTable[K, V]): V =
  for val in t.valList.items():
    yield val

proc `[]=`*[K, V](t: ListTableRef[K, V], key: K, val: V) =
  t[][key] = val

proc `[]`*[K, V](t: ListTableRef[K, V], key: K): var V {.inline.} =
  t[][key]

proc len*[K, V](t: ListTableRef[K, V]): Natural {.inline.} =
  t[].len

iterator values*[K, V](t: ListTableRef[K, V]): V =
  for val in t[].values:
    yield val

proc main() =
  type SomeObj = ref object

  for outer in 0..10_000:
    let myObj = new(SomeObj)
    let table = newListTable[int, SomeObj]()

    table[0] = myObj
    for i in 1..100:
      table[i] = new(SomeObj)

    var myObj2: SomeObj
    for val in table.values():
      if myObj2.isNil:
        myObj2 = val
    doAssert(myObj == myObj2) # passes

    var tableCopy: ListTableRef[int, SomeObj]
    deepCopy(tableCopy, table)

    let myObjCopy = tableCopy[0]
    var myObjCopy2: SomeObj = nil
    for val in tableCopy.values():
      if myObjCopy2.isNil:
        myObjCopy2 = val

    #echo cast[int](myObj)
    #echo cast[int](myObjCopy)
    #echo cast[int](myObjCopy2)

    doAssert(myObjCopy == myObjCopy2) # fails


type
  PtrTable = object
    counter, max: int
    data: array[0..99, (pointer, pointer)]

doAssert(sizeof(PtrTable) == 2*sizeof(int)+sizeof(pointer)*2*100)

main()
echo "ok"

block generic_deep_copy_using_instantiated_for_type:
  # when a generic ``=deepCopy`` implementation used the type instance it
  # is instatatied for in its body, an "unresolved generic parameter"
  # error would occur, when the generic type also had other generic type-bound
  # operators attached
  type Generic[T] = object
    x: T

  var numDestroy {.global.} = 0

  proc `=destroy`[T](x: var Generic[T]) =
    inc numDestroy

  proc `=deepCopy`[T](x: ref Generic[T]): ref Generic[T] =
    var v = Generic[T]() # <- forces the generic `=destroy` operator to be
                         #    instantiated
    result = x
    # `v` is destroyed here, incrementing `numDestroy` by one

  let v = new(Generic[int]) # <- the deep-copy operator is instantiated here

  # make sure the operator really works:
  let other = deepCopy(v)
  doAssert v == other
  doAssert numDestroy == 1