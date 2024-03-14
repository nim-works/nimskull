discard """
  description: '''
  . From https://github.com/nim-lang/Nim/issues/7165
    Coercions with distinct types should traverse pointer modifiers transparently
  . While it's possible to "cast" the original type to its distinct
    counterpart directly, when a ref modifier is added, this coercion gets
    rejected.
  . The problem seems to be limited to generic types
  . Is it an issue though? It seems that a ref distinct T is not the same as a
    distinct ref T, which makes sense to me.
  . It has been fixed since Nim 1.0
  '''
"""
type
  Table[K, V] = object
    key: K
    val: V

  MyTable = distinct Table[string, int]
  MyTableRef = ref MyTable

proc newTable[K, V](): ref Table[K, V] = discard

proc newMyTable: MyTableRef =
  MyTableRef(newTable[string, int]()) # <--- error here

discard newMyTable()