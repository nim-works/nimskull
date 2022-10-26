discard """
  errormsg: "type mismatch: got <ptr Hard[system.string]> but expected 'Book[system.string]'"
  file: "tarraycons_ptr_generic2.nim"
  line: 26
  labels: "array generic ptr ref subtyping"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/7601
      array construction failed generic object of ptr relation subtype with
    . Defensive test to make sure that the fix for
      https://github.com/nim-lang/Nim/issues/7601 doesn't break unrelated code.
    . note: ref object and ref generic is ok, only ptr generic failed

'''
"""

type
  Book[T] = ref object of RootObj
    cover: T
  Hard[T] = ref object of Book[T]
  Soft[T] = ref object of Book[T]

var bookParent = Book[string](cover: "none")
var bookHard = Hard[string](cover: "skin")
var bookSoft = Soft[string](cover: "paper")

let z = [bookParent, bookHard.addr, bookSoft]
