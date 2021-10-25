type
  Foo = distinct seq[int]
  Bar[N: static[int]] = distinct seq[int]
  Baz = distinct Bar[10]

proc newSeq(s: var Foo, n: Natural) {.borrow.} # Tests `var Distinct` -> Base
proc newSeq(s: var Bar, n: Natural) {.borrow.} # Tests `var Typeclass` -> Distinct -> Base
proc newSeq(s: var Baz, n: Natural) {.borrow.} # Tests `var Distinct` -> GenericDistinct -> Distinct -> Base


proc `$`(s: Foo): string {.borrow.} # Tests Distinct -> Base
proc `$`(s: Bar): string {.borrow.} # Tests Typeclass -> Distinct -> Base
proc `$`(s: Baz): string {.borrow.} # Tests Distinct -> GenericDistinct -> Distinct -> Base

proc doThing(b: Bar) = discard
proc doThing(b: Baz) {.borrow.} # Tests Distinct -> GenericDistinct borrow

var
  foo: Foo
  bar: Bar[10]
  baz: Baz

newSeq(foo, 100)
newSeq(bar, bar.N)
newSeq(baz, 10)

bar.doThing()
baz.doThing()

assert $seq[int](foo) == $foo
assert $seq[int](bar) == $bar
assert $seq[int](baz) == $baz
