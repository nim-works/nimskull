discard """
  description: "Tests {.explain.} attached to concept types, to expressions, and diagnostics reporting on failed overload resolution"
  knownIssue: "Requires Report/SemReport removal, see: https://github.com/nim-works/nimskull/issues/443"
  nimout: '''
texplain.nim(144, 10) Hint: Non-matching candidates for e(y)
proc e(i: int): int
  first type mismatch at position: 1
  required type for i: int
  but expression 'y' is of type: MatchingType
 [rsemNonMatchingCandidates]
texplain.nim(147, 7) Hint: Non-matching candidates for e(10)
proc e(o: ExplainedConcept): int
  first type mismatch at position: 1
  required type for o: ExplainedConcept
  but expression '10' is of type: int literal(10)
texplain.nim(110, 6) ExplainedConcept: undeclared field: 'foo'
texplain.nim(110, 11) ExplainedConcept: concept predicate failed
texplain.nim(111, 6) ExplainedConcept: undeclared field: 'bar'
texplain.nim(111, 11) ExplainedConcept: concept predicate failed
 [rsemNonMatchingCandidates]
texplain.nim(150, 10) Hint: Non-matching candidates for e(10)
proc e(o: ExplainedConcept): int
  first type mismatch at position: 1
  required type for o: ExplainedConcept
  but expression '10' is of type: int literal(10)
texplain.nim(110, 6) ExplainedConcept: undeclared field: 'foo'
texplain.nim(110, 11) ExplainedConcept: concept predicate failed
texplain.nim(111, 6) ExplainedConcept: undeclared field: 'bar'
texplain.nim(111, 11) ExplainedConcept: concept predicate failed
 [rsemNonMatchingCandidates]
texplain.nim(154, 20) Error: type mismatch: got <NonMatchingType>
but expected one of:
proc e(i: int): int
  first type mismatch at position: 1
  required type for i: int
  but expression 'n' is of type: NonMatchingType
proc e(o: ExplainedConcept): int
  first type mismatch at position: 1
  required type for o: ExplainedConcept
  but expression 'n' is of type: NonMatchingType
texplain.nim(154, 9) template/generic instantiation of `assert` from here
texplain.nim(111, 11) ExplainedConcept: concept predicate failed

expression: e(n)
texplain.nim(155, 20) Error: type mismatch: got <NonMatchingType>
but expected one of:
proc r(i: string): int
  first type mismatch at position: 1
  required type for i: string
  but expression 'n' is of type: NonMatchingType
proc r(o: RegularConcept): int
  first type mismatch at position: 1
  required type for o: RegularConcept
  but expression 'n' is of type: NonMatchingType
texplain.nim(155, 9) template/generic instantiation of `assert` from here
texplain.nim(115, 11) RegularConcept: concept predicate failed
proc r[T](a: SomeNumber; b: T; c: auto)
  first type mismatch at position: 1
  required type for a: SomeNumber
  but expression 'n' is of type: NonMatchingType

expression: r(n)
texplain.nim(156, 20) Hint: Non-matching candidates for r(y)
proc r(i: string): int
  first type mismatch at position: 1
  required type for i: string
  but expression 'y' is of type: MatchingType
proc r[T](a: SomeNumber; b: T; c: auto)
  first type mismatch at position: 1
  required type for a: SomeNumber
  but expression 'y' is of type: MatchingType
 [rsemNonMatchingCandidates]
texplain.nim(164, 2) Error: type mismatch: got <MatchingType>
but expected one of:
proc f(o: NestedConcept)
  first type mismatch at position: 1
  required type for o: NestedConcept
  but expression 'y' is of type: MatchingType
texplain.nim(114, 6) RegularConcept: undeclared field: 'foo'
texplain.nim(114, 11) RegularConcept: concept predicate failed
texplain.nim(115, 6) RegularConcept: undeclared field: 'bar'
texplain.nim(115, 11) RegularConcept: concept predicate failed
texplain.nim(118, 11) NestedConcept: concept predicate failed

expression: f(y)'''
  errormsg: "type mismatch: got <MatchingType>"
"""











# proc r[T](a: SomeNumber; b: T; c: auto)
# proc r(i: string): int
# proc r(o: RegularConcept): int





# line 106 HERE

type
  ExplainedConcept {.explain.} = concept o
    o.foo is int
    o.bar is string

  RegularConcept = concept o
    o.foo is int
    o.bar is string

  NestedConcept = concept o
    o.foo is RegularConcept

  NonMatchingType = object
    foo: int
    bar: int

  MatchingType = object
    foo: int
    bar: string

proc e(o: ExplainedConcept): int = 1
proc e(i: int): int = i

proc r[T](a: SomeNumber, b: T, c: auto) = discard
proc r(o: RegularConcept): int = 1
proc r(i: string): int = 1

proc f(o: NestedConcept) = discard

var n = NonMatchingType(foo: 10, bar: 20)
var y = MatchingType(foo: 10, bar: "bar")

# no diagnostic here (because the {.explain.} tagged concept *does* match)
discard e(y)

# explain that e(int) doesn't match
discard e(y) {.explain.}

# explain that e(ExplainedConcept) doesn't match
echo(e(10) {.explain.}, 20)

# explain that e(ExplainedConcept) doesn't again
discard e(10)

static:
  # provide diagnostics why the compile block failed
  assert(compiles(e(n)) {.explain.} == false)
  assert(compiles(r(n)) {.explain.} == false)
  assert(compiles(r(y)) {.explain.} == true)

  # these should not produce any output
  assert(compiles(r(10)) == false)
  assert(compiles(e(10)) == true)

# finally, provide multiple nested explanations for failed matching
# of regular concepts, even when the explain pragma is not used
f(y)
