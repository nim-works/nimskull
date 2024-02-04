discard """
  targets: "c js vm"
  description: '''
    Ensure that locals are destroyed when the only exceptional control-flow
    leaving their scope is due to Defects raised by failed run-time checks
  '''
  knownIssue.vm: "defects raised due to failed run-time checks cannot be caught"
"""

type Object = object

var numDestroy = 0

proc `=destroy`(x: var Object) =
  inc numDestroy

template runTest(body: untyped) =
  var raised = false
  numDestroy = 0
  try:
    body
  except:
    raised = true

  doAssert raised, "no defect was raised?"
  doAssert numDestroy == 1, "the destructor wasn't called"


# with how destructor injection works at the time of writing, when a scope is
# only left through structured control-flow, no hidden ``try`` statement is
# injected. The tests here make sure that the unstructured control-flow arising
# from run-time checks is properly considered

block range_checks:
  proc test(x: int) =
    var obj = Object() # obj stores an alive value that needs to be destroyed
    discard range[0..1](x)

  runTest:
    test(2) # provoke a range-check failure

block index_checks:
  proc test(a: seq[int], x: int) =
    var obj = Object() # obj stores an value that needs to be destroyed
    discard a[x]

  runTest:
    test(@[], 1) # provoke an index-check failure

block bound_checks:
  proc test(a: seq[int]) =
    var obj = Object() # obj stores an value that needs to be destroyed
    discard toOpenArray(a, 1, 2)

  runTest:
    test(@[]) # provoke a bound-check failure

block field_checks:
  type WithVariant = object
    case kind: bool
    of true:  a: int
    of false: b: int

  proc test(a: WithVariant) =
    var obj = Object() # obj stores an value that needs to be destroyed
    discard a.a

  runTest:
    test(WithVariant(kind: false)) # provoke a field-check failure

block object_conversion_checks:
  type A = ref object of RootObj

  proc test(a: RootRef) =
    var obj = Object() # obj stores a value that needs to be destroyed
    discard A(a)

  runTest:
    test(RootRef()) # provoke a object-conversion-check failure

block signed_integer_overflow_check:
  proc test(a: int32) =
    var obj = Object() # obj stores a value that needs to be destroyed
    discard 1'i32 + a

  runTest:
    test(high(int32)) # provoke an overflow-check failure
