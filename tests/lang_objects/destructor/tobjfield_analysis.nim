discard """
  output: '''works'''
"""

#  bug #11095

type
  MyVal[T] = object
    f: ptr T

proc `=destroy`[T](x: var MyVal[T]) =
  if x.f != nil:
    dealloc(x.f)

proc `=sink`[T](x1: var MyVal[T], x2: MyVal[T]) =
  if x1.f != x2.f:
    `=destroy`(x1)
    x1.f = x2.f

proc `=`[T](x1: var MyVal[T], x2: MyVal[T]) {.error.}

proc newVal[T](x: sink T): MyVal[T] =
  result.f = create(T)
  result.f[] = x

proc set[T](x: var MyVal[T], val: T) =
  x.f[] = val

proc sinkMe[T](x: sink MyVal[T]) =
  discard

var flag = false

proc main =
  var y = case flag
    of true:
      var x1 = newVal[float](1.0)
      var x2 = newVal[float](2.0)
      (newVal(x1), newVal(x2))

    of false:
      var x1 = newVal[float](1.0)
      var x2 = newVal[float](2.0)
      (newVal(x1), newVal(x2))

  sinkMe y[0]
  sinkMe y[1]
  echo "works"

main()

import mhelper

test constructed_object_field_access:
  type Wrapper = object
    a: Value[int]
    b: Value[int]

  proc prc() =
    # a literal constructor expression (which ``initValue`` also expands to)
    # not used *directly* in a consume context only produces a non-owning
    # container
    # TODO: this doesn't match the behaviour the specification describes (i.e.
    #       constructor expression having the same semantics as procedure calls
    #       in the context of lifetime hook injection). The behaviour here
    #       should either be explicitly added to the spec or changed
    var x = Wrapper(a: initValue(1), b: initValue(2)).a # use a copy

  prc()

  doAssert numCopies == 1