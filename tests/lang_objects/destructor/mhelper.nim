## Helper module containing utilities for the lifetime-hook related tests


type
  Resource* = object
    ## Something that represents a resource
    content*: bool

  Value*[T] = object
    has: bool
    content*: T

var numCopies*, numSinks*, numDestroy*: int

## ------ `Resource` hooks

proc `=destroy`(x: var Resource) =
  if x.content:
    inc numDestroy

proc `=copy`(x: var Resource, y: Resource) =
  `=destroy`(x)
  x.content = y.content
  if x.content:
    inc numCopies

proc `=sink`(x: var Resource, y: Resource) =
  `=destroy`(x)
  x.content = y.content
  if x.content:
    inc numSinks

## ------ `Value` hooks

proc `=destroy`[T](x: var Value[T]) =
  if x.has:
    inc numDestroy

proc `=copy`[T](x: var Value[T], y: Value[T]) =
  `=destroy`(x)
  wasMoved(x)
  if y.has:
    inc numCopies
    x.has = true
    x.content = y.content

proc `=sink`[T](x: var Value[T], y: Value[T]) =
  `=destroy`(x)
  wasMoved(x)
  if y.has:
    inc numSinks
    x.has = true
    x.content = y.content


template test*(name, code: untyped) =
  ## Helper template for a test case that checks the `numCopies`, `numSinks`,
  ## or `numDestroy` counters
  block name:
    # reset the counters first:
    numCopies = 0
    numSinks = 0
    numDestroy = 0

    code

# use templates in order to interfere as little as possible with the tests

template initResource*(): untyped =
  Resource(content: true)

template initValue*[T](x: T): untyped =
  Value[T](has: true, content: x)


func use*[T](x: T) =
  discard

proc mutate*[T](x: var T) =
  discard