##[
This module implements a result-type which encapsulates both a success and a
failure in one value. Only one of the two states can be active.

Based on https://github.com/disruptek/badresults, which was itself based on
https://github.com/arnetheduck/nim-result.

Exception translation
=====================

Using `get` or `value` raises an exception if the `Result` stores a failure.
For `Result[T, E]` the exception raised is:
 * if `E` is a `ref` type that inherits from `Exception`, the `error` value
 * if a custom overload of `toException <#toException, E>`_ exists, the result
  of `toException(r)`
 * otherwise, a new `ResultError[E]` with `error` as the payload

]##

import std/options

type
  ResultError*[E] = object of ValueError
    error: E

  ResultErrorRef*[E] = ref ResultError[E]

  Result*[T, E] = object
    ## A `Result` is typically used to augment a return value of type `T`
    ## indicating that a computation (proc, etc...) is uncertain and should
    ## return a value of type `T` or end in an error of type `E`. This is
    ## useful to avoid encoding entirely expected error cases as values instead
    ## of resorting to exception handling for program control flow or various
    ## side-channel data.
    case o: bool
    of false:
      e: E
    of true:
      v: T

template reportNoError() =
  raise ResultErrorRef[void](msg: "Result does not contain an error")

func error*[T, E](self: Result[T, E]): lent E {.inline.} =
  ## Retrieves the error from a `Result`; raises `ResultError[void]`
  ## if the `Result` is not an error.
  if self.isErr:
    result = self.e
  else:
    reportNoError()

func unsafeGet*[T, E](self: var Result[T, E]): var T {.inline.} =
  ## Fetches the value of a `Result` if set, undefined behavior if unset.
  ##
  ## See also:
  ## * `unsafeGet <options.html#unsafeGet,Option[T]>`_
  assert not isErr(self)
  result = self.v

func unsafeGet*[T, E](self: Result[T, E]): lent T {.inline.} =
  ## Fetches the value of a `Result` if set, undefined behavior if unset.
  ## See also:
  ## * `unsafeGet <options.html#unsafeGet,Option[T]>`_
  assert not isErr(self)
  result = self.v

func unsafeGet*[E](self: Result[void, E]) {.inline.} =
  ## Does nothing if `Result` is set, undefined behavior if unset.
  ## See also:
  ## * `unsafeGet <options.html#unsafeGet,Option[T]>`_
  assert not self.isErr

func newResultError[E](e: sink E;
                       s: sink string): ResultErrorRef[E] {.inline, nimcall.} =
  ## capturing ResultError...
  ResultErrorRef[E](error: e, msg: s)

template toException*[E](err: E): ResultErrorRef[E] =
  ## Refer to `this <#exception-translation>`_ for more information.
  ##
  ## The returned exception is required to be a `ref`.
  mixin `$`
  when compiles($err):
    newResultError(err, "Result isErr: " & $err)
  else:
    newResultError(err, "Result isErr; no `$` in scope.")

template raiseResultError[T, E](self: Result[T, E]) =
  mixin toException
  when E is ref Exception:
    if self.error.isNil: # for example Result.default()!
      raise ResultErrorRef[void](msg: "Result isErr; no exception.")
    else:
      raise self.error
  else:
    type ET = typeof(self.error.toException)
    when typeof(ET) is ref:
      raise self.error.toException
    else:
      # In theory, the non-ref exception could be turned into a ref
      # exception here
      {.error:
        "The provided `toException` overload doesn't produce a `ref` value".}

func ok*[E](R: typedesc[Result[void, E]]): Result[void, E] =
  ## Returns a result as success.
  R(o: true)

func initSuccess*[E](self: var Result[void, E]) =
  ## Sets a result to success.
  runnableExamples:
    var r: Result[void, string]
    r.initSuccess()
    doAssert r.isOk

  self = ok[E](typeof(self))

func ok*[T, E](R: typedesc[Result[T, E]]; v: sink T): Result[T, E] =
  ## Returns a result with a success and value.
  runnableExamples:
    let r = Result[int, string].ok(42)
    doAssert r.value == 42

  R(o: true, v: v)

proc initSuccess*[T, E](self: var Result[T, E]; v: sink T) =
  ## Sets the result to success and updates value.
  self = ok[T, E](typeof(self), v)

func err*[T, E](R: typedesc[Result[T, E]]; e: sink E): Result[T, E] =
  ## Returns a result with an error.
  runnableExamples:
    let r = Result[int, string].err("uh-oh")
    doAssert r.error == "uh-oh"

  R(o: false, e: e)

func initFailure*[T, E](self: var Result[T, E]; e: sink E) =
  ## Sets the result as an error.
  self = err[T, E](typeof(self), e)

func isOk*(self: Result): bool {.inline.} = self.o
func isErr*(self: Result): bool {.inline.} = not self.o

func `==`*(a, b: Result): bool {.inline.} =
  ## Equal if both `a` and `b` have the same ok/err status, then compares the
  ## value or error pairs using available `==` for those type of values.
  if a.isOk == b.isOk:
    if a.isOk: a.v == b.v
    else:      a.e == b.e
  else:
    false

template get*[T: not void, E](self: Result[T, E]): untyped =
  ## If `self` is set, returns the value. Raises `self.error` as an Exception
  ## otherwise.
  ##
  ## See also:
  ## * `get <options#get,Option[T]>`_
  if self.isErr:
    raiseResultError self
  unsafeGet self

template get*[T, E](self: Result[T, E]; otherwise: T): untyped =
  ## If `self` is set, returns the value. Returns `otherwise` if `self` is
  ## an error.
  ##
  ## See also:
  ## * `get <options#get,Option[T]>`_
  if self.isErr:
    otherwise
  else:
    unsafeGet self

template get*[T, E](self: var Result[T, E]): untyped =
  ## If `self` is set, returns the mutable value. Raises `self.error` as an
  ## `Exception` otherwise.
  ##
  ## See also:
  ## * `get <options#get,Option[T]>`_
  if self.isErr:
    raiseResultError self
  unsafeGet self

template get*[E](self: Result[void, E]) =
  ## Raise error as an `Exception` if `self.isErr`.
  ##
  ## See also:
  ## * `get <options#get,Option[T]>`_
  if self.isErr:
    raiseResultError self

proc `$`*[T: not void; E](self: Result[T, E]): string =
  ## Returns the string representation of `self`
  if self.isOk: "Ok(" & $self.v & ")"
  else: "Err(" & $self.e & ")"

func `$`*[E](self: Result[void, E]): string =
  ## Returns the string representation of `self`
  if self.isOk: "Ok()"
  else: "Err(" & $self.e & ")"

template value*[T, E](self: Result[T, E]): T = get self
template value*[T, E](self: var Result[T, E]): T = get self

template value*[E](self: Result[void, E]) = get self
template value*[E](self: var Result[void, E]) = get self

template valueOr*[T, E](self: Result[T, E], def: T): T =
  ## Fetches the value of result if set, or supplied default
  ## `def` will not be evaluated iff value is set
  self.get(def)

proc map*[T: not void; E, R](r: Result[T, E],
                             callback: proc(v: T): R
                            ): Result[R, E] {.effectsOf: callback.} =
  ## See also:
  ## * `mapErr <#mapErr,Result[T, E],proc(E)>`_
  ## * `map <options#map,Option[T],proc(T)>`_
  if r.isOk: Result[R, E].ok(callback(r.unsafeGet))
  else:      Result[R, E].err(r.e)

proc mapErr*[T: not void; E, R](r: Result[T, E],
                                callback: proc(e: E): R
                               ): Result[T, R] {.effectsOf: callback.} =
  ## See also:
  ## * `map <#map,Result[T, E],proc(T)>`_
  ## * `map <options#map,Option[T],proc(T)>`_
  if r.isErr: Result[T, R].err callback(r.e)
  else:       Result[T, R].ok r.unsafeGet

proc flatMap*[T: not void; E, R](r: Result[T, E],
                                 callback: proc(v: T): Result[R, E]
                                ): Result[R, E] {.effectsOf: callback.} =
  ## See also:
  ## * `flatMapErr <#flatMapErr,Result[T, E],proc(E)>`_
  ## * `flatMap <options#flatMap,Option[T],proc(T)>`_
  if r.isOk: callback(r.unsafeGet)
  else:      Result[R, E].err r.e

proc flatMapErr*[T: not void; E, R](r: Result[T, E],
                                    callback: proc(e: E): Result[T, R]
                                   ): Result[T, R] {.effectsOf: callback.} =
  ## See also:
  ## * `flatMap <#flatMap,Result[T, E],proc(T)>`_
  ## * `flatMap <options#flatMap,Option[T],proc(T)>`_
  if r.isErr: callback(r.e)
  else:       Result[T, R].ok r.unsafeGet

func filter*[T: not void; E](r: Result[T, E],
                             callback: proc(v: T): bool): Option[T] =
  ## See also:
  ## * `filterErr <#filterErr,Result[T, E],proc(E)>`_ for filtering based on
  ##  the error
  ## * `filter <options#filter,Option[T],proc(T)>`_
  if r.isOk and callback(r.v):
    some(r.v)
  else:
    none(T)

func filterErr*[T, E](r: Result[T, E],
                      callback: proc(e: E): bool): Option[E] =
  ## See also:
  ## * `filter <#filter,Result[T, E],proc(T)>`_ for filtering based on the
  ##  value
  ## * `filter <options#filter,Option[T],proc(T)>`_
  if r.isErr and callback(r.e):
    some(r.e)
  else:
    none(E)

func take*[T: not void; E](r: sink Result[T, E]): T =
  ## Returns the result's value if it stores one. Raises the result's error as
  ## an exception otherwise
  result = move(r.value)

func takeErr*[T, E](r: sink Result[T, E]): E =
  ## Returns the result's error if it stores one. Raises a `ResultError[void]`
  ## otherwise
  if r.isErr:
    result = move(r.e)
  else:
    reportNoError()