
Overview
--------

This document describes how the portable tail-call elimination used for
calls to `.musttail` routines works.

Tail-call elimination refers to the process of turning a tail-call into a
sibling-call, that is, a call that re-uses the same stack frame as the caller.

Transformation
--------------

The following `.musttail` procedures:

.. code-block:: nim

  proc a(): int {.musttail.} =
    result = 1
    result = 2
    return

  proc b(): int {.musttail.} =
    return a()

are transformed into the internal equivalent of:

.. code-block:: nim

  proc a(env: pointer): Continuation[int] =
    # the original `result` simply becomes a normal variable, so that result
    # assignments, taking the address of `result`, etc. all continue to work
    var result': int
    result' = 1
    result' = 2
    return Continuation[int](done: true, result: result')

  proc b(env: pointer): Continuation[int] =
    return Continuation[int](done: false, next: a)

A `Continuation` is either terminal (it stores the procedure's result) or
non-terminal (it stores the procedure to continue with).

Since `.musttail` routines are allowed to call other `.musttail` routines with
arbitrary signatures, but `Continuation` can only store procedures of a single
type, thunks and a separate parameter storage are used:

.. code-block:: nim

  proc a(x, y: int): int {.musttail.} =
    x + y

  proc b(): int {.musttail.} =
    a(1, 2)

  # become:

  proc a(x, y: int, env: pointer): Continuation[int] {.musttail.} =
    return Continuation[int](done: true, result: x + y)

  proc a_apply(env: ptr (int, int)): Continuation[int] {.musttail.} =
    return a(env[][0], env[][1])

  proc b(env: pointer): Continuation[int] {.musttail.} =
    store env, (1, 2)
    return Continuation[int](done: false, next: a_apply)

`store` is a magic procedure responsible for writing the argument tuple to the
storage in a type-safe fashion.

The storage is fixed in size and allocated on the stack. This gets around
having to use heap allocation, but it also puts a hard limit on the maximum
available space occupied by parameters.

How parameters are stored depends on their type and passing mode:
* `sink` parameters are always stored as owning values
* `var` parameters are stored as pointers
* all pass-by-reference parameters are stored as pointers
* everything else uses shallow copies

Since `sink` parameters may internally be passed by reference, a temporary
location has to be allocated for them, as the `env` may be modified by the
callee (which would clobber the storage already in use by a `sink`
parameter).

Invocation From Non- `.musttail` Routines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a `.musttail` routine is called from a routine that is not a `.musttail`
routine, no guaranteed tail call (and thus sibling call) takes places.

The caller receives the `Continuation` returned by the `.musttail` callee and
"trampolines" it to completion. Example:

.. code-block:: nim

  proc a(x, y: int): int {.musttail.} =
    x + y

  proc b(): int =
    result = a(4, 5)
    echo result

becomes:

.. code-block:: nim

  proc a(x, y: int, env: pointer): Continuation[int] =
    return Continuation[int](done: true, result: x + y)

  proc b(): int =
    var env: Storage
    var cont = a(4, 5, addr env)
    while not cont.done:
      cont = cont.next(addr env)
    result = cont.val

    echo result
