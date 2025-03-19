
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
    return Continuation[int](has: true, val: result')

  proc b(env: pointer): Continuation[int] =
    return Continuation[int](has: false, next: a)

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
    x + y

  proc a_apply(env: ptr (int, int)): Continuation[int] {.musttail.} =
    a(env[][0], env[][1])

  proc b(env: pointer): Continuation[int] {.musttail.} =
    store env, (1, 2)
    return Continuation[int](has: false, next: a_apply)

`store` is a magic procedure responsible for writing the argument tuple to the
storage in a type-safe fashion.

The storage is fixed in size and allocated on the stack. This gets around
having to use heap allocation, but it also puts a hard limit on the maximum
possible space occupied by parameters.

How parameters are stored depends on their type and passing mode:
* `sink` parameters are always stored in full
* `var` parameters are stored as pointers
* all pass-by-reference parameters are stored as *pointer*
* everything else uses shallow copies

Since `sink` parameters may internally be passed by reference, a temporary
location has to be allocated for them, as the `env` may be modified by the
callee (which would clobber the storage already in use by a `sink`
parameter).

Invocation From Non- `.musttail` Routines
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To guarantee sibling-calls when calling `.musttail` procedures within
non- `.musttail` ones, the caller also has to be transformed to return a
`Continuation`.

All calls to the original procedure are then replaced with calls to a
synthesized trampoline procedure. Example:

.. code-block:: nim

  proc a(): int {.musttail.} =
    return

  proc b(x, y: int): int =
    a()

  discard b(1, 2)

becomes:

.. code-block:: nim

  proc a(env: pointer): Continuation[int] =
    return Continuation[int](has: true, val: 0)

  proc b(x, y: int, env: pointer): Continuation[int] =
    return Continuation[int](has: false, next: a)

  proc b_trampoline(x, y: int): int =
    var env = default(Storage)
    var cont = b(x, y, addr env)
    while not cont.has:
      cont = cont.next(addr env)
    return cont.val

  discard b_trampoline(1, 2)

This is not possible for all routines (e.g.: methods, iterators, exportc'ed
ones), which is the reason why `.musttail` calls are disallowed within those.
