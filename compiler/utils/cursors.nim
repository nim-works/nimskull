## This module implements the ``Cursor`` container type, which works similar to
## the ``.cursor`` similar to the ``.cursor`` pragma. It creates a non-owning
## and **unsafe** view into the source location, and is meant as a temporary
## solution until first-class view-types become stable enough to be used in the
## compiler.
##
## Once first-class view-types are stable, all usages of ``Cursor[T]`` are to be
## replaced with usage of ``lent T``.
##
## **There is nothing preventing the ``Cursor`` living longer than the borrowed
## from location**.

type
  Cursor*[T] = distinct ptr T
    ## Not as efficient as a built-in cursor location (i.e. with ``.cursor``),
    ## but works when used for fields that are not of ``ref`` or closure type.
    ##
    ## While neither an intended nor encouraged (it is actively discouraged)
    ## use case, compared to ``.cursor``, all modifications to the borrowed-
    ## from location through handles other than the ``Cursor`` are observable
    ## through the cursor.

# ------ ``Cursor`` implementation

template cursor*[T](x: T): untyped =
  ## Creates an **unsafe** view into `x`
  Cursor[T](unsafeAddr x)

func `[]`*[T](x: Cursor[T]): lent T {.inline.} =
  ## Dereferences the cursor `x`. A procedure is used instead of a template in
  ## order to prevent modifications of the pointed-to location
  (ptr T)(x)[]

# convenience routines for cursors of ``seq``s:

template `[]`*[I: Ordinal; T: seq](s: Cursor[T], i: I): untyped =
  s[][i]

template len*[T: seq](s: Cursor[T]): int =
  s[].len
