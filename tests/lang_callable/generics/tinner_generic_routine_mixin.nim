discard """
  description: '''
    Generic routines defined within other routines use the same scope for
    mixins and late symbol resolution as the one they're defined in.
  '''
"""

proc simple() =
  proc generic[T](x: T): int =
    mixin other
    other()

  proc other(): int = 1

  doAssert generic("") == 1

simple()

# the enclosing scope for inner generic routines is also visible to non-mixin
# symbol resolution happening at instantiation time

template get(x: int): int =
  mixin other
  other()

proc templateSymbol() =
  proc generic[T](x: T): int =
    # `get` is a late-expanded template
    get(x)

  proc other(): int = 1

  doAssert generic(2) == 1

templateSymbol()

# the mixin only considers symbols from the same scope as the generic is
# defined in, even when it's instantiated in a more nested scope
proc sameScopeOnly() =
  proc generic[T](x: T) =
    mixin other
    static:
      doAssert not declared(other)

  block: # introduces a new scope
    proc other() = discard
    # ^^ not visible
    generic(0)

sameScopeOnly()
