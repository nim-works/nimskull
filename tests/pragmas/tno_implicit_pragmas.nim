discard """
  description: '''
    Block and statement pragmas don't inherit implicit (i.e. pushed) pragmas
  '''
  action: compile
"""

import std/macros

template custom(x: int) {.pragma.}

macro verify(x: typed) =
  ## Verifies that the pragma list doesn't contain the ``custom`` pragma
  expectKind(x, nnkSym)
  let impl = x.getImpl()

  for it in impl.pragma.items:
    if it.kind == nnkCall and it[0].kind == nnkSym and
        it[0] == bindSym"custom":
      error("the `custom` pragma was found!", it)

proc p() =
  {.push custom(1).}

  # the content doesn't matter, as long as the pragma list contains at least
  # one item (in order to prevent them from getting optimized away, now or in
  # the future)
  {.cast(noSideEffect).}:
    discard

  {.hints: off.}

  {.pop.}

# ``p`` must not have the ``custom`` pragma attached!
verify(p)