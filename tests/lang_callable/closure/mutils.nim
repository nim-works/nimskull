## Utility routines for writing tests

import std/macros

type Target* = enum
  c
  js
  vm

macro test*(label: untyped, targets: static set[Target], code: untyped) =
  ## Wraps the `code` in a block with label `label` that is only enabled when
  ## the active target is present in `targets`
  assert targets != {}
  var cond = NimNode(nil)

  for t in targets.items:
    let name = newIdentNode($t)
    if cond == nil:
      cond = quote do: defined(`name`)
    else:
      cond = quote do: `cond` or defined(`name`)

  result = newTree(nnkWhenStmt):
    newTree(nnkElifBranch, cond):
      newTree(nnkBlockStmt, label, code)