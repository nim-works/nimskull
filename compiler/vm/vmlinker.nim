## Implements types and procedures related to mapping symbols to their in-VM
## IDs.

import
  std/[
    strutils
  ],
  compiler/ast/[
    ast_types
  ]

from compiler/ast/ast import id
from compiler/ast/idents import cmpIgnoreStyle

type
  LinkIndex* = uint32
    ## Identifies a linker-relevant entity. There are three namespaces, one
    ## for procedures, one for globals, and one for constants -- which
    ## namespace an index is part of is stored separately.

  IdentPattern* = distinct string
    ## A matcher pattern for a fully qualified symbol identifier

  LinkerData* = object
    ## All data required for mapping symbols to the entities they represent
    ## inside the VM (procedures, globals, constants).
    callbackKeys*: seq[IdentPattern]
      ## the matcher patterns used for implementing overrides

func matches(s: PSym; x: IdentPattern): bool =
  var s = s
  for part in rsplit(string(x), '.'):
    if s == nil or (part.cmpIgnoreStyle(s.name.s) != 0 and part != "*"):
      return false
    s = if sfFromGeneric in s.flags: s.owner.owner else: s.owner
    while s != nil and s.kind == skPackage and s.owner != nil: s = s.owner
  result = true

func lookup*(patterns: seq[IdentPattern]; s: PSym): int =
  ## Tries to find and return the index of the pattern matching `s`. If none
  ## is found, -1 is returned
  var i = 0
  # XXX: `pairs` doesn't use `lent`, so a manual implementation of `pairs`
  #      is used
  for p in patterns.items:
    if s.matches(p): return i
    inc i

  result = -1
