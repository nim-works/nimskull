discard """
  description: "Tests for the compiler/mir/datatables module"
  targets: native
"""

import compiler/ast/ast
import compiler/mir/mirtypes
include compiler/mir/datatables

# some placeholder types to assing to the nodes. For object types, a different
# ID means that it's a different type
let
  t1 = Int8Type
  t2 = Int16Type
  t3 = Int32Type

# node constructor
template node(k: MirNodeKind, t: TypeId, field, val: untyped): MirNode =
  MirNode(kind: k, typ: t, field: val)
template node(k: MirNodeKind, field, val: untyped): MirNode =
  MirNode(kind: k, field: val)
template node(k: MirNodeKind): MirNode =
  MirNode(kind: k)
template literal(val: NumberId): MirNode =
  MirNode(kind: mnkIntLit, number: val)

block tree_equality:
  # the type is only relevant for the head of the tree (the first node)

  # setup a list of structurally valid and unique (in terms of equality) trees
  let trees = @[
    # --- literals
    @[node(mnkIntLit, t1, number, NumberId 0)],
    @[node(mnkIntLit, t2, number, NumberId 0)],
    @[node(mnkUIntLit, t1, number, NumberId 0)],
    @[node(mnkUIntLit, t2, number, NumberId 0)],
    @[node(mnkStrLit, t1, strVal, StringId 0)],
    @[node(mnkStrLit, t1, strVal, StringId 1)],
    @[node(mnkFloatLit, t1, number, NumberId 0)],
    @[node(mnkFloatLit, t2, number, NumberId 0)],

    # --- ordered aggregates
    @[node(mnkTupleConstr, t1, len, 0)],
    @[node(mnkTupleConstr, t2, len, 0)],
    @[node(mnkTupleConstr, t1, len, 1),
        node(mnkArg), literal(NumberId 0)],
    @[node(mnkTupleConstr, t1, len, 2),
        node(mnkArg), literal(NumberId 0),
        node(mnkArg), literal(NumberId 0)],

    # --- aggregates with fields
    @[node(mnkObjConstr, t1, len, 0)],
    @[node(mnkObjConstr, t2, len, 0)],
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkBinding),
          node(mnkField, field, 0),
          node(mnkArg), literal(NumberId 0)],
    # same field value, different field:
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkBinding),
          node(mnkField, field, 1),
          node(mnkArg), literal(NumberId 0)],
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkBinding),
          node(mnkField, field, 0),
          node(mnkArg), literal(NumberId 0),
        node(mnkBinding),
          node(mnkField, field, 1),
          node(mnkArg), literal(NumberId 0)],
    # swapped fields
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkBinding),
          node(mnkField, field, 1),
          node(mnkArg), literal(NumberId 0),
        node(mnkBinding),
          node(mnkField, field, 0),
          node(mnkArg), literal(NumberId 0)]
  ]

  # compare all trees with each other
  for i in 0..<trees.len:
    doAssert cmp(trees[i], trees[i]) # tree must be equal to itself
    for j in (i+1)..<trees.len:
      if cmp(trees[i], trees[j]):
        echo "compared equal, but shouldn't: ", i, " vs. ", j
        doAssert false
