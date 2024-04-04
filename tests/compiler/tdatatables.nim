discard """
  description: "Tests for the compiler/mir/datatables module"
  targets: native
"""

import compiler/ast/ast
include compiler/mir/datatables

# some placeholder types to assing to the nodes. For object types, a different
# ID means that it's a different type
let
  t1 = PType(itemId: ItemId(item: 1), kind: tyObject, sons: @[PType nil])
  t2 = PType(itemId: ItemId(item: 2), kind: tyObject, sons: @[PType nil])
  t3 = PType(itemId: ItemId(item: 3), kind: tyObject, sons: @[PType nil])
  field1 = PSym(itemId: ItemId(item: 1))
  field2 = PSym(itemId: ItemId(item: 2))

# node constructor
template node(k: MirNodeKind, t: PType, field, val: untyped): MirNode =
  MirNode(kind: k, typ: t, field: val)
template node(k: MirNodeKind, field, val: untyped): MirNode =
  MirNode(kind: k, field: val)
template node(k: MirNodeKind): MirNode =
  MirNode(kind: k)
template literal(val: PNode): MirNode =
  MirNode(kind: mnkLiteral, lit: val)

block tree_equality:
  # the type is only relevant for the head of the tree (the first node)

  # setup a list of structurally valid and unique (in terms of equality) trees
  let trees = @[
    # --- literals
    @[node(mnkLiteral, t1, lit, newIntNode(nkIntLit, 0))],
    @[node(mnkLiteral, t2, lit, newIntNode(nkIntLit, 0))],
    @[node(mnkLiteral, t1, lit, newStrNode(nkStrLit, ""))],
    @[node(mnkLiteral, t1, lit, newStrNode(nkStrLit, "a"))],
    @[node(mnkLiteral, t1, lit, newFloatNode(nkFloatLit, 0.0))],
    # 0.0 and -0.0 are different float values
    @[node(mnkLiteral, t1, lit, newFloatNode(nkFloatLit, -0.0))],

    # --- ordered aggregates
    @[node(mnkTupleConstr, t1, len, 0), node(mnkEnd)],
    @[node(mnkTupleConstr, t2, len, 0), node(mnkEnd)],
    @[node(mnkTupleConstr, t1, len, 1),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)),
      node(mnkEnd)],
    @[node(mnkTupleConstr, t1, len, 2),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
      node(mnkEnd)],

    # --- aggregates with fields
    @[node(mnkObjConstr, t1, len, 0), node(mnkEnd)],
    @[node(mnkObjConstr, t2, len, 0), node(mnkEnd)],
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkField, field, field1),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
      node(mnkEnd)],
    # same field value, different field:
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkField, field, field2),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
      node(mnkEnd)],
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkField, field, field1),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
        node(mnkField, field, field2),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
      node(mnkEnd)],
    # swapped fields
    @[node(mnkObjConstr, t1, len, 1),
        node(mnkField, field, field2),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
        node(mnkField, field, field1),
        node(mnkArg), literal(newIntNode(nkIntLit, 0)), node(mnkEnd),
      node(mnkEnd)]
  ]

  # compare all trees with each other
  for i in 0..<trees.len:
    doAssert cmp(trees[i], trees[i]) # tree must be equal to itself
    for j in (i+1)..<trees.len:
      if cmp(trees[i], trees[j]):
        echo "compared equal, but shouldn't: ", i, " vs. ", j
        doAssert false
