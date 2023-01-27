===================================
Mid-end intermediate representation
===================================

.. include:: rstcommon.rst
.. default-role:: code

.. note:: This document is a work in progress.

Purpose
=======

The MIR is designed and meant to be used as a target-independent
intermediate representation for analysis, transformations, and
lowerings. It is centered around operations, control-flow, and locations.

In the compilation pipeline, the MIR is located after semantic analysis and
high-level transformation (``transf``) but before code-generation.

Overview
========

For reasons of flexibility, modularity, and since the MIR eventually should
support type lowering, it is itself type agnostic: no knowledge of the types
is required in order to reason about the semantics of MIR code.

.. note:: While this eventually should be the case, it is currently not
          entirely true. To know whether a conversion is an lvalue
          conversion or not, looking at both the source and destination type
          is required

Currently, ``PType``s are required for representing the type of nodes, but
the plan is to use an ID (handle) centered design where what the ID refers
to is up to the MIR's user.

Even though the control-flow primitives are simpler and more regular than
the ones used by the higher-level AST, they're still fairly high-level.
For example, constructs like ``try`` and ``case`` statements still exist.
The reason for this is twofold:
- due to the code-generators still operating on ``PNode`` AST, the MIR code
  has to be translated back to it. Translating low-level control-flow
  primitives such as 'goto's back into 'if', 'block', and 'break' is fairly
  involved and would also introduce additional overhead to the
  back-translation
- some transformation passes currently using the MIR are simpler to implement
  with access to first-class control-flow constructs such as 'try-finally'

The goal is to eventually also have low-level primitives like 'goto's as
part the MIR, maybe replacing the current higher-level ones.

Concepts
========

An *operator* (operation?) is something that takes zero or more *values* (operands) and
produces an another value. An initial value is provided via what is
referred to as an *input*. This is either: the name of a location, the name of
a constant, or a nullary operation (one that has no operands). A value always
has to be consumed by something. Note that "consume" here only means "used by
something" and is not related to the higher-level concept of resource ownership
in any way.

An *operation sequence* starts with an input, is followed by zero or more
operators, and ends in an output.

An *output* is a special operator that accepts one or more operands but
doesn't produce a value.

Effects
-------

There are three kinds of *effects*: control-flow effects, location effects, and
general effects. Applying an operator can have one or more of these
effects.

Location effects are encoded via a special parameterized operator: the `tag`
operator. It takes a single *lvalue* as input and is purely declarative.
Applying the operator that the tagged lvalue is an operand to then has the
provided effect to the on the lvalue's underlying location.

Structure
=========

Everything part of the MIR is stored in a `MirTree` (which is a single `seq` of
`MirNode`s). This simplifies processing and applying changes, as all
information is encoded with `MirNode`s. There are downside to this, however (which?).

High-level
----------

On an abstract level, a tree/sequence-hybrid representation is used. That is,
some constructs (mostly statements) use a node-tree tructure while others
use a node *sequence*. Both are stored in the same underlying storage
buffer -- they're not separated.

Due to the focus on control-flow, the nodes are ordered in such as way as that
a linear iteration over all nodes will yield them in an order that reflects how
the operators the nodes represent are executed in the final program (ignoring
loops and branching control-flow). This was an explicit design goal.

Memory layout
-------------

Nodes are layed out as a flat contiguous sequence in memory. This allows for
efficient linear traversal and random access, which is not possible with a
pointer-based tree structure (such as the one currently used with the AST).

Grammar
=======

The MIR grammar, represented in EBNF. It describes how the nodes are layed out
in memory:

.. code-block:: literal

  obj-constr = "objConstr", {"field"}, "end"

  name = "proc" | "const" | "global" | "param" | "local" | "temp" | "type" |
         "literal"

  def = "def", name, "end" {* note: not all names are semantically valid inside a 'def' *}

  stmt-list  = "stmtList", {stmt-list-item}, "end"
  stmt = stmt-list | single-stmt
  if-stmt    = "if", stmt, "end"
  while-stmt = "while", stmt, "end"
  block-stmt = "block", stmt, "end"

  branch-val = "literal" | "type"
  branch = "branch", {branch-val}, stmt, "end"

  case-stmt = "case", branch, {branch}, "end"

  except-branch = "branch", {("type" | "pnode")}, stmt, "end"

  except   = "except", except-branch, {except-branch}, "end"
  finally  = "finally", stmt, "end"
  try-stmt = "try", stmt, [except], [finally], "end"

  scope = "scope", {stmt-list-item}, "end"

  single-stmt = "break" | "return" | "pnode" | def | while-stmt | try-stmt |
                block-stmt | scope

  region = "region", {stmt-list-item}, "end"

  arg = in-op, {in-out-op}, ("arg" | "name" | "consume")
  arg-block-item = arg | stmt-list-item
  arg-block = "argBlock", {arg-block-item}, arg, "end"

  in-op     = "none" | name | arg-block | "opParam" {* may only appear inside a region *}
  out-op    = "void" | "raise" | "init" | "fastAsgn" | "asgn" | "switch" |
              def | if-stmt | case-stmt | region
  in-out-op = "magic" | "call" | "pathNamed" | "pathPos" | "pathArray" |
              "pathVariant" | "constr" | obj-constr | "cast" | "deref" |
              "addr" | "stdConv" | "conv" | "view" | "derefView"

  sequence = in-op, {in-out-op}, out-op

  stmt-list-item = sequence | single-stmt

  top-level = stmt-list-item {* the entry point *}