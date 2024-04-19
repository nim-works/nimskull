===================================
Mid-end intermediate representation
===================================

.. include:: rstcommon.rst
.. default-role:: code

.. note:: This document is a work in progress.

Overview
========

The MIR is a desugared, simplified, and flattened intermediate representation
(=IR) of NimSkull code that is intended for analysis, transformations, and
lowerings.

In the compilation pipeline, the stage using the MIR is located after semantic
analysis and high-level transformation (``transf``) but before code
generation.

.. note:: The plan is to replace the current IR consumed by the code
          generators (i.e., the `CgNode` IR) with the MIR

Semantics
=========

.. code-block:: literal

  NAME = <Temp>   # a temporary introduced during the MIR phase
       | <Alias>  # a temporary view introduced during the MIR phase
       | <Local>  # a user-defined local or temporary introduce prior to the
                  # MIR phase
       | <Global> # a user-defined global
       | <Const>  # a user-defined constant

  LVALUE = NAME
         | PathNamed   LVALUE <Field>    # named field access (objects only)
         | PathPos     LVALUE <Position> # positional field access (tuples only)
         | PathVariant LVALUE <Field>    # access a variant within an object
         | PathConv    LVALUE <Type>     # access a sub- or super-type of
         | PathArray   LVALUE (NAME | <Literal>) # access an array at given
                                         # position
         | Deref       NAME              # dereference a `ptr` or `ref`
         | DerefView   NAME              # dereference a `var` or `lent`

  VALUE = <Literal>
        | <ProcVal>
        | <Type>
        | LVALUE

  INTERMEDIATE_TARGET = <Label>
                      | Leave <Label>

  TARGET = <Label>
         | TargetList INTERMEDIATE_TARGET ... <Label>

  EX_TARGET = TARGET
            | TargetList INTERMEDIATE_TARGET ... Resume

  UNARY_OP = NegI VALUE

  BINARY_OP = AddI VALUE, VALUE
            | SubI VALUE, VALUE
            | MulI VALUE, VALUE
            | DivI VALUE, VALUE
            | ModI VALUE, VALUE

  CALL_ARG = Arg VALUE                    # pass-by-value argument
           | Arg <none>                   # argument that's going to be omitted
                                          # later
           | Name LVALUE                  # pass-by-name argument where the
                                          # lvalue is only used for reading
           | Name (Tag <Effect> LVALUE)   # pass-by-name argument where the
                                          # lvalue is used for mutation
           | Consume VALUE                # pass-by-value argument, but
                                          # the value is consumed (i.e., moved)

  CONSTR_ARG = Arg VALUE
             | Consume OPERAND

  CALL_EXPR = Call <Proc> CALL_ARG ...   # a static call of the provided
                                         # procedure with the given arguments
            | Call LVALUE CALL_ARG ...   # indirect call
            | Call <Magic> CALL_ARG ...  # a call of a magic procedure (i.e.,
                                         # a procedure that is either going to
                                         # be lowered into something else, or
                                         # one for which the behaviour cannot
                                         # be represented in the MIR)

  # checked calls have the same shape as normal calls. The difference
  # is that the call has an exceptional exit (i.e., it might raise an
  # exception)
  CHECKED_CALL_EXPR = CheckedCall <Proc> CALL_ARG ...  EX_TARGET
                    | CheckedCall LVALUE CALL_ARG ...  EX_TARGET
                    | CheckedCall <Magic> CALL_ARG ... EX_TARGET

  SET_CONSTR_ARG = VALUE
                 | Range VALUE VALUE     # range construction

  RVALUE = UNARY_OP
         | BINARY_OP
         | CALL_EXPR
         | CHECKED_CALL_EXPR
         | SetConstr SET_CONSTR_ARG ...
         | ArrayConstr CONSTR_ARG...
         | SeqConstr CONSTR_ARG...
         | TupleConstr CONSTR_ARG...
         | ClosureConstr CONSTR_ARG...
         | ObjConstr (<Field> CONSTR_ARG) ... # construct an `object`
         | RefConstr (<Field> CONSTR_ARG) ... # construct a `ref object`
         | StdConv  VALUE                # number conversion or conversion
                                         # between cstring and string
         | Conv     VALUE                # same as `StdConv`. Only duplicate
                                         # for legacy code generator reasons
         | Cast     VALUE                # reinterpret the value as a different
                                         # type
         | Addr     LVALUE               # create a pointer from the lvalue
         | View     LVALUE               # create a view (`var`/`lent`) of the
                                         # lvalue
         | MutView  LVALUE
         | ToSlice  VALUE                # create an `openArray` slice of
                                         # the full sequence
         | MutToSlice LVALUE
         | ToSlice  VALUE, VALUE, VALUE  # create an `openArray` slice from the
                                         # first operand, starting at the lower
                                         # bound (second parameter) and ending
                                         # at the upper bound (inclusive, third
                                         # parameter)
         | MutToSlice LVALUE, VALUE, VALUE

  ASGN_SRC = RVALUE
           | VALUE
           | Copy VALUE
           | Move LVALUE
           | Sink LVALUE

  SHALLOW_SRC = RVALUE
              | VALUE

  STATEMENT = Scope STATEMENT           # wrap the statement in a scope, which
                                        # delimits the lifetime of all
                                        # definitions within
            | Def NAME none             # definition
            | Def NAME ASGN_SRC         # definition + initial value assignment
            | DefCursor NAME            # definition of non-owning location
            | DefCursor NAME SHALLOW_SRC# definition of non-owning location +
                                        # initial (shallow copy) assignment
            | Bind <Alias> LVALUE       # bind the lvalue to the given alias.
                                        # May be used for mutation, but must
                                        # not be used as an assignment's
                                        # destination or `Tag`'s operand
            | BindMut <Alias> LVALUE    # bind the lvalue to the given alias.
                                        # The alias may be used as an
                                        # assignment's destination or as a
                                        # `Tag`'s operand
            | Void LVALUE               # evaluates the lvalue for side-effects
                                        # and acts as a usage of the lvalue
                                        # during data-flow analysis
            | Void CALL_EXPR            # represents a void call. The called
                                        # procedure or magic *must* have a
                                        # `void`` return type
            | Asgn LVALUE ASGN_SRC      # normal assignment of the right value
                                        # to the left location
            | Init LVALUE ASGN_SRC      # initial assignment (the destination
                                        # is empty)
            | Switch LVALUE ASGN_SRC    # changes the active branch of a
                                        # variant. Unclear semantics.
            | If VALUE <Label>          # fall through if the value evaluates
                                        # to true, otherwise jump to the if's
                                        # corresponding end
            | Case VALUE BRANCH_LIST    # dispatch to one of the branches based
                                        # on the value, where value must be
                                        # either of integer, float, or string
                                        # type
            | Goto TARGET
            | Loop <Label>              # unconditional jump back to the start
                                        # of a loop
            | Destroy LVALUE
            | Raise LVALUE EX_TARGET
            | Raise <None> EX_TARGET
            | Join <Label>              # join point for non-exceptional
                                        # control-flow (e.g., goto)
            | LoopJoin <Label>          # join point for `Loop`
            | Except <Type> ... EX_TARGET
            | Except <Local> EX_TARGET
            | Except                    # catch-all handler
            | Finally <Label>
            | Continue <Label> (<Label> | Resume) ...
            | End <Label>               # marks the end of an if, repeat, or
                                        # except
            | Emit VALUE ...
            | Asm VALUE ...

  BRANCH_LABEL = <Literal>
               | <Const>
               | Range <Literal> <Literal>
  BRANCH_LIST = (Branch BRANCH_LABEL ... TARGET) ... # a list of branches

Only allowing calls, conversions, casts, etc. as the source operand (i.e., on
the right) of an assignment makes sure that they always have named receivers,
which:
* ensures an atomic statement (e.g., assignment) only has at most one control-
  flow effect, making control-/data-flow analysis easier
* allows for local, assignment-based lowering of call, magics, etc.

If a call raises an exception, whether the assignment destination is
written to is currently undefined.

Types
-----

``PType`` is currently re-used for representing types at the MIR stage. In the
future, the usage of ``PType`` will be replaced with simplified and more data-
oriented version that betters supports lowering and traversal.

Effects
-------

Lvalues passed to by-name parameters may optionally be tagged with an *effect*.
The effect describes what may happen to the underlying location *during*
execution of the procedure. If no effects are specified, the location may only
be read from during execution of the procedure.

This information is intended for use by data-flow analysis and code
generators.


Control Flow Representation
===========================

Terminology:
* *basic block*: a basic block is a region of statements that contains no
  jumps and is not jumped into
* *label*: identifies a control-flow-related construct
* *terminator*: marks the end of a basic block

A basic block is started by `Finally` and `Except`. Terminators are: `Case`,
`Goto`, `Raise`, `Continue`, and `Loop`. `If`, `Join`, and `LoopJoin` act as
both the start and end of a basic block. The nature of `End` depends on the
associated construct:
* for `If`, it acts as both a terminator and start of a basic block
* for `Except`, it only marks the end of the section

Except for `Loop`, all terminators only allow forward control-flow.

Structured Constructs
---------------------

Each `If` and `Except` must be paired with exactly one `End`, each `LoopJoin`
with a `Loop`, and each `Finally` with a `Continue`. These are the
*structured* constructs, and they must not overlap each other, meaning
that:

.. code-block::

  if x (L1)
  if y (L2)
  end L1
  end L2

is not allowed. However, much like in the high-level language, structured
constructs can be nested.

Target Lists
------------

`Goto`, `Raise`, `Except`, and `CheckedCall` support *target lists*. The
target list specifies intermediate jump targets as well as which sections
are exited. Take, for example:

.. code-block::

  goto [Leave L1, L2, Leave L3, L4]

What this means is the following:
1. leave the section (`Except` or `Finally`) identified by label L1
2. enter the `Finally` section identified by label L2
3. leave the section identified by label L3
4. land at the `Finally` or `Join` identified by label L4

An example of the code that would result in such `Goto`:

.. code-block:: nim

  block L4:
    try:
      ...
    finally:
      try:
        try
          ...
        finally:
          break L4 # this would translate to the aforementioned goto
      finally:
        ...

In the context of exceptional control-flow, the final target must be either
a `Finally` or an `Except`, otherwise it must be either a `Finally` or `Join`.

Resume
------

`Resume` is a special jump target that may only appear as the final target of
`Raise`, `CheckedCall`, and `Except`. It specifies that unwinding/exception-
handling *resumes* in the caller procedure.

Exception Handler
-----------------

`Except` represents an exception handler. If types or a local is specified,
the section is only entered if the run-time type of the active exception
matches the section's filters. If there's a match, execution continues with
the statement following the `Except`, otherwise it continues at the target
specified by the `Except`.

Finally Sections
----------------

A `Finally` section is used as an intermediate target in a jump chain. Where
the `Continue` statement marking the end of the section jumps to depends on
the *target list* the entered `Finally` is part of. For example, with
`Goto [L1, L2]`, the `Continue` of the `Finally` section identified by L1
would jump to L2.

The `Continue` must also be present if it is never actually reached. In this
case, the `Finally` section may only appear as the final target in a target
list.

Storage
=======

The MIR uses a tree-based representation similar to the AST. For easier
processing and faster access, the whole code for a procedure is stored in a
single sequence of *nodes*, with the nodes forming a tree.

Sub-trees are currently delimited via an explicit `End` node.

Constant Expressions
====================

MIR constant expression are stored separately from MIR trees representing
routine bodies. Constant expressions describe a value not depending on any
dynamic/run-time information. They use a variation/sub-set of the MIR that is
better suited for statement-less trees.

The syntax is similar to that of the normal MIR, with the biggest difference
being that the representation is flat (i.e., a single tree rather than multiple
ones).

.. code-block:: literal

  VALUE = <ProcVal>
        | <Literal>
        | COMPLEX

  ARG = Arg VALUE

  SET_CONSTR_ARG = <Literal>
                 | Range <Literal> <Literal>

  COMPLEX = SetConstr SET_CONSTR_ARG...
          | ArrayConstr ARG...
          | SeqConstr ARG...
          | TupleConstr ARG...
          | ClosureConstr ARG...
          | ObjConstr (<Field> ARG)...
          | RefConstr (<Field> ARG)...
