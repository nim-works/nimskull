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
        | <Proc>
        | <Type>
        | LVALUE

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
            | Magic <Magic> CALL_ARG ... # a call of a magic procedure (i.e.,
                                         # a procedure that is either going to
                                         # be lowered into something else, or
                                         # one of which of which the behaviour
                                         # cannot be represented in the MIR)

  RVALUE = CALL_EXPR
         | Constr   CONSTR_ARG ...       # construct a tuple, closure, set, or
         | ObjConstr (<Field> CONSTR_ARG) ... # construct an `object` or
                                         # `ref object`
                                         # array
         | StdConv  VALUE                # number conversion or conversion
                                         # between cstring and string
         | Conv     VALUE                # same as `StdConv`. Only duplicate
                                         # for legacy code generator reasons
         | Cast     VALUE                # reinterpret the value as a different
                                         # type
         | Addr     LVALUE               # create a pointer from the lvalue
         | View     LVALUE               # create a view (`var`/`lent`) of the
                                         # lvalue
         | ToSlice  VALUE                # create an `openArray` slice of
                                         # the full sequence

  FULL_VALUE = RVALUE | VALUE

  STATEMENT =
            | StmtList STATEMENT ...    # a list of statements
            | Scope STATEMENT           # wrap the statement in a scope, which
                                        # delimits the lifetime of all
                                        # definitions within
            | Def NAME none             # definition
            | Def NAME FULL_VALUE       # definition + initial value assignment
            | DefCursor NAME            # definition of non-owning location
            | DefCursor NAME FULL_VALUE # same as above, but with initial
                                        # assignment
            | Bind <Alias> LVALUE       # bind the lvalue to the given alias
            | BindMut <Alias> LVALUE    # bind the lvalue to the given alias.
                                        # The alias may be used for mutations
                                        # (e.g., on the left of assignments)
            | Void LVALUE               # evaluates the lvalue for side-effects
                                        # and acts as a usage of the lvalue
                                        # during data-flow analysis
            | Void CALL_EXPR            # represents a void call. The called
                                        # procedure or magic *must* have a
                                        # `void`` return type
            | Asgn LVALUE FULL_VALUE    # normal assignment of the right value
                                        # to the left location
            | Init LVALUE FULL_VALUE    # initial assignment (the destination
                                        # is empty)
            | FastAsgn LVALUE FULL_VALUE# fast assignment (cannot be rewritten
                                        # into a full copy)
            | Switch LVALUE FULL_VALUE  # changes the active branch of a
                                        # variant. Unclear semantics.
            | If VALUE STATEMENT        # if the value evaluates to true
                                        # execute the statement
            | Case VALUE BRANCH_LIST    # dispatch to one of the branches based
                                        # on the value, where value must be
                                        # either of integer, float, or string
                                        # type
            | Block <Label> STATEMENT   # run the wrapped statement and provide
                                        # a named exit. The label must be
                                        # unique across all blocks in the
                                        # procedure
            | Break <Label>             # exit the enclosing block that has the
                                        # given label
            | Repeat STATEMENT          # unconditional loop. Repeat the
                                        # statement for an indefinite amount
                                        # of times
            | TRY_STMT
            | Raise LVALUE              # push the given exception to the
                                        # exception stack and start exceptional
                                        # control-flow. The `ref object` is
                                        # consumed
            | Raise <None>              # re-raise the current exception
            | Return                    # exit the procedure, but execute all
                                        # enclosing finalizers first (from
                                        # innermost to outermost)
            | Emit VALUE ...
            | Asm VALUE ...

  BRANCH_LIST = (Branch <Literal> ... STATEMENT) ... # a list of branches
  HANDLER = Except (Branch <Type> ... STATEMENT) ... # exception handler

  TRY_STMT = Try STATEMENT (HANDLER)? (Finally STATEMENT)?
    # if a handler is present, all `raise` statements within the tried
    # statement are redirected to the handler. If a finalizer is present, all
    # control-flow exiting the tried statement or handler is first redirected
    # to the finalizer.

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

Storage
=======

The MIR uses a tree-based representation similar to the AST. For easier
processing and faster access, the whole code for a procedure is stored in a
single sequence of *nodes*, with the nodes forming a tree.

Sub-trees are currently delimited via an explicit `End` node.