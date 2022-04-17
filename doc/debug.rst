.. include:: rstcommon.rst

========================
Compiler Debugging Guide
========================

.. raw:: html

  <blockquote><p>
  The process of identifying and removing errors from computer hardware or software.
  </p></blockquote>

How to debug different subsystems of the compiler using built-in tooling.

Each of these approaches requires building a debug or temporary compiler.
Building a debug or temporary compiler is covered in the Internals
documentation under the section `Developing the compiler<intern.html>`_. Some
might require additional defines as part of building the compiler and are
detailed in the relevant sections below.

The remaining sections of this document describes how to debug various aspects
of the compiler.


Debugging Defines - Quick Reference
-----------------------------------

(if you're new to compiler debugging skip this section to the next heading)

For those familiar with debugging the compiler below are quick reference tables
for the various defines involved. Those new to debugging the compiler you can
skim or skip these and see the following sections for details what they are and
how to use them.

**Used when compiling the compiler itself**

=========================== =======
Define                      Enables
--------------------------- -------
`nimDebugUtils`             Allows for semantic analysis execution tracing and more
`nimDebugUnreportedErrors`  Enable unreported error debugging
`nimVMDebugExecute`         Print out every instruction executed by the VM
`nimVMDebugGenerate`        List VM code generated for every procedure called at compile-time
=========================== =======

**Used when executing the compiler**

(most of these require a compiler built with `nimDebugUtils`)
============================ =======
Define                       Enables
---------------------------- -------
`nimCompilerDebug`           Reports for localized piece of the user code
`nimCompilerDebugCalltrace`  Call trace reports
`nimCompilerDebugTraceDir`   Writes call traces to the specified directory
============================ =======


Simplest / All Apsects - Debug Helper Modules
=============================================

`astrepr
<https://nim-works.github.io/nimskull/compiler/utils/astrepr.html>`_ module
provides a collection of useful procedures for printing internal
representation types (``PSym``, ``PType`` and ``PNode``) in a readable
manner. For more documentation see the module itself.

This is the simplest approach a bit better than `echo` based debugging.
Use the exported procs from the module, build the compiler and look at the
output.


Semantic Analysis - Execution Tracing
=====================================

This creates a call trace of semantic analysis functions and procedures that
were invoked when compiler some code. This is useful for debugging how the
compiler is interpreting a fragment of code.

Quick start:
* Ensure the compiler is built with `nimDebugUtils` defined.
* wrap a region of user code with a define/undefine `nimCompilerDebug`
  * or use the `system.nimCompilerDebugRegion`
* compile the user code with the define `nimCompilerDebugCalltrace`
* watch the spam


Execution Tracing a Fragment
----------------------------

Using a compiler built with `nimDebugUtils` defined, compile your test file
with `nim c -d:nimCompilerDebugCalltrace --filenames:canonical file.nim`:cmd:
and annotate code block with the `nimCompilerDebug` wrapper.

.. code-block:: nim

    {.define(nimCompilerDebug).}
    vmTarget(Obj())
    {.undef(nimCompilerDebug).}

You will get all the call entries traced

.. code-block:: literal

    >>] trace start
    #0]> semExpr @ sem/semexprs.nim(2948, 21) from sem/semstmts.nim(2446, 1)
    #1]  > semOverloadedCallAnalyseEffects @ sem/semexprs.nim(941, 21) from sem/semexprs.nim(1133, 1)
    #2]    > semOverloadedCall @ sem/semcall.nim(569, 21) from sem/semexprs.nim(950, 1)
    #3]      > semExpr @ sem/semexprs.nim(2948, 21) from sem/semexprs.nim(43, 1)
    #4]        > semTypeNode @ sem/semtypes.nim(1903, 21) from sem/semobjconstr.nim(469, 1)
    #4]        < semTypeNode @ sem/semtypes.nim(1903, 21) from sem/semobjconstr.nim(469, 1)
    #3]      < semExpr @ sem/semexprs.nim(2948, 21) from sem/semexprs.nim(43, 1)
    #2]    < semOverloadedCall @ sem/semcall.nim(569, 21) from sem/semexprs.nim(950, 1)
    #1]  < semOverloadedCallAnalyseEffects @ sem/semexprs.nim(941, 21) from sem/semexprs.nim(1133, 1)
    #0]< semExpr @ sem/semexprs.nim(2948, 21) from sem/semstmts.nim(2446, 1)
    #0]> semExpr @ sem/semexprs.nim(2948, 21) from sem/semstmts.nim(2446, 1)
    <<] trace end


Anatomy of a Trace Entry
------------------------

Reports are formatted in the `cli_reporter.nim` as well (all debug reports
are also transferred using regular reporting pipeline). It has a lot of
information, but general parts for each call parts are:

.. code-block:: literal

   #2]    < semOverloadedCall @ sem/semcall.nim(569, 21) from sem/semexprs.nim(950, 1)
   ^      ^ ^                   ^                             ^
   |      | |                   |                             Where proc has been called from
   |      | |                   Location of the `addInNimDebugUtils()` - the proc itsemf
   |      | Name of the proc
   |      Whether proc has been entered or exited
   Depth of the traced call tree


Missing Trace Entries
---------------------

Execution tracing is implemented by adding a template to each routine in the
compiler manually (gasp!). Thankfully they're easy to add and quick win
contributions making coverage easy to grow.

To add traces for a missed compiler routine insert a call to one of the
`addInNimDebugUtils()` overloads from `utils.debugutils` at the start of said
routine. See the snippet below to show how tracing was added to
`sem.semOverloadedCall`:

.. code-block:: nim

    proc semOverloadedCall(c: PContext, n, nOrig: PNode,
                           filter: TSymKinds, flags: TExprFlags): PNode {.nosinks.} =
      addInNimDebugUtils(c.config, "semOverloadedCall")
      # Rest of the code as before ...

Comparing/Logging Traces: Differential Debugging
---------------------------------------------------

If test compiler runs with `-d:nimCompilerDebugTraceDir=/some/dir` option
the reports stored between different sections are written into separate
files in this directory. This is helpful if you want to track bugs where
you have two pieces or versions of code where one works and the other
doesn't. Collect traces for both and compare them to see how the compiler
analysis differs between them and use that to guide your investigation.
This technique is called differential debugging or sometimes differential
diagnosis.

For example, `semchecked ast passed down as untyped macro argument #193
<https://github.com/nim-works/nimskull/issues/193>`_ has two distinct cases
(`foo1` and `foo3`) whose only difference is the presence of the `let`
symbol. We can clean up the example code a little (leaving only two
examples that we plan to compare):

.. code-block:: nim

    macro check(args: varargs[untyped]): untyped = discard
    # Removed the import and `treeRepr()` call because they are not
    # necessary in this case - we will see all the processed data
    # in the debug trace.

    proc foo1() =
      let check = 123
      var a = 1
      var b = 1
      {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
      check(a == b)
      {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

    proc foo3() =
      var a = 1
      var b = 1
      # this is what it should be
      {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
      check(a == b)
      {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

If we compiled the code with the following command (`/tmp/nimtrace` can any
other target directory)

.. code-block:: cmd

    nim c --filenames:canonical -d:nimCompilerDebugTraceDir=/tmp/nimtrace -d:nimDebugUtils file.nim

We will get two large files - `/tmp/nimtrace/0` and `/tmp/nimtrace/1` that
contain debug trace for the first and second sections respectively. The
files are quite large, so they are not included here, but the most notable
part are (again, for the first and second group respectively):

.. code-block:: literal

    #0]> semExpr @ sem/semexprs.nim(2948, 21) from sem/semstmts.nim(2446, 1)
          kind:stepNodeFlagsToNode
          from flags: {}
          from node:
            Call
            0 Ident check
            1 Infix
              0 Ident ==
              1 Ident a
              2 Ident b
    #1]  > semIndirectOp @ sem/semexprs.nim(1025, 21) from sem/semexprs.nim(3095, 1)


.. code-block:: literal

    #0]> semExpr @ sem/semexprs.nim(2948, 21) from sem/semstmts.nim(2446, 1)
          kind:stepNodeFlagsToNode
          from flags: {}
          from node:
            Call
            0 Ident check
            1 Infix
              0 Ident ==
              1 Ident a
              2 Ident b
    #1]  > semOverloadedCallAnalyseEffects @ sem/semexprs.nim(941, 21) from sem/semexprs.nim(1133, 1)

As you can see, the input ASTs are identical - `check(a == b)`, call, infix
and so on. We need to find out where exactly the calltree diverges, that is
most likely going to be a location of the bug. Second group's
`semOverloadedCall` generates correct results, so we most likely need to
look into the `semIndirectOp` call here, because it does not make any
sense. It calls `from sem/semexprs.nim(3095, 1)`. If we go into the file
and look at the surrounding code we will see that call to `semDirectOp` is
really there. `line 3070
<https://github.com/nim-works/nimskull/blob/eaf1e8ac8a/compiler/sem/semexprs.nim#L3070>`_
contains call to the `qualifiedLookUp2` call, that should've returned a
`skMacro` symbol there (branch on line `3077`), but this does not happen in
the first case. This means we need to add the tracer call to the lookup
implementation.

.. code-block:: nim

    c.config.addInNimDebugUtils("qualifiedLookup2", n, result)

This change results in the two more entries added to the call trace:

.. code-block:: nim

    #1]  > qualifiedLookup2 @ sem/lookups.nim(732, 11) from sem/semexprs.nim(3070, 1)
             kind:stepNodeToSym
             from node:
               Ident check
    #1]  < qualifiedLookup2 @ sem/lookups.nim(732, 11) from sem/semexprs.nim(3070, 1)
             kind:stepNodeToSym
             to sym:
               Let
                 typ:   Int sk:Type
                 owner:  kind:skProc name:foo1

.. code-block:: nim

    #1]  > qualifiedLookup2 @ sem/lookups.nim(732, 11) from sem/semexprs.nim(3070, 1)
             kind:stepNodeToSym
             from node:
               Ident check
    #1]  < qualifiedLookup2 @ sem/lookups.nim(732, 11) from sem/semexprs.nim(3070, 1)
             kind:stepNodeToSym
             to sym:
               Macro
                 flags: {Used, Global}
                 offset:2
                 typ:   Proc (args):
                 owner:  kind:skModule name:file


As you can see now, the return values of this procedure are different here.
We have successfully localized the bug from 'whole compiler' to a specific
procedure.


-------

.. tip::

   It is perfeclty fine to add new debug trace options or remove some of
   the existing ones if you think it is necessary for you to better
   understand what is going on in the compiler.

.. tip::

    Sometimes it is necessary to print processed nodes for a specific part
    of the compiler procedure, but it runs multiple times on the code that
    does not show any issues, clobbering the final output and making it
    harder to figure out what was wrong.

    In that case you can wrap problematic (input) code with
    `{.define(nimCompilerDebug).}` and `{.undef(nimCompilerDebug).}`
    sections, and then check for the symbols definition using
    `debugutils.isCompilerDebug`, to filter out unnecessary noise.


    .. code-block:: nim

        if c.config.isCompilerDebug():
          # Check if we are in the `{.defined(nimCompilerDebug).}` section

          # Call any debugging logic you can think of
          echo c.config.treeRepr(result, maxPath = 1)

    Alternatively, if you need to filter out node on the file/line location
    basis, you can use `inFile` and `inLines` procs defined in the
    `astrepr`. Note that both of them are only for debug purposes, and
    should not stay in the final code.


.. warning::

     Specific details of processing for modules in presence of multiple
     compile-time statements (such as `{.define().}`) is yet to be properly
     specified and tested, so when used for **cross-module** semantic
     issues (god help you if you ever find yourself facing something like
     this) it might inhibit unexpected behavior.


VM Codegen and Execution
========================

VM code generation prints all of the generated procedures. If this is not
needed (which would be the majority of use cases) you can add
`--define:expandVmListing=vmTarget`:option: to print only the code
generated for this proc. For example (generated listing might not match
exactly)

.. code-block:: nim

    type
      Obj = object
        charField: char
        intField: int

    proc vmTarget(arg: Obj) =
      echo arg.charField.int + arg.intField

    static:
      vmTarget(Obj())


.. code-block:: cmd

  nim c --filenames:canonical --define:expandVmListing=vmTarget file.nim


.. code-block:: literal

    Code listing for the 'vmTarget' file.nim(6, 6)

      LdConst      r3     $     1279                system.nim(2005, 30)
      LdObj        r6     r1     r0                 file.nim(7, 11)
      NodeToReg    r5     r6     r0                 file.nim(7, 11)
      Conv         r6     r5     int   char         file.nim(7, 21)
      LdObj        r7     r1     r1                 file.nim(7, 31)
      NodeToReg    r5     r7     r0                 file.nim(7, 31)
      AddInt       r4     r6     r5                 file.nim(7, 26)
      IndCallAsgn  r2     r3     #2                 file.nim(7, 26)
      Echo         r2     r1     r0                 file.nim(7, 26)
      Ret          r0     r0     r0                 file.nim(7, 8)
      Eof          r0     r0     r0                 file.nim(7, 8)
