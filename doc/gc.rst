=======================
Nim's Memory Management
=======================

.. default-role:: code
.. include:: rstcommon.rst

:Author: Andreas Rumpf
:Version: |nimversion|

..


  "The road to hell is paved with good intentions."


Introduction
============

A memory-management algorithm optimal for every use-case cannot exist.
|NimSkull| currently focuses on automatic reference counting, but this might
change or shift in the future. This document describes how the management
strategies work.

.. note:: the default GC is incremental, thread-local and not "stop-the-world"

Multi-paradigm Memory Management Strategies
===========================================

.. default-role:: option

To choose the memory management strategy use the `--gc:` switch. Note that the
switch is currently ignored when using the JavaScript or VM backends.

--gc:arc    Plain reference counting with
  `move semantic optimizations <destructors.html#move-semantics>`_, offers a shared heap.
  It offers deterministic performance for `hard realtime`:idx: systems. Reference cycles
  cause memory leaks, beware.

--gc:orc    Same as `--gc:arc` but adds a cycle collector based on "trial deletion".
  Unfortunately, that makes its performance profile hard to reason about so it is less
  useful for hard real-time systems.


================== ======== ================= ============== ===================
Memory Management  Heap     Reference Cycles  Stop-The-World Command line switch
================== ======== ================= ============== ===================
ARC                Shared   Leak              No             `--gc:arc`
ORC                Shared   Cycle Collector   No             `--gc:orc`
================== ======== ================= ============== ===================

.. default-role:: code
.. include:: rstcommon.rst

JavaScript's garbage collector is used for the `JavaScript and NodeJS
<backends.html#backends-the-javascript-target>`_ compilation targets.
The `NimScript <nims.html>`_ and VM targets use their own custom garbage
collector, which, as of now, leaks reference cycles.

ARC
===

ARC, which is short for automatic reference counting, is a memory-management
strategy based on plain reference counting. Each managed heap cell (that is,
a location allocated via either `new` or by using the object construction syntax
where the type is a `ref T`) also stores a reference counter.

When a managed cell is allocated, its counter starts at `1` (because the
allocation created a single `ref` handle). Creating a `ref` to the cell
increments the counter, and destroying one decrements it.

Once the counter reaches zero, the cell is immediately destroyed (via calling
the attached destructor, if available) and the underlying memory returned to
the allocator.

In |NimSkull|, the counting is implemented via the lifetime-tracking-hook
mechanism. When copying a `ref` (i.e. its `=copy` hook is invoked), the
`ref` value itself (which is a pointer) is copied and the referenced cell's
refcounter is incremented by one. Moving a `ref` only copies the pointer
value, and destroying a `ref` (i.e. its `=destroy` is invoked) decrements the
refcounter of the referenced cell by one.

The `=destroy` hook for a `ref` checks the refcounter of the cell, and if it
just reached zero, the static or virtual destructor of the cell is invoked and
the underlying memory freed.

As `ref`s use the lifetime-tracking hooks, they're subject to the same
optimizations (i.e. cursor inference, turning copies into moves) as all other
types with lifetime-tracking hooks.

ORC
===

The problem with plain reference counting is that it is not able to handle
reference cycles. A reference cycle exists when a cell either directly or
indirectly stores a reference (i.e. `ref`) to itself. Example:

.. code-block:: nim
  type A = object
    x: ref A

  var a = A(x: nil) # okay; no cycle exists and the refcounter is 1
  a.x = a # a cycle is introduced! the refcounter is 2

  # once `a` gets destroyed, the refcounter becomes 1, but it's not possible
  # for it to reach 0 from there
  a.x = nil # explicitly breaking the cycle would work

  type
    B = object
      c: ref C
    C = object
      b: ref B

  var b = B()
  b.c = C() # okay; no cycle exists
  b.c.b = b # an indirect cycle is introduced!

This is where ORC becomes relevant. ORC is automatic reference-counting with a
run-time cycle collector (the letter 'O' in ORC is meant to represent a cycle).

For easier visualization, it makes sense to view a managed heap cell as a *node*,
and a `ref` as an *edge* in a directed cyclic graph. The cycle collector is
responsible for freeing cells only referenced as part of references cycles.

The Algorithm
-------------

The cycle collector used for ORC is based on "trial deletion". As the first
step, all edges reachable from potential *cycle roots* are temporarily
deleted. The second step restores the outgoing edges for nodes (and nodes
reachable from them) that still have incoming edges after the temporary
deletion. Once done, all nodes that have no incoming edges are known to only
be alive because of a reference cycle, and can now be freed.

The Implementation
------------------

Collection is implemented with a 2-pass colouring algorithm for detection, and
a 2-step memory collection process. In the following text, the terms "Cell" and
"Node" are used interchangeably and refer to the same thing.

Each cell is assigned a *color*:
- *black*: the cell is alive (new cells start with this color)
- *gray*: the cell was visited by the collector already
- *white*: the cell is dead and can be cleaned up

.. note:: The cycle collector only considers *edges* and *nodes* through which
  a cycle is possible, all others are ignored and never touched.

The first pass traverses all *black* nodes reachable from *black* nodes in the
input set, assigns the color *gray* to them, and temporarily decrements the
refcounter of each cell connected to a now *gray* cell by one (i.e. removing
the outgoing edges). *Gray* nodes are not traversed further -- they've
already been visited.

The second pass traverses the sub-graph of each cell in the input set and
forward-propagates the color *black* from all nodes with the color *gray* and
a refcount > 0 -- they are being kept alive by something not part of a cycle.
The others are marked as *white*.

When coloring a node *black* again, the refcounter of each connected cell is
incremented by one -- this undoes the decrement that happened during the first
pass.

.. note:: the second pass is skipped if all cells marked as *gray* have no
  incoming edges left

The collector then traverses the input sub-graphs one last time, gathering all
cells with either *white* or *gray* as the color into a list. On adding a cell
to the "to-be-freed" list, its outgoing edges relevant to the cycle collector
are physically removed (by setting the `ref` values to `nil`) -- this is
necessary so that the following normal cleanup doesn't attempt to touch the
referenced cells.

Finally, all cells in the "to-be-freed" list are disposed by first invoking
their destructor and then freeing the underlying memory location.

Input Set
~~~~~~~~~

Which cells the input set contains depends on the what type of collection is
run. If it's a full collection, the input set contains all potential *cycle
roots*, but for a partial collection (i.e. started by calling
`GC_partialCollect`), only the roots with an index greater than or equal to
the specified `limit` are considered.

.. note:: how the `limit` parameter works is somewhat confusing, and likely
  going to change in the future.

Potential Cycle Roots
~~~~~~~~~~~~~~~~~~~~~

A central part of ORC is registering potential *cycle roots*. If static
analysis of a `ref`'s type yields that no cycles are possible through it,
it is never treated as an edge by the collector.

When an edge through which a reference cycle can happen is removed (i.e. a
`ref` is destroyed), the cell is remembered as a potential *cycle root*. Doing
this on edge removal instead of creation has the benefit that sub-graphs that
are definitely kept alive from outside a cycle are not already scanned during
the first two passes.

Once the list of potential *cycle roots* reaches a certain threshold, a full
cycle collection is immediately run (if cycle collection is not disabled at
program run-time, that is). Since roots are only registered when a `ref` value
(that can form a reference cycle) is destroyed, automatic cycle collection can
only happen when copying, sinking, or destroying a `ref`.

The threshold is dynamic (but can be made static via the `nimFixedOrc` define).
If more than 50% of the visited cells were freed during a *full* collection,
the threshold is reset to the default value -- otherwise it's increased until
an implementation defined upper bound is reached.

After a full or partial cycle collection, all processed potential *cycle roots*
are removed from the list -- they were either freed, not really part of a cycle,
or part of a cycle but kept alive from a root that wasn't processed.

Gathering Edges
~~~~~~~~~~~~~~~

To know about the outgoing edges of a cell, the cell's attached `=trace` hook
is invoked. The hook is responsible for collecting all directly reachable
relevant `ref`s of the cell to a list provided by the collector.

For more information, see the `=trace` hook `documentation
<destructors.html#lifetimeminustracking-hooks-nimeqtrace-hook>`_

Thread Safety
~~~~~~~~~~~~~

Neither the cycle collector nor its API are thread-safe. Only a single thread
may ever perform cycle collection, or, in other words, use `ref`s that can be
potentially part of a reference cycle.

Reading or modifying a managed heap cell that can be part of a reference cycle
through a `ptr` from a different thread than the one that registered it as a
potential *cycle root* is unsafe and can lead to memory corruption issues.

Tweaking ORC
------------

The operation of ORC can be configured at both compile- and run-time.

Compile-time configuration:

(1) `--define:nimFixedOrc`:option:

    Use an implementation-defined static *cycle root* threshold.

Run-time configuration:

To disable the cycle collector, `GC_disableOrc` is used. When the collector is
disabled, potential *cycle roots* will accumulate. To enable it again, call
`GC_enableOrc`.

Cycle collection can be manually triggered via calling either `GC_runOrc` (full
collection) or `GC_partialCollect(limit)` (partial collect). Manually
triggering a cycle collection while the cycle collector is disabled is
possible, but note that doing so (currently) enables the collector again.

Keeping track of memory
=======================

If you need to pass around memory allocated by Nim to C, you can use the
procs `GC_ref` and `GC_unref` to mark objects as referenced to avoid them
being freed by the garbage collector.
Other useful procs from `system <system.html>`_ you can use to keep track of memory are:

* `getTotalMem()` Returns the amount of total memory managed by the garbage collector.
* `getOccupiedMem()` Bytes reserved by the garbage collector and used by objects.
* `getFreeMem()` Bytes reserved by the garbage collector and not in use.
* `GC_getStatistics()` Garbage collector statistics as a human-readable string.

These numbers are usually only for the running thread, not for the whole heap,
with the exception of `--gc:boehm`:option: and `--gc:go`:option:.

In addition to `GC_ref` and `GC_unref` you can avoid the garbage collector by manually
allocating memory with procs like `alloc`, `alloc0`, `allocShared`, `allocShared0` or `allocCStringArray`.
The garbage collector won't try to free them, you need to call their respective *dealloc* pairs
(`dealloc`, `deallocShared`, `deallocCStringArray`, etc)
when you are done with them or they will leak.


Heap dump
=========

The heap dump feature is still in its infancy, but it already proved
useful for us, so it might be useful for you. To get a heap dump, compile
with `-d:nimTypeNames`:option: and call `dumpNumberOfInstances`
at a strategic place in your program.
This produces a list of the used types in your program and for every type
the total amount of object instances for this type as well as the total
amount of bytes these instances take up.

The numbers count the number of objects in all garbage collector heaps, they refer to
all running threads, not only to the current thread. (The current thread
would be the thread that calls `dumpNumberOfInstances`.) This might
change in later versions.

.. note:: this feature isn't supported at the moment, but will be again in the future
