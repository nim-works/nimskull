=========================
Nim Experimental Features
=========================

:Authors: Andreas Rumpf
:Version: |nimversion|

.. default-role:: code
.. include:: rstcommon.rst
.. contents::


About this document
===================

This document describes features of Nim that are to be considered experimental.
Some of these are not covered by the `.experimental` pragma or
`--experimental`:option: switch because they are already behind a special syntax and
one may want to use Nim libraries using these features without using them
oneself.

.. note:: Unless otherwise indicated, these features are not to be removed,
  but refined and overhauled.


Void type
=========

The `void` type denotes the absence of any type. Parameters of
type `void` are treated as non-existent, `void` as a return type means that
the procedure does not return a value:

.. code-block:: nim

  proc nothing(x, y: void): void =
    echo "ha"

  nothing() # writes "ha" to stdout

The `void` type is particularly useful for generic code:

.. code-block:: nim

  proc callProc[T](p: proc (x: T), x: T) =
    when T is void:
      p()
    else:
      p(x)

  proc intProc(x: int) = discard
  proc emptyProc() = discard

  callProc[int](intProc, 12)
  callProc[void](emptyProc)

However, a `void` type cannot be inferred in generic code:

.. code-block:: nim

  callProc(emptyProc)
  # Error: type mismatch: got (proc ())
  # but expected one of:
  # callProc(p: proc (T), x: T)

The `void` type is only valid for parameters and return types; other symbols
cannot have the type `void`.


Unicode Operators
=================

Under the `--experimental:unicodeOperators`:option: switch,
these Unicode operators are also parsed as operators::

  ∙ ∘ × ★ ⊗ ⊘ ⊙ ⊛ ⊠ ⊡ ∩ ∧ ⊓   # same priority as * (multiplication)
  ± ⊕ ⊖ ⊞ ⊟ ∪ ∨ ⊔             # same priority as + (addition)


If enabled, Unicode operators can be combined with non-Unicode operator
symbols. The usual precedence extensions then apply, for example, `⊠=` is an
assignment like operator just like `*=` is.

No Unicode normalization step is performed.

.. note:: Due to parser limitations one **cannot** enable this feature via a
  pragma `{.experimental: "unicodeOperators".}` reliably.


Overloadable enum value names
=============================

Enabled via `{.experimental: "overloadableEnums".}`.

Enum value names are overloadable, much like routines. If both of the enums
`T` and `U` have a member named `foo`, then the identifier `foo` corresponds
to a choice between `T.foo` and `U.foo`. During overload resolution,
the correct type of `foo` is decided from the context. If the type of `foo` is
ambiguous, a static error will be produced.

.. code-block:: nim
    :test: "nim c $1"

  {.experimental: "overloadableEnums".}

  type
    E1 = enum
      value1,
      value2
    E2 = enum
      value1,
      value2 = 4

  const
    Lookuptable = [
      E1.value1: "1",
      # no need to qualify value2, known to be E1.value2
      value2: "2"
    ]

  proc p(e: E1) =
    # disambiguation in 'case' statements:
    case e
    of value1: echo "A"
    of value2: echo "B"

  p value2


Package level objects
=====================

Every Nim module resides in a (nimble) package. An object type can be attached
to the package it resides in. If that is done, the type can be referenced from
other modules as an `incomplete`:idx: object type. This feature allows to
break up recursive type dependencies across module boundaries. Incomplete
object types are always passed `byref` and can only be used in pointer like
contexts (`var/ref/ptr IncompleteObject`) in general, since the compiler does
not yet know the size of the object. To complete an incomplete object,
the `package` pragma has to be used. `package` implies `byref`.

As long as a type `T` is incomplete, no runtime type information for `T` is
available.


Example:

.. code-block:: nim

  # module A (in an arbitrary package)
  type
    Pack.SomeObject = object # declare as incomplete object of package 'Pack'
    Triple = object
      a, b, c: ref SomeObject # pointers to incomplete objects are allowed

  # Incomplete objects can be used as parameters:
  proc myproc(x: SomeObject) = discard


.. code-block:: nim

  # module B (in package "Pack")
  type
    SomeObject* {.package.} = object # Use 'package' to complete the object
      s, t: string
      x, y: int

This feature will likely be superseded in the future by support for
recursive module dependencies.


Importing private symbols
=========================

In some situations, it may be useful to import all symbols (public or private)
from a module. The syntax `import foo {.all.}` can be used to import all
symbols from the module `foo`. Note that importing private symbols is
generally not recommended.

See also the experimental `importutils <importutils.html>`_ module.


Special Operators
=================

dot operators
-------------

.. note:: Dot operators are still experimental and so need to be enabled
  via `{.experimental: "dotOperators".}`.

|NimSkull| offers a special family of dot operators that can be used to
intercept and rewrite proc call and field access attempts, referring
to previously undeclared symbol names. They can be used to provide a
fluent interface to objects lying outside the static confines of the
type system such as values from dynamic scripting languages
or dynamic file formats such as JSON or XML.

When Nim encounters an expression that cannot be resolved by the
standard overload resolution rules, the current scope will be searched
for a dot operator that can be matched against a re-written form of
the expression, where the unknown field or proc name is passed to
an `untyped` parameter:

.. code-block:: nim

  a.b # becomes `.`(a, b)
  a.b(c, d) # becomes `.`(a, b, c, d)

The matched dot operators can be symbols of any callable kind (procs,
templates and macros), depending on the desired effect:

.. code-block:: nim

  template `.`(js: PJsonNode, field: untyped): JSON = js[astToStr(field)]

  var js = parseJson("{ x: 1, y: 2}")
  echo js.x # outputs 1
  echo js.y # outputs 2

The following dot operators are available:

operator `.`
------------
This operator will be matched against both field accesses and method calls.

operator `.()`
---------------
This operator will be matched exclusively against method calls. It has higher
precedence than the `.` operator and this allows one to handle expressions like
`x.y` and `x.y()` differently if one is interfacing with a scripting language
for example.

operator `.=`
-------------
This operator will be matched against assignments to missing fields.

.. code-block:: nim

  a.b = c # becomes `.=`(a, b, c)

Call operator
-------------
The call operator, `()`, matches all kinds of unresolved calls and has lower
precedence than dot operators, and matches missing overloads for existing
routines. The experimental `callOperator` switch must be enabled to use this
operator.

.. code-block:: nim

  {.experimental: "callOperator".}

  template `()`(a: int, b: float): untyped = $(a, b)

  block:
    let a = 1.0
    let b = 2
    doAssert b(a) == `()`(b, a)
    doAssert a.b == `()`(b, a)

  block:
    let a = 1.0
    proc b(): int = 2
    doAssert not compiles(b(a))
    doAssert not compiles(a.b) # `()` not called

  block:
    let a = 1.0
    proc b(x: float): int = int(x + 1)
    let c = 3.0

    doAssert not compiles(a.b(c)) # gives a type mismatch error same as b(a, c)
    doAssert (a.b)(c) == `()`(a.b, c)


Not nil annotation
==================

**Note:** This is an experimental feature. It can be enabled with
`{.experimental: "notnil".}`.

All types for which `nil` is a valid value can be annotated with the
`not nil` annotation to exclude `nil` as a valid value:

.. code-block:: nim

  {.experimental: "notnil".}

  type
    PObject = ref TObj not nil
    TProc = (proc (x, y: int)) not nil

  proc p(x: PObject) =
    echo "not nil"

  # compiler catches this:
  p(nil)

  # and also this:
  var x: PObject
  p(x)

The compiler ensures that every code path initializes variables which contain
non-nilable pointers. The details of this analysis are still to be specified
here.

.. include:: manual_experimental_strictnotnil.rst


Aliasing restrictions in parameter passing
==========================================

.. note:: The aliasing restrictions are currently not enforced by the
  implementation and need to be fleshed out further.

"Aliasing" here means that the underlying storage locations overlap in memory
at runtime. An "output parameter" is a parameter of type `var T`,
an input parameter is any parameter that is not of type `var`.

1. Two output parameters should never be aliased.
2. An input and an output parameter should not be aliased.
3. An output parameter should never be aliased with a global or thread local
   variable referenced by the called proc.
4. An input parameter should not be aliased with a global or thread local
   variable updated by the called proc.

One problem with rules 3 and 4 is that they affect specific global or thread
local variables, but Nim's effect tracking only tracks "uses no global variable"
via `.noSideEffect`. The rules 3 and 4 can also be approximated by a different rule:

5. A global or thread local variable (or a location derived from such a location)
   can only passed to a parameter of a `.noSideEffect` proc.


Strict funcs
============

Since version 1.4, a stricter definition of "side effect" is available.
In addition to the existing rule that a side effect is calling a function
with side effects, the following rule is also enforced:

Any mutation to an object does count as a side effect if that object is reachable
via a parameter that is not declared as a `var` parameter.

For example:

.. code-block:: nim

  {.experimental: "strictFuncs".}

  type
    Node = ref object
      le, ri: Node
      data: string

  func len(n: Node): int =
    # valid: len does not have side effects
    var it = n
    while it != nil:
      inc result
      it = it.ri

  func mut(n: Node) =
    let m = n # is the statement that connected the mutation to the parameter
    m.data = "yeah" # the mutation is here
    # Error: 'mut' can have side effects
    # an object reachable from 'n' is potentially mutated


The algorithm behind this analysis is described in
the `view types section <#view-types-algorithm>`_.


View types
==========

.. tip::  `--experimental:views`:option: is more effective
  with `--experimental:strictFuncs`:option:.

A view type is a type that is or contains one of the following types:

- `lent T` (view into `T`)
- `openArray[T]` (pair of (pointer to array of `T`, size))

For example:

.. code-block:: nim

  type
    View1 = openArray[byte]
    View2 = lent string
    View3 = Table[openArray[char], int]


Exceptions to this rule are types constructed via `ptr` or `proc`.
For example, the following types are **not** view types:

.. code-block:: nim

  type
    NotView1 = proc (x: openArray[int])
    NotView2 = ptr openArray[char]
    NotView3 = ptr array[4, lent int]


The mutability aspect of a view type is not part of the type but part
of the locations it's derived from. More on this later.

A *view* is a symbol (a let, var, const, etc.) that has a view type.

Since version 1.4, Nim allows view types to be used as local variables.
This feature needs to be enabled via `{.experimental: "views".}`.

A local variable of a view type *borrows* from the locations and
it is statically enforced that the view does not outlive the location
it was borrowed from.

For example:

.. code-block:: nim

  {.experimental: "views".}

  proc take(a: openArray[int]) =
    echo a.len

  proc main(s: seq[int]) =
    var x: openArray[int] = s # 'x' is a view into 's'
    # it is checked that 'x' does not outlive 's' and
    # that 's' is not mutated.
    for i in 0 .. high(x):
      echo x[i]
    take(x)

    take(x.toOpenArray(0, 1)) # slicing remains possible
    let y = x  # create a view from a view
    take y
    # it is checked that 'y' does not outlive 'x' and
    # that 'x' is not mutated as long as 'y' lives.


  main(@[11, 22, 33])


A local variable of a view type can borrow from a location
derived from a parameter, another local variable, a global `const` or `let`
symbol or a thread-local `var` or `let`.

Let `p` the proc that is analysed for the correctness of the borrow operation.

Let `source` be one of:

- A formal parameter of `p`. Note that this does not cover parameters of
  inner procs.
- The `result` symbol of `p`.
- A local `var` or `let` or `const` of `p`. Note that this does
  not cover locals of inner procs.
- A thread-local `var` or `let`.
- A global `let` or `const`.
- A constant array/seq/object/tuple constructor.


Path expressions
----------------

A location derived from `source` is then defined as a path expression that
has `source` as the owner. A path expression `e` is defined recursively:

- `source` itself is a path expression.
- Container access like `e[i]` is a path expression.
- Tuple access `e[0]` is a path expression.
- Object field access `e.field` is a path expression.
- `system.toOpenArray(e, ...)` is a path expression.
- Pointer dereference `e[]` is a path expression.
- An address `addr e` is a path expression.
- A type conversion `T(e)` is a path expression.
- A cast expression `cast[T](e)` is a path expression.
- `f(e, ...)` is a path expression if `f`'s return type is a view type.
  Because the view can only have been borrowed from `e`, we then know
  that the owner of `f(e, ...)` is `e`.


If a view type is used as a return type, the location must borrow from a location
that is derived from the first parameter that is passed to the proc.
See `the manual <https://nim-lang.org/docs/manual.html#procedures-var-return-type>`_
for details about how this is done for `var T`.

A mutable view can borrow from a mutable location, an immutable view can borrow
from both a mutable or an immutable location.

If a view borrows from a mutable location, the view can be used to update the
location. Otherwise it cannot be used for mutations.

The *duration* of a borrow is the span of commands beginning from the assignment
to the view and ending with the last usage of the view.

For the duration of the borrow operation, no mutations to the borrowed locations
may be performed except via the view that borrowed from the
location. The borrowed location is said to be *sealed* during the borrow.

.. code-block:: nim

  {.experimental: "views".}

  type
    Obj = object
      field: string

  proc dangerous(s: var seq[Obj]) =
    let v: lent Obj = s[0] # seal 's'
    s.setLen 0  # prevented at compile-time because 's' is sealed.
    echo v.field


The scope of the view does not matter:

.. code-block:: nim

  proc valid(s: var seq[Obj]) =
    let v: lent Obj = s[0]  # begin of borrow
    echo v.field            # end of borrow
    s.setLen 0  # valid because 'v' isn't used afterwards


The analysis requires as much precision about mutations as is reasonably obtainable,
so it is more effective with the experimental `strict funcs <#strict-funcs>`_
feature. In other words `--experimental:views`:option: works better
with `--experimental:strictFuncs`:option:.

The analysis is currently control flow insensitive:

.. code-block:: nim

  proc invalid(s: var seq[Obj]) =
    let v: lent Obj = s[0]
    if false:
      s.setLen 0
    echo v.field

In this example, the compiler assumes that `s.setLen 0` invalidates the
borrow operation of `v` even though a human being can easily see that it
will never do that at runtime.


Start of a borrow
-----------------

A borrow starts with one of the following:

- The assignment of a non-view-type to a view-type.
- The assignment of a location that is derived from a local parameter
  to a view-type.


End of a borrow
---------------

A borrow operation ends with the last usage of the view variable.


Reborrows
---------

A view `v` can borrow from multiple different locations. However, the borrow
is always the full span of `v`'s lifetime and every location that is borrowed
from is sealed during `v`'s lifetime.


Algorithm
---------

The following section is an outline of the algorithm that the current implementation
uses. The algorithm performs two traversals over the AST of the procedure or global
section of code that uses a view variable. No fixpoint iterations are performed, the
complexity of the analysis is O(N) where N is the number of nodes of the AST.

The first pass over the AST computes the lifetime of each local variable based on
a notion of an "abstract time", in the implementation it's a simple integer that is
incremented for every visited node.

In the second pass, information about the underlying object "graphs" is computed.
Let `v` be a parameter or a local variable. Let `G(v)` be the graph
that `v` belongs to. A graph is defined by the set of variables that belong
to the graph. Initially for all `v`: `G(v) = {v}`. Every variable can only
be part of a single graph.

Assignments like `a = b` "connect" two variables, both variables end up in the
same graph `{a, b} = G(a) = G(b)`. Unfortunately, the pattern to look for is
much more complex than that and can involve multiple assignment targets
and sources::

  f(x, y) = g(a, b)

connects `x` and `y` to `a` and `b`: `G(x) = G(y) = G(a) = G(b) = {x, y, a, b}`.
A type based alias analysis rules out some of these combinations, for example
a `string` value cannot possibly be connected to a `seq[int]`.

A pattern like `v[] = value` or `v.field = value` marks `G(v)` as mutated.
After the second pass a set of disjoint graphs was computed.

For strict functions it is then enforced that there is no graph that is both mutated
and has an element that is an immutable parameter (that is a parameter that is not
of type `var T`).

For borrow checking, a different set of checks is performed. Let `v` be the view
and `b` the location that is borrowed from.

- The lifetime of `v` must not exceed `b`'s lifetime. Note: The lifetime of
  a parameter is the complete proc body.
- If `v` is used for a mutation, `b` must be a mutable location too.
- During `v`'s lifetime, `G(b)` can only be modified by `v` (and only if
  `v` is a mutable view).
- If `v` is `result` then `b` has to be a location derived from the first
  formal parameter or from a constant location.
- A view cannot be used for a read or a write access before it was assigned to.


Concepts
========

Concepts, also known as "user-defined type classes", are used to specify an
arbitrary set of requirements that the matched type must satisfy.

Concepts are written in the following form:

.. code-block:: nim

  type
    Comparable = concept x, y
      (x < y) is bool

    Stack[T] = concept s, var v
      s.pop() is T
      v.push(T)

      s.len is Ordinal

      for value in s:
        value is T

The concept matches if:

a) all expressions within the body can be compiled for the tested type
b) all statically evaluable boolean expressions in the body are true

The identifiers following the `concept` keyword represent instances of the
currently matched type. You can apply any of the standard type modifiers such
as `var`, `ref`, `ptr` and `static` to denote a more specific type of
instance. You can also apply the `type` modifier to create a named instance of
the type itself:

.. code-block:: nim

  type
    MyConcept = concept x, var v, ref r, ptr p, static s, type T
      ...

Within the concept body, types can appear in positions where ordinary values
and parameters are expected. This provides a more convenient way to check for
the presence of callable symbols with specific signatures:

.. code-block:: nim

  type
    OutputStream = concept var s
      s.write(string)

In order to check for symbols accepting `type` params, you must prefix
the type with the explicit `type` modifier. The named instance of the
type, following the `concept` keyword is also considered to have the
explicit modifier and will be matched only as a type.

.. code-block:: nim

  type
    # Let's imagine a user-defined casting framework with operators
    # such as `val.to(string)` and `val.to(JSonValue)`. We can test
    # for these with the following concept:
    MyCastables = concept x
      x.to(type string)
      x.to(type JSonValue)

    # Let's define a couple of concepts, known from Algebra:
    AdditiveMonoid* = concept x, y, type T
      x + y is T
      T.zero is T # require a proc such as `int.zero` or 'Position.zero'

    AdditiveGroup* = concept x, y, type T
      x is AdditiveMonoid
      -x is T
      x - y is T

Please note that the `is` operator allows one to easily verify the precise
type signatures of the required operations, but since type inference and
default parameters are still applied in the concept body, it's also possible
to describe usage protocols that do not reveal implementation details.

Much like generics, concepts are instantiated exactly once for each tested type
and any static code included within the body is executed only once.


Concept diagnostics
-------------------

By default, the compiler will report the matching errors in concepts only when
no other overload can be selected and a normal compilation error is produced.
When you need to understand why the compiler is not matching a particular
concept and, as a result, a wrong overload is selected, you can apply the
`explain` pragma to either the concept body or a particular call-site.

.. code-block:: nim

  type
    MyConcept {.explain.} = concept ...

  overloadedProc(x, y, z) {.explain.}

This will provide Hints in the compiler output either every time the concept is
not matched or only on the particular call-site.


Generic concepts and type binding rules
---------------------------------------

The concept types can be parametric just like the regular generic types:

.. code-block:: nim

  ### matrixalgo.nim

  import std/typetraits

  type
    AnyMatrix*[R, C: static int; T] = concept m, var mvar, type M
      M.ValueType is T
      M.Rows == R
      M.Cols == C

      m[int, int] is T
      mvar[int, int] = T

      type TransposedType = stripGenericParams(M)[C, R, T]

    AnySquareMatrix*[N: static int, T] = AnyMatrix[N, N, T]

    AnyTransform3D* = AnyMatrix[4, 4, float]

  proc transposed*(m: AnyMatrix): m.TransposedType =
    for r in 0 ..< m.R:
      for c in 0 ..< m.C:
        result[r, c] = m[c, r]

  proc determinant*(m: AnySquareMatrix): int =
    ...

  proc setPerspectiveProjection*(m: AnyTransform3D) =
    ...

  --------------
  ### matrix.nim

  type
    Matrix*[M, N: static int; T] = object
      data: array[M*N, T]

  proc `[]`*(M: Matrix; m, n: int): M.T =
    M.data[m * M.N + n]

  proc `[]=`*(M: var Matrix; m, n: int; v: M.T) =
    M.data[m * M.N + n] = v

  # Adapt the Matrix type to the concept's requirements
  template Rows*(M: typedesc[Matrix]): int = M.M
  template Cols*(M: typedesc[Matrix]): int = M.N
  template ValueType*(M: typedesc[Matrix]): typedesc = M.T

  -------------
  ### usage.nim

  import matrix, matrixalgo

  var
    m: Matrix[3, 3, int]
    projectionMatrix: Matrix[4, 4, float]

  echo m.transposed.determinant
  setPerspectiveProjection projectionMatrix

When the concept type is matched against a concrete type, the unbound type
parameters are inferred from the body of the concept in a way that closely
resembles the way generic parameters of callable symbols are inferred on
call sites.

Unbound types can appear both as params to calls such as `s.push(T)` and
on the right-hand side of the `is` operator in cases such as `x.pop is T`
and `x.data is seq[T]`.

Unbound static params will be inferred from expressions involving the `==`
operator and also when types dependent on them are being matched:

.. code-block:: nim

  type
    MatrixReducer[M, N: static int; T] = concept x
      x.reduce(SquareMatrix[N, T]) is array[M, int]

The Nim compiler includes a simple linear equation solver, allowing it to
infer static params in some situations where integer arithmetic is involved.

Just like in regular type classes, Nim discriminates between `bind once`
and `bind many` types when matching the concept. You can add the `distinct`
modifier to any of the otherwise inferable types to get a type that will be
matched without permanently inferring it. This may be useful when you need
to match several procs accepting the same wide class of types:

.. code-block:: nim

  type
    Enumerable[T] = concept e
      for v in e:
        v is T

  type
    MyConcept = concept o
      # this could be inferred to a type such as Enumerable[int]
      o.foo is distinct Enumerable

      # this could be inferred to a different type such as Enumerable[float]
      o.bar is distinct Enumerable

      # it's also possible to give an alias name to a `bind many` type class
      type Enum = distinct Enumerable
      o.baz is Enum

On the other hand, using `bind once` types allows you to test for equivalent
types used in multiple signatures, without actually requiring any concrete
types, thus allowing you to encode implementation-defined types:

.. code-block:: nim

  type
    MyConcept = concept x
      type T1 = auto
      x.foo(T1)
      x.bar(T1) # both procs must accept the same type

      type T2 = seq[SomeNumber]
      x.alpha(T2)
      x.omega(T2) # both procs must accept the same type
                  # and it must be a numeric sequence

As seen in the previous examples, you can refer to generic concepts such as
`Enumerable[T]` just by their short name. Much like the regular generic types,
the concept will be automatically instantiated with the bind once auto type
in the place of each missing generic param.

Please note that generic concepts such as `Enumerable[T]` can be matched
against concrete types such as `string`. Nim doesn't require the concept
type to have the same number of parameters as the type being matched.
If you wish to express a requirement towards the generic parameters of
the matched type, you can use a type mapping operator such as `genericHead`
or `stripGenericParams` within the body of the concept to obtain the
uninstantiated version of the type, which you can then try to instantiate
in any required way. For example, here is how one might define the classic
`Functor` concept from Haskell and then demonstrate that Nim's `Option[T]`
type is an instance of it:

.. code-block:: nim
    :test: "nim c $1"

  import std/[sugar, typetraits]

  type
    Functor[A] = concept f
      type MatchedGenericType = genericHead(typeof(f))
        # `f` will be a value of a type such as `Option[T]`
        # `MatchedGenericType` will become the `Option` type

      f.val is A
        # The Functor should provide a way to obtain
        # a value stored inside it

      type T = auto
      map(f, A -> T) is MatchedGenericType[T]
        # And it should provide a way to map one instance of
        # the Functor to a instance of a different type, given
        # a suitable `map` operation for the enclosed values

  import std/options
  echo Option[int] is Functor # prints true


Concept derived values
----------------------

All top level constants or types appearing within the concept body are
accessible through the dot operator in procs where the concept was successfully
matched to a concrete type:

.. code-block:: nim

  type
    DateTime = concept t1, t2, type T
      const Min = T.MinDate
      T.Now is T

      t1 < t2 is bool

      type TimeSpan = typeof(t1 - t2)
      TimeSpan * int is TimeSpan
      TimeSpan + TimeSpan is TimeSpan

      t1 + TimeSpan is T

  proc eventsJitter(events: Enumerable[DateTime]): float =
    var
      # this variable will have the inferred TimeSpan type for
      # the concrete Date-like value the proc was called with:
      averageInterval: DateTime.TimeSpan

      deviation: float
    ...


Concept refinement
------------------

When the matched type within a concept is directly tested against a different
concept, we say that the outer concept is a refinement of the inner concept and
thus it is more-specific. When both concepts are matched in a call during
overload resolution, Nim will assign a higher precedence to the most specific
one. As an alternative way of defining concept refinements, you can use the
object inheritance syntax involving the `of` keyword:

.. code-block:: nim

  type
    Graph = concept g, type G of EquallyComparable, Copyable
      type
        VertexType = G.VertexType
        EdgeType = G.EdgeType

      VertexType is Copyable
      EdgeType is Copyable

      var
        v: VertexType
        e: EdgeType

    IncidendeGraph = concept of Graph
      # symbols such as variables and types from the refined
      # concept are automatically in scope:

      g.source(e) is VertexType
      g.target(e) is VertexType

      g.outgoingEdges(v) is Enumerable[EdgeType]

    BidirectionalGraph = concept g, type G
      # The following will also turn the concept into a refinement when it
      # comes to overload resolution, but it doesn't provide the convenient
      # symbol inheritance
      g is IncidendeGraph

      g.incomingEdges(G.VertexType) is Enumerable[G.EdgeType]

  proc f(g: IncidendeGraph)
  proc f(g: BidirectionalGraph) # this one will be preferred if we pass a type
                                # matching the BidirectionalGraph concept

..
  Converter type classes
  ----------------------

  Concepts can also be used to convert a whole range of types to a single type or
  a small set of simpler types. This is achieved with a `return` statement within
  the concept body:

  .. code-block:: nim

    type
      Stringable = concept x
        $x is string
        return $x

      StringRefValue[CharType] = object
        base: ptr CharType
        len: int

      StringRef = concept x
        # the following would be an overloaded proc for cstring, string, seq and
        # other user-defined types, returning either a StringRefValue[char] or
        # StringRefValue[wchar]
        return makeStringRefValue(x)

    # the varargs param will here be converted to an array of StringRefValues
    # the proc will have only two instantiations for the two character types
    proc log(format: static string, varargs[StringRef])

    # this proc will allow char and wchar values to be mixed in
    # the same call at the cost of additional instantiations
    # the varargs param will be converted to a tuple
    proc log(format: static string, varargs[distinct StringRef])


..
  VTable types
  ------------

  Concepts allow Nim to define a great number of algorithms, using only
  static polymorphism and without erasing any type information or sacrificing
  any execution speed. But when polymorphic collections of objects are required,
  the user must use one of the provided type erasure techniques - either common
  base types or VTable types.

  VTable types are represented as "fat pointers" storing a reference to an
  object together with a reference to a table of procs implementing a set of
  required operations (the so called vtable).

  In contrast to other programming languages, the vtable in Nim is stored
  externally to the object, allowing you to create multiple different vtable
  views for the same object. Thus, the polymorphism in Nim is unbounded -
  any type can implement an unlimited number of protocols or interfaces not
  originally envisioned by the type's author.

  Any concept type can be turned into a VTable type by using the `vtref`
  or the `vtptr` compiler magics. Under the hood, these magics generate
  a converter type class, which converts the regular instances of the matching
  types to the corresponding VTable type.

  .. code-block:: nim

    type
      IntEnumerable = vtref Enumerable[int]

      MyObject = object
        enumerables: seq[IntEnumerable]
        streams: seq[OutputStream.vtref]

    proc addEnumerable(o: var MyObject, e: IntEnumerable) =
      o.enumerables.add e

    proc addStream(o: var MyObject, e: OutputStream.vtref) =
      o.streams.add e

  The procs that will be included in the vtable are derived from the concept
  body and include all proc calls for which all param types were specified as
  concrete types. All such calls should include exactly one param of the type
  matched against the concept (not necessarily in the first position), which
  will be considered the value bound to the vtable.

  Overloads will be created for all captured procs, accepting the vtable type
  in the position of the captured underlying object.

  Under these rules, it's possible to obtain a vtable type for a concept with
  unbound type parameters or one instantiated with metatypes (type classes),
  but it will include a smaller number of captured procs. A completely empty
  vtable will be reported as an error.

  The `vtref` magic produces types which can be bound to `ref` types and
  the `vtptr` magic produced types bound to `ptr` types.


..
  deepCopy
  --------
  `=deepCopy` is a builtin that is invoked whenever data is passed to
  a `spawn`'ed proc to ensure memory safety. The programmer can override its
  behaviour for a specific `ref` or `ptr` type `T`. (Later versions of the
  language may weaken this restriction.)

  The signature has to be:

  .. code-block:: nim

    proc `=deepCopy`(x: T): T

  This mechanism will be used by most data structures that support shared memory,
  like channels, to implement thread safe automatic memory management.

  The builtin `deepCopy` can even clone closures and their environments. See
  the documentation of `spawn <#parallel-amp-spawn-spawn-statement>`_ for details.


Term rewriting macros
=====================

Term rewriting macros are macros or templates that have not only
a *name* but also a *pattern* that is searched for after the semantic checking
phase of the compiler: This means they provide an easy way to enhance the
compilation pipeline with user defined optimizations:

.. code-block:: nim

  template optMul{`*`(a, 2)}(a: int): int = a + a

  let x = 3
  echo x * 2

The compiler now rewrites `x * 2` as `x + x`. The code inside the
curly brackets is the pattern to match against. The operators `*`,  `**`,
`|`, `~` have a special meaning in patterns if they are written in infix
notation, so to match verbatim against `*` the ordinary function call syntax
needs to be used.

Term rewriting macros are applied recursively, up to a limit. This means that
if the result of a term rewriting macro is eligible for another rewriting,
the compiler will try to perform it, and so on, until no more optimizations
are applicable. To avoid putting the compiler into an infinite loop, there is
a hard limit on how many times a single term rewriting macro can be applied.
Once this limit has been passed, the term rewriting macro will be ignored.

Unfortunately optimizations are hard to get right and even this tiny example
is **wrong**:

.. code-block:: nim

  template optMul{`*`(a, 2)}(a: int): int = a + a

  proc f(): int =
    echo "side effect!"
    result = 55

  echo f() * 2

We cannot duplicate 'a' if it denotes an expression that has a side effect!
Fortunately Nim supports side effect analysis:

.. code-block:: nim

  template optMul{`*`(a, 2)}(a: int{noSideEffect}): int = a + a

  proc f(): int =
    echo "side effect!"
    result = 55

  echo f() * 2 # not optimized ;-)

You can make one overload matching with a constraint and one without, and the
one with a constraint will have precedence, and so you can handle both cases
differently.

So what about `2 * a`? We should tell the compiler `*` is commutative. We
cannot really do that however as the following code only swaps arguments
blindly:

.. code-block:: nim

  template mulIsCommutative{`*`(a, b)}(a, b: int): int = b * a

What optimizers really need to do is a *canonicalization*:

.. code-block:: nim

  template canonMul{`*`(a, b)}(a: int{lit}, b: int): int = b * a

The `int{lit}` parameter pattern matches against an expression of
type `int`, but only if it's a literal.



Parameter constraints
---------------------

The `parameter constraint`:idx: expression can use the operators `|` (or),
`&` (and) and `~` (not) and the following predicates:

===================      =====================================================
Predicate                Meaning
===================      =====================================================
`atom`                   The matching node has no children.
`lit`                    The matching node is a literal like `"abc"`, `12`.
`sym`                    The matching node must be a symbol (a bound
                         identifier).
`ident`                  The matching node must be an identifier (an unbound
                         identifier).
`call`                   The matching AST must be a call/apply expression.
`lvalue`                 The matching AST must be an lvalue.
`sideeffect`             The matching AST must have a side effect.
`nosideeffect`           The matching AST must have no side effect.
`param`                  A symbol which is a parameter.
`genericparam`           A symbol which is a generic parameter.
`module`                 A symbol which is a module.
`type`                   A symbol which is a type.
`var`                    A symbol which is a variable.
`let`                    A symbol which is a `let` variable.
`const`                  A symbol which is a constant.
`result`                 The special `result` variable.
`proc`                   A symbol which is a proc.
`method`                 A symbol which is a method.
`iterator`               A symbol which is an iterator.
`converter`              A symbol which is a converter.
`macro`                  A symbol which is a macro.
`template`               A symbol which is a template.
`field`                  A symbol which is a field in a tuple or an object.
`enumfield`              A symbol which is a field in an enumeration.
`forvar`                 A for loop variable.
`label`                  A label (used in `block` statements).
`nk*`                    The matching AST must have the specified kind.
                         (Example: `nkIfStmt` denotes an `if` statement.)
`alias`                  States that the marked parameter needs to alias
                         with *some* other parameter.
`noalias`                States that *every* other parameter must not alias
                         with the marked parameter.
===================      =====================================================

Predicates that share their name with a keyword have to be escaped with
backticks.
The `alias` and `noalias` predicates refer not only to the matching AST,
but also to every other bound parameter; syntactically they need to occur after
the ordinary AST predicates:

.. code-block:: nim

  template ex{a = b + c}(a: int{noalias}, b, c: int) =
    # this transformation is only valid if 'b' and 'c' do not alias 'a':
    a = b
    inc a, c

Another example:

.. code-block:: nim

  proc somefunc(s: string)                 = assert s == "variable"
  proc somefunc(s: string{nkStrLit})       = assert s == "literal"
  proc somefunc(s: string{nkRStrLit})      = assert s == r"raw"
  proc somefunc(s: string{nkTripleStrLit}) = assert s == """triple"""
  proc somefunc(s: static[string])         = assert s == "constant"

  # Use parameter constraints to provide overloads based on both the input parameter type and form.
  var variable = "variable"
  somefunc(variable)
  const constant = "constant"
  somefunc(constant)
  somefunc("literal")
  somefunc(r"raw")
  somefunc("""triple""")


Pattern operators
-----------------

The operators `*`,  `**`, `|`, `~` have a special meaning in patterns
if they are written in infix notation.


The `|` operator
~~~~~~~~~~~~~~~~~~

The `|` operator if used as infix operator creates an ordered choice:

.. code-block:: nim

  template t{0|1}(): untyped = 3
  let a = 1
  # outputs 3:
  echo a

The matching is performed after the compiler performed some optimizations like
constant folding, so the following does not work:

.. code-block:: nim

  template t{0|1}(): untyped = 3
  # outputs 1:
  echo 1

The reason is that the compiler already transformed the 1 into "1" for
the `echo` statement. However, a term rewriting macro should not change the
semantics anyway. In fact, they can be deactivated with the `--trmacros:off`:option:
command line option or temporarily with the `trmacros` pragma.


The `{}` operator
~~~~~~~~~~~~~~~~~~~

A pattern expression can be bound to a pattern parameter via the `expr{param}`
notation:

.. code-block:: nim

  template t{(0|1|2){x}}(x: untyped): untyped = x + 1
  let a = 1
  # outputs 2:
  echo a


The `~` operator
~~~~~~~~~~~~~~~~~~

The `~` operator is the 'not' operator in patterns:

.. code-block:: nim

  template t{x = (~x){y} and (~x){z}}(x, y, z: bool) =
    x = y
    if x: x = z

  var
    a = false
    b = true
    c = false
  a = b and c
  echo a


The `*` operator
~~~~~~~~~~~~~~~~~~

The `*` operator can *flatten* a nested binary expression like `a & b & c`
to `&(a, b, c)`:

.. code-block:: nim

  var
    calls = 0

  proc `&&`(s: varargs[string]): string =
    result = s[0]
    for i in 1..len(s)-1: result.add s[i]
    inc calls

  template optConc{ `&&` * a }(a: string): untyped = &&a

  let space = " "
  echo "my" && (space & "awe" && "some " ) && "concat"

  # check that it's been optimized properly:
  doAssert calls == 1


The second operator of `*` must be a parameter; it is used to gather all the
arguments. The expression `"my" && (space & "awe" && "some " ) && "concat"`
is passed to `optConc` in `a` as a special list (of kind `nkArgList`)
which is flattened into a call expression; thus the invocation of `optConc`
produces:

.. code-block:: nim

   `&&`("my", space & "awe", "some ", "concat")


The `**` operator
~~~~~~~~~~~~~~~~~~~

The `**` is much like the `*` operator, except that it gathers not only
all the arguments, but also the matched operators in reverse polish notation:

.. code-block:: nim

  import std/macros

  type
    Matrix = object
      dummy: int

  proc `*`(a, b: Matrix): Matrix = discard
  proc `+`(a, b: Matrix): Matrix = discard
  proc `-`(a, b: Matrix): Matrix = discard
  proc `$`(a: Matrix): string = result = $a.dummy
  proc mat21(): Matrix =
    result.dummy = 21

  macro optM{ (`+`|`-`|`*`) ** a }(a: Matrix): untyped =
    echo treeRepr(a)
    result = newCall(bindSym"mat21")

  var x, y, z: Matrix

  echo x + y * z - x

This passes the expression `x + y * z - x` to the `optM` macro as
an `nnkArgList` node containing::

  Arglist
    Sym "x"
    Sym "y"
    Sym "z"
    Sym "*"
    Sym "+"
    Sym "x"
    Sym "-"

(This is the reverse polish notation of `x + y * z - x`.)


Parameters
----------

Parameters in a pattern are type checked in the matching process. If a
parameter is of the type `varargs`, it is treated specially and can match
0 or more arguments in the AST to be matched against:

.. code-block:: nim

  template optWrite{
    write(f, x)
    ((write|writeLine){w})(f, y)
  }(x, y: varargs[untyped], f: File, w: untyped) =
    w(f, x, y)


noRewrite pragma
----------------

Term rewriting macros and templates are currently greedy and
they will rewrite as long as there is a match.
There was no way to ensure some rewrite happens only once,
e.g. when rewriting term to same term plus extra content.

`noRewrite` pragma can actually prevent further rewriting on marked code,
e.g. with given example `echo("ab")` will be rewritten just once:

.. code-block:: nim

  template pwnEcho{echo(x)}(x: untyped) =
    {.noRewrite.}: echo("pwned!")

  echo "ab"

`noRewrite` pragma can be useful to control term-rewriting macros recursion.



Example: Partial evaluation
---------------------------

The following example shows how some simple partial evaluation can be
implemented with term rewriting:

.. code-block:: nim

  proc p(x, y: int; cond: bool): int =
    result = if cond: x + y else: x - y

  template optP1{p(x, y, true)}(x, y: untyped): untyped = x + y
  template optP2{p(x, y, false)}(x, y: untyped): untyped = x - y


Example: Hoisting
-----------------

The following example shows how some form of hoisting can be implemented:

.. code-block:: nim

  import std/pegs

  template optPeg{peg(pattern)}(pattern: string{lit}): Peg =
    var gl {.global, gensym.} = peg(pattern)
    gl

  for i in 0 .. 3:
    echo match("(a b c)", peg"'(' @ ')'")
    echo match("W_HI_Le", peg"\y 'while'")

The `optPeg` template optimizes the case of a peg constructor with a string
literal, so that the pattern will only be parsed once at program startup and
stored in a global `gl` which is then re-used. This optimization is called
hoisting because it is comparable to classical loop hoisting.


AST based overloading
=====================

Parameter constraints can also be used for ordinary routine parameters; these
constraints then affect ordinary overloading resolution:

.. code-block:: nim

  proc optLit(a: string{lit|`const`}) =
    echo "string literal"
  proc optLit(a: string) =
    echo "no string literal"

  const
    constant = "abc"

  var
    variable = "xyz"

  optLit("literal")
  optLit(constant)
  optLit(variable)

However, the constraints `alias` and `noalias` are not available in
ordinary routines.


Lock levels
===========

Lock levels are used to enforce a global locking order in order to detect
potential deadlocks during semantic analysis. A lock level is an constant
integer in the range 0..1_000. Lock level 0 means that no lock is acquired at
all.

If a section of code holds a lock of level `M`, it can also acquire any
lock of level `N < M`. Another lock of level `M` cannot be acquired. Locks
of the same level can only be acquired *at the same time* within a
single `locks` section:

.. code-block:: nim

  var a, b: TLock[2]
  var x: TLock[1]
  # invalid locking order: TLock[1] cannot be acquired before TLock[2]:
  {.locks: [x].}:
    {.locks: [a].}:
      ...
  # valid locking order: TLock[2] acquired before TLock[1]:
  {.locks: [a].}:
    {.locks: [x].}:
      ...

  # invalid locking order: TLock[2] acquired before TLock[2]:
  {.locks: [a].}:
    {.locks: [b].}:
      ...

  # valid locking order, locks of the same level acquired at the same time:
  {.locks: [a, b].}:
    ...


Here is how a typical multilock statement can be implemented in Nim. Note how
the runtime check is required to ensure a global ordering for two locks `a`
and `b` of the same lock level:

.. code-block:: nim

  template multilock(a, b: ptr TLock; body: untyped) =
    if cast[ByteAddress](a) < cast[ByteAddress](b):
      pthread_mutex_lock(a)
      pthread_mutex_lock(b)
    else:
      pthread_mutex_lock(b)
      pthread_mutex_lock(a)
    {.locks: [a, b].}:
      try:
        body
      finally:
        pthread_mutex_unlock(a)
        pthread_mutex_unlock(b)


Whole routines can also be annotated with a `locks` pragma that takes a lock
level. This then means that the routine may acquire locks of up to this level.
This is essential so that procs can be called within a `locks` section:

.. code-block:: nim

  proc p() {.locks: 3.} = discard

  var a: TLock[4]
  {.locks: [a].}:
    # p's locklevel (3) is strictly less than a's (4) so the call is allowed:
    p()


As usual, `locks` is an inferred effect and there is a subtype
relation: `proc () {.locks: N.}` is a subtype of `proc () {.locks: M.}`
iff (M <= N).

The `locks` pragma can also take the special value `"unknown"`. This
is useful in the context of dynamic method dispatching. In the following
example, the compiler can infer a lock level of 0 for the `base` case.
However, one of the overloaded methods calls a procvar which is
potentially locking. Thus, the lock level of calling `g.testMethod`
cannot be inferred statically, leading to compiler warnings. By using
`{.locks: "unknown".}`, the base method can be marked explicitly as
having unknown lock level as well:

.. code-block:: nim

  type SomeBase* = ref object of RootObj
  type SomeDerived* = ref object of SomeBase
    memberProc*: proc ()

  method testMethod(g: SomeBase) {.base, locks: "unknown".} = discard
  method testMethod(g: SomeDerived) =
    if g.memberProc != nil:
      g.memberProc()

This feature may be removed in the future due to its practical difficulties.
