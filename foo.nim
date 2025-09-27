when true:
  type
    Foo {.inheritable.} = object
    Bar = object of Foo

  proc `=destroy`(a: var Foo) = echo "Foo"
  proc `=destroy`(a: var Bar) = echo "Bar"; `=destroy`(Foo(a))

  let b = Bar()

when false:
  template foo(a: sink untyped) =
    echo a

  foo("test")

when false:
  proc f(a: varargs[string, `$`]) =
    for n in a.items:
      doAssert n is string
  
  f(1, 2, false, "test")

when false:
  import std/macros

  var a = 10
  proc foo(a: varargs[int], b: varargs[bool]) =
    echo "a: ", a, " b: ", b

  macro bar(fstmt: typed) =
    let f = fstmt[0]
    echo treeRepr f
    result = newCall(f[0], ident("a"), f[2])

  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  foo(1, 2, 3, false)
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

  bar:
    foo(1, 2, 3, false)  # echos a: 10 b: false

when false:
  import std/strformat

  proc exec(cmd: string, args: varargs[string]) =
    let x = @[cmd] & args
    raise newException(CatchableError):
      fmt"{x}"
    # Only referring to a "broken" variable would this issue happen

when false:
  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  type
    Tile3 = Tile2
    Tile2 = Tile
    Tile[n] = object
      a: n
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

  var a: Tile3[int]

  block: # Ensure no segfault from constraint
    type
      Regex[A: SomeOrdinal] = ref object
        val: Regex[A]
      MyConstraint = (seq or enum or set)
      MyOtherType[A: MyConstraint] = ref object
        val: MyOtherType[A]

    var
      a = Regex[int]()
      b = Regex[bool]()
      c = MyOtherType[seq[int]]()


when false:
  import std/typetraits

  type Foo[T1,T2] = object
    x1: T1
    x2: T2
  type FooInst = Foo[int, float]
  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  type Foo2 = genericHead(FooInst)
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}
  type Foo2Inst = Foo2[int, float]

  proc a[T: tuple or object](x, y: T) = discard

  var
    x: Foo[int, float]
    y: FooInst
    z: Foo2[int, float]
    w: Foo2Inst

  doAssert Foo[int, float] is FooInst
  doAssert FooInst is Foo[int, float]

  doAssert x is Foo[int, float]
  doAssert x is FooInst
  doAssert x is Foo2[int, float]
  doAssert x is Foo2Inst

  doAssert y is Foo[int, float]
  doAssert y is FooInst
  doAssert y is Foo2[int, float]
  doAssert y is Foo2Inst

  doAssert z is Foo[int, float]
  doAssert z is FooInst
  doAssert z is Foo2[int, float]
  doAssert z is Foo2Inst

  doAssert w is Foo[int, float]
  doAssert w is FooInst
  doAssert w is Foo2[int, float]
  doAssert w is Foo2Inst

  doAssert typeOf(x) is Foo[int, float]
  doAssert typeOf(x) is FooInst
  doAssert typeOf(x) is Foo2[int, float]
  doAssert typeOf(x) is Foo2Inst

  doAssert typeOf(y) is Foo[int, float]
  doAssert typeOf(y) is FooInst
  doAssert typeOf(y) is Foo2[int, float]
  doAssert typeOf(y) is Foo2Inst

  doAssert typeOf(z) is Foo[int, float]
  doAssert typeOf(z) is FooInst
  doAssert typeOf(z) is Foo2[int, float]
  doAssert typeOf(z) is Foo2Inst

  doAssert typeOf(w) is Foo[int, float]
  doAssert typeOf(w) is FooInst
  doAssert typeOf(w) is Foo2[int, float]
  doAssert typeOf(w) is Foo2Inst

  doAssert FooInst is Foo2[int, float]
  doAssert Foo2[int, float] is FooInst

  doAssert FooInst is Foo[int, float]
  doAssert Foo[int, float] is FooInst

  doAssert Foo2Inst is Foo2[int, float]
  doAssert Foo2[int, float] is Foo2Inst

  doAssert Foo2Inst is FooInst
  doAssert FooInst is Foo2Inst

  doAssert Foo2Inst is Foo[int, float]
  doAssert Foo[int, float] is Foo2Inst

  # {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  a y, w
  # {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}
  # doAssert FooInst.default == Foo2Inst.default

when false:

  type
    SyntaxObject = object
      ## Represents AST + metadata to support semantic analysis, including
      ## various lowerings/raisings.
      ast: LinearNodes
      lineInfos: LineMeta
      attr: AttributeLog

    LinearNode = object ## packed linear AST
      kind: LNKind
      left, right: int32
    LinearNodes = seq[LinearNode]
    LNKind = enum
     lnkNone # TODO: remaining enum fields

    LineMeta = object
      ## xxx: store lineinfo for nodes, separate so it can be compact?
      ## later expand to store splicing information so we can track meta-
      ## routine transformations (macros and templates)

    AttributeLog = object
      ## TODO: more fields will go here to support tighter data representation
      ##       in the `log` itself, and perhaps some indexing structures
      ## Store things like: types, symbols, diagnostics, scope open/closures,
      ##                    analysis/lowering phases, context information, etc
      ##
      ## Things that when taken into account with `ConfigRef` and
      ## `SemData.TContext` one could query all the relevant semantic
      ## information thus far. The log means we can speculatively analyse
      ## something without polluting things like `ConfigRef` or `TContext` and
      ## then choose to commit that information, or not, depending upon whether
      ## the analysis was deemed successful.
      log: seq[Attribute]

    Attribute = object
      ## meant to represent key value information, possibly associated with a
      ## sub-tree
      key, value: string  ## TODO: more specialized structured data
      id: NodeId          ## optionally associate to an `SyntaxObject.ast` node

    Kind = enum
      kNone

      kDefStg

      kFatal
      kError
      kWarn
      kHint

      kDefSym

      kDefPrj

      kDefPkg

      kDefMod # xxx: use `kDefSym` instead?
      kImpMod
      kIncMod

      kRcvAst
      kSndAst

      kDefTyp


when false:
  ## This describes the medium to long-term target for nimskull, and it's
  ## unlikely we'll implement this all in one go.
  block the_basics:
    ## key principles, the basics:
    ## 1. all type comparisons are fundamentally structural
    ## 2. types are conceptually Table[Field], an unordered set
    ## 3. type declaration: is a symbol on the left, with a type expression on
    ##    the right
    ## TODO:
    ##   1. describe how name field vs alias fields works
    ##   2. describe how field ordering is achieved (i.e. order num in Field)
    type
      ## nominal types are conceptually achieved through a special name field,
      ## 'breaking' the structural comparison
      Foo = object # object is nominal with an implied name field 
      Bar = object # this is why `Bar` doesn't match `Foo`
      Baz = int    # `Baz` and `int` are equivalent, `Baz` is an alias, meaning
                  # it and the symbol `int` produce the same type value of `int`

  block basic_parameteric_polymorphism:
    ## key principles, basic parametric polymorphism (universal/for-all):
    ## 1. same as above
    ## 2. same as above
    ## 3. revised to: is a symbol, or parameterized symbol, with a type
    ##    expression, or type expression body if the symbol is parameterized, on
    ##    the right
    ##    a. generic type expression: formed by type parameters preceding the
    ##       type expression body, `[A] => body`, or `[A, B] => body`
    ##    b. for convenience, type expression refers to both basic and generic
    ##       varieties unless qualified to mean one or the other
    ##    c. parameterized symbols define 1) the generic kind symbol (excluding
    ##       type parameters), and, when provided, for all type parameter values
    ##       a unique symbol each.
    ##    d. application: specifying a type parameter for a generic in a type
    ##       expression
    ##    TODO:
    ##      1. kind (`Foo`) vs parameterized type (`Foo[A]`) vs type (`int`),
    ##           and cannot go "up" a level of abstraction
    ##      2. elaborate on binding?
    ##      3. differentiate bound, unbound, and unapplied?
    type
      # no effort was made to keep names unique
      Foo[A] = object # a kind `Foo` and all types `Foo[A]` representing distinct
                      #   nominal objects
      Bar[A] = int    # a kind `Bar` and all types `Bar[A]` representing the same
                      #   type value `int`, `Bar[A]` and int are structurally
                      #   equivalent
      Bar = Foo       # alias to the kind `Foo`
      Bar = Foo[A]    # illegal, symbols must be parameterized in order for the
                      #   type expression to have a type parameter(s)
      Bar[A] = Foo[A] # for all applications of `Bar[A]` we produce a `Foo[A]`,
                      #   this is not an alias, but a mapping, and the type
                      #   expressions `Bar[A]` and `Foo[A]` are structurally
                      #   equivalent for all applications of `A`
      Bar[A] = Foo    # illegal, a parameterized type cannot go up a level to a
                      #   kind

  block basic_parameteric_polymorphism_examples:
    ## Some examples to demonstrate the above:
    type
      Foo[A] = object
      Bar[A] = Foo[A]
      Baz = Foo
      Qux[A] = int

    when false:
      type
        Foo = int # name collision

    var
      fooInt = Foo[int]()
      fooFlt = Foo[float]()
    doAssert fooInt is Bar[int], "Bar: (A)->Foo(A) => (int)->Foo(int) => (int)->object#Foo_int"
    doAssert fooFloat is Bar[float]

    doAssert not compiles(var fooFoo: Foo), "Foo isn't construct/init-able, only recv/asgn existing value"
    doAssert not compiles(var fooFoo: Baz), "alias, so same as above"

    proc a[A](f: Foo[A]): string = "FooA"
    proc a(f: Foo[float]): string = "Foofloat"
    proc a(f: Foo): string = "Foo"

  when false:
    proc a[A](f: Bar[A]) = discard # ambiguous with `Foo[A]` variety
    proc a(f: Baz) = discard       # ambiguous with `Foo` variety
  
  doAssert a(fooInt) == "FooA"
  doAssert a(Bar[float]) == "Foofloat"
  doAssert a(Foo fooInt) == "Foo", "only way it could win in an overload, but should we allow this cast?"

when false:
  import std/sugar
  proc call5(f: (int {.noSideEffect.} -> int)): int = f(42)
  # doAssert call5(x {.noSideEffect.} => x + 1) == 43
  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  discard call5(x {.noSideEffect.} => x + 1)
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

when false:
  import std/sequtils

  iterator foo(): int =
    yield 1
    yield 2
    yield 3

  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  discard toSeq(foo())
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

when false:
  import std/macros

  macro m(x: varargs[typed]) =
    discard

  m(missingIdent)

when false:
  import std/macros

  macro m(x: typed) =
    echo "typed"
    discard

  macro m(x: untyped) =
    echo "untyped"
    discard

  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  m(missingIdent)
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

when false:
  import std/macros

  macro bar(x: typed): untyped =
    x

  bar:
    static:
      raise newException(CatchableError, "hi")

when false:
  iterator foo(): int =
    yield 0

  template one(arg: untyped): untyped =
    mixin items
    discard compiles(typeof(items(arg)))

  template two(arg: untyped): untyped =
    mixin items
    discard compiles(typeof(arg()))

  template three(arg: untyped): untyped =
    mixin items
    discard toSeqBuiltin[typeof(arg)](arg)

  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  three(foo())
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

when false:
  import std/macros

  macro bar(x: typed): untyped =
    x

  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  proc foobar() {.bar.} =
    totallyInvalidSym()
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

when false:
  proc overloaded(x: int) = discard
  proc overloaded(x: string) = discard
  
  var x: RootRef
  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  overloaded((ref RootObj)(x)[])
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}

when false:
  proc test(arg: string) =
    discard

  template test(arg: untyped) =
    for it in arg:
      discard

  # two overloads of ``test2`` have to exist, with one being an iterator while
  # the other is not. The return types don't matter as long as the non-iterator
  # doesn't return anything that matches `string` or something for which an
  # ``items`` iterator exists.
  iterator test2(): int =
    yield 0

  proc test2(): bool =
    discard

  {.define(nimCompilerDebug), define(nimCompilerDebugCalltrace).}
  test(test2())
  {.undef(nimCompilerDebug), undef(nimCompilerDebugCalltrace).}
