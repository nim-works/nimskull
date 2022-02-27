=====================
Nimskull coding style
=====================


.. default-role:: code
.. include:: rstcommon.rst
.. contents::


Introduction
============

Although Nim supports a variety of code and formatting styles, it is
nevertheless beneficial that certain community efforts, such as the standard
library, should follow a consistent set of style guidelines when suitable.
This enhancement proposal aims to list a series of guidelines that the standard
library should follow.

Note that there can be exceptions to these rules. Nim being as flexible as
it is, there will be parts of this style guide that don't make sense in
certain contexts. Furthermore, just as `Python's style
guide<http://legacy.python.org/dev/peps/pep-0008/>`_ changes over time,
this style guide will too.

These rules will only be enforced for contributions to the |nimskull|
codebase and official projects, such as the |nimskull| compiler, the
standard library, and the various official tools.

Style Guidelines
================


Spacing and Whitespace Conventions
-----------------------------------

- Lines should be no longer than 80 characters. Limiting the amount of
  information present on each line makes for more readable code - the reader
  has smaller chunks to process.

- Two spaces should be used for indentation of blocks; tabstops are not allowed
  (the compiler enforces this). Using spaces means that the appearance of code
  is more consistent across editors. Unlike spaces, tabstop width varies across
  editors, and not all editors provide means of changing this width.

- Although use of whitespace for stylistic reasons other than the ones endorsed
  by this guide are allowed, careful thought should be put into such practices.
  Not all editors support automatic alignment of code sections, and re-aligning
  long sections of code by hand can quickly become tedious.

  .. code-block:: nim
    # This is bad, as the next time someone comes
    # to edit this code block, they
    # must re-align all the assignments again:
    type
      WordBool*    = int16
      CalType*     = int
      ... # 5 lines later
      CalId*       = int
      LongLong*    = int64
      LongLongPtr* = ptr LongLong

Control flow constructs
-----------------------

General rule for spacing out control flow constructs and similar elements
is to not try and needlessly cram all constructs into a single chunk of
code. Adding more spacing in the code (to a reasonable degree of course)
increases readability. There is no need to pack as much actions as possible
into as little space as possible. Utility of code like this is questionable
at best.

Code like this is noticeably harder to read than one that is spaced out
more.

.. code-block:: nim

  if i == arg.len: discard
  elif i < arg.len and (arg[i] in {':', '='}): inc(i)
  else: invalidCmdLineOption(conf, pass, orig, info)

.. code-block:: nim

  if i == arg.len:
    discard

  elif i < arg.len and (arg[i] in {':', '='}):
    inc(i)

  else:
    invalidCmdLineOption(conf, pass, orig, info)

- Avoid using single-line `if` and `elif` unless it is a small inline expression


Naming Conventions
------------------

- Type identifiers should be in PascalCase. All other identifiers should be in
  camelCase with the exception of constants which **may** use PascalCase but
  are not required to.

  .. code-block:: nim
    # Constants can start with either a lower case or upper case letter.
    const aConstant = 42
    const FooBar = 4.2

    var aVariable = "Meep" # Variables must start with a lowercase letter.

    # Types must start with an uppercase letter.
    type
      FooBar = object

  For constants coming from a C/C++ wrapper, `ALL_UPPERCASE` are allowed,
  but ugly. (Why shout `CONSTANT`? Constants do no harm, variables do!)

- When naming types that come in value, pointer, and reference varieties, use a
  regular name for the variety that is to be used the most, and add a "Obj",
  "Ref", or "Ptr" suffix for the other varieties. If there is no single variety
  that will be used the most, add the suffixes to the pointer variants only. The
  same applies to C/C++ wrappers.

  .. code-block:: nim
    type
      Handle = object # Will be used most often
        fd: int64
      HandleRef = ref Handle # Will be used less often

- Exception and Error types should have the "Error" or "Defect" suffix.

  .. code-block:: nim
    type
      ValueError = object of CatchableError
      AssertionDefect = object of Defect
      Foo = object of Exception # bad style, try to inherit CatchableError or Defect

- Unless marked with the `{.pure.}` pragma, members of enums should have an
  identifying prefix, such as an abbreviation of the enum's name.

  .. code-block:: nim
    type
      PathComponent = enum
        pcDir
        pcLinkToDir
        pcFile
        pcLinkToFile

- Non-pure enum values should use camelCase whereas pure enum values should use
  PascalCase.

  .. code-block:: nim
    type
      PathComponent {.pure.} = enum
        Dir
        LinkToDir
        File
        LinkToFile

- In the age of HTTP, HTML, FTP, TCP, IP, UTF, WWW it is foolish to pretend
  these are somewhat special words requiring all uppercase. Instead treat them
  as what they are: Real words. So it's `parseUrl` rather than
  `parseURL`, `checkHttpHeader` instead of `checkHTTPHeader` etc.

- Operations like `mitems` or `mpairs` (or the now deprecated `mget`)
  that allow a *mutating view* into some data structure should start with an `m`.
- When both in-place mutation and 'returns transformed copy' are available the latter
  is a past participle of the former:

  - reverse and reversed in algorithm
  - sort and sorted
  - rotate and rotated

- When the 'returns transformed copy' version already exists like `strutils.replace`
  an in-place version should get an ``-In`` suffix (`replaceIn` for this example).


- Use `subjectVerb`, not `verbSubject`, e.g.: `fileExists`, not `existsFile`.

The stdlib API is designed to be **easy to use** and consistent. Ease of use is
measured by the number of calls to achieve a concrete high level action. The
ultimate goal is that the programmer can *guess* a name.

The library uses a simple naming scheme that makes use of common abbreviations
to keep the names short but meaningful.


-------------------     ------------   --------------------------------------
English word            To use         Notes
-------------------     ------------   --------------------------------------
initialize              initFoo        initializes a value type `Foo`
new                     newFoo         initializes a reference type `Foo`
                                       via `new` or a value type `Foo`
                                       with reference semantics.
this or self            self           for method like procs, e.g.:
                                       `proc fun(self: Foo, a: int)`
                                       rationale: `self` is more unique in English
                                       than `this`, and `foo` would not be DRY.
find                    find           should return the position where
                                       something was found; for a bool result
                                       use `contains`
contains                contains       often short for `find() >= 0`
append                  add            use `add` instead of `append`
compare                 cmp            should return an int with the
                                       `< 0` `== 0` or `> 0` semantics;
                                       for a bool result use `sameXYZ`
put                     put, `[]=`     consider overloading `[]=` for put
get                     get, `[]`      consider overloading `[]` for get;
                                       consider to not use `get` as a
                                       prefix: `len` instead of `getLen`
length                  len            also used for *number of elements*
size                    size, len      size should refer to a byte size
capacity                cap
memory                  mem            implies a low-level operation
items                   items          default iterator over a collection
pairs                   pairs          iterator over (key, value) pairs
delete                  delete, del    del is supposed to be faster than
                                       delete, because it does not keep
                                       the order; delete keeps the order
remove                  delete, del    inconsistent right now
include                 incl
exclude                 excl
command                 cmd
execute                 exec
environment             env
variable                var
value                   value, val     val is preferred, inconsistent right
                                       now
executable              exe
directory               dir
path                    path           path is the string "/usr/bin" (for
                                       example), dir is the content of
                                       "/usr/bin"; inconsistent right now
extension               ext
separator               sep
column                  col, column    col is preferred, inconsistent right
                                       now
application             app
configuration           cfg
message                 msg
argument                arg
object                  obj
parameter               param
operator                opr
procedure               proc
function                func
coordinate              coord
rectangle               rect
point                   point
symbol                  sym
literal                 lit
string                  str
identifier              ident
indentation             indent
-------------------     ------------   --------------------------------------


Coding Conventions
------------------

- The `return` statement should ideally be used when its control-flow properties
  are required. Use a procedure's implicit `result` variable whenever possible.
  This improves readability.

  .. code-block:: nim
    proc repeat(text: string, x: int): string =
      result = ""

      for i in 0..x:
        result.add($i)

- Ideally implementation should start with `func`, since it does not allow
  side effects. If you have some sort of side effect use `proc`, and
  consider using the more powerful facilities of macros, templates,
  iterators, and converters only when necessary.

- Use the `let` statement (not the `var` statement) when declaring variables that
  do not change within their scope. Using the `let` statement ensures that
  variables remain immutable, and gives those who read the code a better idea
  of the code's purpose.


Conventions for multi-line statements and expressions
-----------------------------------------------------

- Tuples which are longer than one line should indent their parameters to
  align with the parameters above it.

  .. code-block:: nim
    type
      LongTupleA = tuple[wordyTupleMemberOne: int, wordyTupleMemberTwo: string,
                         wordyTupleMemberThree: float]

- Similarly, any procedure and procedure type declarations that are longer
  than one line should do the same thing.

  .. code-block:: nim
    type
      EventCallback = proc (timeReceived: Time, errorCode: int, event: Event,
                            output: var string)

    proc lotsOfArguments(argOne: string, argTwo: int, argThree: float,
                         argFour: proc(), argFive: bool): int
                        {.heyLookALongPragma.} =

- Multi-line procedure calls should continue on the same column as the opening
  parenthesis (like multi-line procedure declarations).

  .. code-block:: nim
    startProcess(nimExecutable, currentDirectory, compilerArguments
                 environment, processOptions)

Miscellaneous
-------------

- Use `a..b` instead of `a .. b`, except when `b` contains an operator, for example `a .. -3`.
  Likewise with `a..<b`, `a..^b` and other operators starting with `..`.

- Use `std` prefix for standard library modules, namely use `std/os` for single module and
  use `std/[os, sysrand, posix]` for multiple modules.

- Prefer multiline triple quote literals to start with a newline; it's semantically identical
  (it's a feature of triple quote literals) but clearer because it aligns with the next line:

  use this:

  .. code-block:: nim
    let a = """
    foo
    bar
    """

  instead of:

  .. code-block:: nim
    let a = """foo
    bar
    """

- A getter API for a private field `foo` should preferably be named `foo`, not `getFoo`.
  A getter-like API should preferably be named `getFoo`, not `foo` if:
    * the API has side effects
    * or the cost is not `O(1)`
  For in between cases, there is no clear guideline.

- Likewise with a setter API, replacing `foo` with `foo=` and `getFoo` with `setFoo`
  in the above text.

Best practices
==============

Note: these are general guidelines, not hard rules; there are always exceptions.
Code reviews can just point to a specific section here to save time and
propagate best practices.

Avoid primitive types for an API
--------------------------------

Prefer using semantically meaningful types for return values instead of
booleans or integer codes. Prefer named types (enums, objects).

When returning tuples do name fields, especially if meaning of the each one
is not obvious from the procedure name itself.

Define needs prefix
-------------------

New `defined(foo)` symbols need to be prefixed by the nimble package name, or
by `nim` for symbols in nim sources (e.g. compiler, standard library). This is
to avoid name conflicts across packages.

.. code-block:: nim

  # if in nim sources
  when defined(allocStats): discard # bad, can cause conflicts
  when defined(nimAllocStats): discard # preferred
  # if in a package `cligen`:
  when defined(debug): discard # bad, can cause conflicts
  when defined(cligenDebug): discard # preferred

No implicit bool
----------------

Take advantage of no implicit bool conversion

.. code-block:: nim

  doAssert isValid() == true
  doAssert isValid() # preferred

Designing for method call syntax
--------------------------------

Design with method call syntax chaining in mind

.. code-block:: nim

  proc foo(cond: bool, lines: seq[string]) # bad
  proc foo(lines: seq[string], cond: bool) # preferred
  # can be called as: `getLines().foo(false)`

Avoid quit
----------

Use exceptions (including `assert` / `doAssert`) instead of `quit`

.. code-block:: nim

  quit() # bad in almost all cases
  doAssert() # preferred

Use ``doAssert`` in tests
-------------------------

Use `doAssert` (or `unittest.check`, `unittest.require`), not `assert` in all
tests so they'll be enabled even with `--assertions:off`:option:.

.. code-block:: nim

  block: # foo
    assert foo() # bad
    doAssert foo() # preferred

Use assert in runnable examples
-------------------------------

An exception to the above rule is `runnableExamples` and ``code-block`` rst blocks
intended to be used as `runnableExamples`, which for brevity use `assert`
instead of `doAssert`. Note that `nim doc -d:danger main`:cmd: won't pass `-d:danger`:option: to the
`runnableExamples`, but `nim doc --doccmd:-d:danger main`:cmd: would, and so would the
second example below:

.. code-block:: nim

  runnableExamples:
    doAssert foo() # bad
    assert foo() # preferred

  runnableExamples("-d:danger"):
    doAssert foo() # `assert` would be disabled here, so `doAssert` makes more sense

Delegate printing
-----------------

Delegate printing to caller: return `string` instead of calling `echo`
rationale: it's more flexible (e.g. allows the caller to call custom printing,
including prepending location info, writing to log files, etc).

.. code-block:: nim

  proc foo() = echo "bar" # bad
  proc foo(): string = "bar" # preferred (usually)

Use ``Option``
--------------

Consider using Option instead of return bool + var argument,
unless stack allocation is needed (e.g. for efficiency).

.. code-block:: nim

  proc foo(a: var Bar): bool
  proc foo(): Option[Bar]
