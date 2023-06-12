============
Contributing
============

.. default-role:: code
.. include:: ../doc/rstcommon.rst

.. contents::

Especially if you're looking to contribute a large change, please read this
short blurb on `the direction <https://github.com/nim-works/nimskull#direction>`_
of the language.

Specifically, we value |sustainability|. This means that the way we work
together, our customs and practices, the design and development of the
language, associated libraries, tools and so on, all work towards this goal.

At present the primary mechanism of contributions are by way of Pull Requests,
which at a minimum:

- consists of the, ideally small, change
- are tested and the Continuous Integration checks pass
- have a clear and neat commit and PR message
- approved by at least one core developer



Getting Started
===============

For a very small change (typos, fixes, etc) please, open up a PR with tests and
a fix, with help from the rest of this guide this will be quick and easy.

If you're looking for ideas to work on, or to make more impactful or sweeping
changes, then we recommend gaining some context through one or more of these
suggested activities:

- check the `current near-term development plan <https://github.com/nim-works/nimskull#near-term-development>`_
- see current efforts by reading the last `~10 commits messages <https://github.com/nim-works/nimskull/commits/devel>`_
- to understand the current broader ideas, `see recent discussions <https://github.com/nim-works/nimskull/discussions>`_
- check out roadmap progression `thread <https://github.com/nim-works/nimskull/discussions/142?sort=new>`_ for a high-level overview
- join our `matrix chat <https://matrix.to/#/#nimworks:envs.net>`_

This context gathering helps, as not everything is being worked on and
somethings may in fact be up for removal outright. It's a big code base, and
sometimes it's not easy to see where things are headed. What trade-offs to make
and all sorts of questions that arise during software development.



Change Approval
===============

We recognize that contributors are the life blood of this project. Our
intention is to both encourage new contributors and ensure one contributor's
actions don't takeaway the momentum of others and the project as a whole. This
is how contribution reviews are approached:

1. monotonic improvements, a change must leave things better than before
2. at least one core developer must approve

Small changes, docs, typos, bug fixes, items reported in issues, test, standard
lib improvements, etc can easily be accommodated with the PR review cycle. Note
for "bigger" small changes discussing it in chat/issues prior to submitting a
PR can smooth the process.

Changes, especially larger ones, often result in debates or back and forth in
review. No code is perfect and the intention of review is to ensure progress,
for contributors the review will be proportionate to the following:

1. impact of the change
2. cost of changing the code in the future
3. contributor's history instilling trust they'll keep contributing

Finally, discussion cannot resolve things and to maintain a cohesive direction,
resolving debates or matters of direction/taste, the final word is with Saem.

.. tip::

    Small PRs, simple implementation (state/control flow), principled design,
    thoughtful testing, documented code, and the like cut through lengthy back
    and forth.



Anatomy of a Pull Request
=========================

There is a GitHub Pull Request template to help guide crafting a description,
and you can liberally copy content from the commit message as needed.

If your PR isn't quite ready feel free to create it as a draft, then once
you're all set feel free to flip it to "Ready for review".


One Commit Per Pull Request
---------------------------

Pull request should contain a single **passing** commit. This is necessary
to ensure clean git history that is not cluttered by a partially working,
failing and outright failing to compile states.

To achieve this you can do either of the following:

- If the change fits into a single commit you don't need to do anything
- If you need to made some additional modifications (review requested) you
  can amend the commit and force-push it (`git commit --amend
  --no-edit`:cmd: and `git push --force <remote> <your branch>`:cmd:)
- Create multiple commits and then squash them when your pull request is
  approved.
  1. Create multiple commits
  2. Create new branch based on `devel` (`git checkout devel`:cmd: and `git
     checkout <branch>-squashed`:cmd:)
  3. Squash merge your original branch into a new one - `git merge --squash
     <branch>`:cmd:
  4. Commit your squashed branch using `git commit`:cmd:

     .. note::

        By default you will get pre-filled commit message which contains
        pretty verbose "sqash of the following" message - those are not
        going to be accepted by the PR reviewers, and need to be edited
        into human-readable error message according to the commit message
        guidelines.

     .. tip::

        You can write a commit message as a PR description and then
        copy-paste it when you are done with the implementation.

  5. Force-push your squashed branch using `git push --force <remote>
     HEAD:<branch>`:cmd:


Commit Message
--------------

One of the key goals of the |nimskull| project is sustainability. Writing
good commit messages is important, since future contributors need to have a
way to reason about decisions made in the past.

The impact (*"what"*) and reasoning (*"why"*) of the change **must** be clear,
and for more complex changes an overview of the approach (*"how"*). 

.. code::

  transf: move the `ImplicitObjConv` hint to `sempass2`

  The hint is about program semantics and should thus be reported
  during semantic analysis. This is a step towards removing all
  program-semantics-related diagnostics reporting from both the
  transformation and back-end phase.

  As a side-effect of this change, the hint is now produced for all code
  part of the compilation, instead of only for alive code.

*transf: move the `ImplicitObjConv` hint to `sempass2`*: descibes the change,
the *transf:* prefix describes where the change occurred and is recommended to
help disambiguate changes.

The body, starting with *The hint is about ...* paragraph describes the impact
of the change, as well as the reasoning behind it. Finally, the second
paragraph highlights indirect/additional impact of the change.

Last but not least, the text formatting of the the commit message must be::

  Maximum 50 character title

  Body is separated from the title by a new line. Each line in the body
  is wrapped to a maximum of 72 characters.

The title is formatted to 50 characters or less and describes what changed, it
also is prefixed by the recommended location/subsystem of the change.

How to Write a Good Commit Message
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

When writing commit message consider who are you writing this message for:

- you or someone else in the far future found a bug and stumbled upon
  this in a git bisect
- you or someone else is going through commits trying to write a changelog
- you or someone else is trying to find a bug that can't be replicated
  reliably so they're reading commits seeing which might be related
- someone new to the codebase is trying to learn bits and pieces about
  what's being worked on and what types of things are changing
- you or someone else is sorting out documentation, find gaps in testing, or
  seeing if old issues can be closed with recent changes

Is my Commit Message is Acceptable?
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

Commit message should contain enough information in order for the end user to
understand what is going on by looking at the git log.

If your commit message requires the reader to play detective to piece together
the impact, reasoning, and other pertinent information, it'll be rejected.

A classic case of a git commit message that'll be rejected::

  Fixes #123; refs #124

Getting Fancy with Commit Messages
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

The recommended "where" prefix for commit titles, can be a specific file, or a
subsystem (like `sem` (semantic analysis in compiler), `lexer` (compiler
lexer), `stdlib` (standard library), `tests`, `spec` (for language
specification improvements) and so on).

When using the "fixes" feature in GitHub to automatically close issues, use the
full URL instead of a GitHub `#` prefixed id.

What to Avoid
<<<<<<<<<<<<<

If your commit message requires a reader to play detective to piece together
the impact, reasoning, and other pertinent information, it'll be rejected.

A classic case of a git commit message that'll be rejected::

  Fixes #123; refs #124

Additional considerations
<<<<<<<<<<<<<<<<<<<<<<<<<

1. All changes introduced by the commit (diff lines) must be related to the
   subject of the commit.

   *Tip:* Never commit everything as is using `git commit -a`:cmd:, but review
   your changes carefully with `git add -p`:cmd:.

2. Changes should not introduce any trailing whitespace.

   Always check your changes for whitespace errors using `git diff --check`:cmd:
   or add the following ``pre-commit`` hook:

   .. code:: cmd

      #!/bin/sh
      git diff --check --cached || exit $?

3. Commits should be always be rebased against devel

   e.g.: use `git pull --rebase origin devel`:cmd:. This is to avoid messing up
   git history.

4. Do not mix pure formatting changes with other code changes, create separate PRs


Continuous Integration (CI)
---------------------------

1. Continuous Integration is by default run on every push in a PR; this clogs
   the CI pipeline and affects other PR's; if you don't need it (e.g. for WIP or
   documentation only changes), add ``[skip ci]`` to your commit message title.
   This convention is supported by our GitHub Actions pipelines (using custom
   logic, which should complete in < 1mn).

2. Consider enabling CI (GitHub Actions and builds.sr.ht) in your own |NimSkull| fork, and
   waiting for CI to be green in that fork (fixing bugs as needed) before
   opening your PR in the original |NimSkull| repo, so as to reduce CI congestion. Same
   applies for updates on a PR: you can test commits on a separate private
   branch before updating the main PR.

Debugging CI failures, flaky tests, etc
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

1. First check the CI logs and search for `FAIL` to find why CI failed; if the
   failure seems related to your PR, try to fix the code instead of restarting CI.

2. If CI failure seems unrelated to your PR, it could be caused by a flaky test.
   File a bug for it if it isn't already reported. A PR push (or opening/closing PR)
   will re-trigger all CI jobs (even successful ones, which can be wasteful). Instead,
   follow these instructions to only restart the jobs that failed:

   * GitHub actions: under "Checks" tab, click "Re-run jobs" in the right.

For reproducible tests (to reproduce an environment more similar to the one
run by Continuous Integration on GitHub Actions pipelines), you may want to disable your
local configuration (e.g. in ``~/.config/nim/nim.cfg``) which may affect some
tests; this can also be achieved by using
`export XDG_CONFIG_HOME=pathtoAlternateConfig`:cmd: before running `./koch.py`:cmd:
commands.



Change Specific Guidance
========================

"Breaking" Changes
------------------

|NimSkull| the language and standard library are unstable, the only thing that
needs to work is the boostrap process and the VS Code Extension written and
used by Saem and other devs. If a change would break bootstrapping, then
instead of a rename you need to deprecate the old name and introduce a new one:

.. code-block:: nim

  # for routines (proc/template/macro/iterator) and types:
  proc oldProc(a: int, b: float): bool {.deprecated:
      "deprecated since v1.2.3; use `newImpl: string -> int` instead".} = discard

  # for (const/var/let/fields) the msg is not yet supported:
  const Foo {.deprecated.}  = 1

  # for enum types, you can deprecate the type or some elements
  # (likewise with object types and their fields):
  type Bar {.deprecated.} = enum bar0, bar1
  type Barz  = enum baz0, baz1 {.deprecated.}, baz2


See also `Deprecated <manual.html#pragmas-deprecated-pragma>`_
pragma in the manual.


Compiler/language spec bugfixes
-------------------------------

This can even be applied to compiler "bugfixes": If the compiler should have been
"pickier" in its handling of `typedesc`:

While in regular discussion with |NimSkull| core devs:
- Spec out how `typedesc` should really work and also spec out the cases where it
  should not be allowed!
- Deprecate `typedesc` and name the new metatype something new like `typeArg`.
- Implement the spec.


Evolving the stdlib
-------------------

Maybe in the future the compiler itself can depend on external packages but for
the time being, we strive to have zero dependencies in the compiler as the
compiler is the root of the bootstrapping process.



Testing
=======

Writing Tests
-------------

Tests are located in ``tests/`` (e.g.: ``tests/destructor/tdestructor3.nim``).

Each test has its own file. All test files are prefixed with `t`. If you want
to create a file for import into another test only, use the prefix `m`. For the
standard library, each module (anything under ``lib/``,
e.g. ``lib/pure/os.nim``) should have a corresponding separate tests folder
and files.

At the beginning of every test file is the test specification description, eg:

.. code-block:: nim

  discard """
  description: "parameter type mismatch results in an error"
  errormsg: "type mismatch: got (PTest)"
  """

  type
    PTest = ref object

  proc test(x: PTest, y: int) = nil

  var buf: PTest
  buf.test()

For a full spec, see here: ``testament/specs.nim``

Each test should be in a separate `block:` statement, such that
each has its own scope. Use boolean conditions and `doAssert` for the
testing by itself, don't rely on echo statements or similar; in particular, avoid
things like `echo "done"`.

Some legacy code use these conventions which are to be avoided:
- using a `when isMainModule:` block in the source file
- using `unittest.suite` and `unittest.test` routines

Please replace this if you're making changes associated to the module.

Sample standard library test:

.. code-block:: nim

  discard """
  """

  block foo_multiplies_by_10:
    doAssert foo(1) == 10

  block int_constant_expression_evaluation:
    ## encountered a regressions with integer constant expression evaluation
    ## see bug #1234
    static: doAssert 1+1 == 2, "int constant expression evaluation regression"

  block structural_comparison_of_nested_seqs:
    var seq2D = newSeqWith(4, newSeq[bool](2))
    seq2D[0][0] = true
    seq2D[1][0] = true
    seq2D[0][1] = true
    doAssert seq2D == @[@[true, true], @[true, false],
                        @[false, false], @[false, false]]
    # doAssert with `not` can now be done as follows:
    doAssert not (1 == 2)

Rationale for using a separate test file instead of `when isMainModule:` block:
* allows custom compiler flags or testing options (see details below)
* faster CI since they can be joined in ``megatest`` (combined into a single test)
* avoids making the parser do un-necessary work when a source file is merely imported
* avoids mixing source and test code when reporting line of code statistics or code coverage


Running Tests
-------------

To execute tests use `./koch.py`:

.. code-block:: cmd

  ./koch.py test all # run all available tests

Run a single category of tests. A category are the immediate subdirectories
under the directory ``tests/``:

.. code:: cmd

  ./koch.py tests c stdlib # runs tests under tests/stdlib

or run a single tests:

.. code:: cmd

  ./koch.py test run tests/stdlib/tos.nim

For a faster test cycle, rerun failing tests only:

.. code:: cmd

  ./koch.py test --retry


Comparing tests
---------------

Test failures can be grepped using ``Failure:``.

The tester can compare two test runs. First, you need to create a
reference test. You'll also need to the commit id, because that's what
the tester needs to know in order to compare the two.

.. code:: cmd

  git checkout devel
  DEVEL_COMMIT=$(git rev-parse HEAD)
  ./koch.py tests

Then switch over to your changes and run the tester again.

.. code:: cmd

  git checkout your-changes
  ./koch.py tests

Then you can ask the tester to create a ``testresults.html`` which will
tell you if any new tests passed/failed.

.. code:: cmd

  ./koch.py tests --print html $DEVEL_COMMIT


Useful Test Patterns
--------------------

If you're new to testing easiest way to start is by looking in the
`tests/lang` directory. Compiler tests are executed using `testament`, see its
docs for further details.

Here is a short list of patterns for testing specific cases for which examples
might be hard to find unless you know what to look for:

- *Test for specific values*: Either use `doAssert <given> == <expected>`
  or `check` from `std/unittest`

- *Test that is supposed to fail*: There are several different ways a test
  can "fail":

  - *Known issue that should be fixed later*: Write a test for code as it
    *should* work and annotate the specification using `knownIssue` with
    provided explanation.

  - *Exception must be thrown*: Either use `try ... except` construct and
    check for type/field of the exception captured or use `expect` macro
    from `std/unittest`

  - *Specific compilation error*: Use `errormsg: "<error>"` and `line:
    <line>` fields in the specification.

.. tip:: If the test already uses `std/unittest` or you plan to rewrite it
         to use the library you should prefer `check` and `expect` macros,
         otherwise you can stick to `try..except` and `doAssert`.

         Another reason why might opt to use simpler solution is reducing
         number of dependencies for the test -- if you want your test to
         include as little additional details as possible.



Documenting
===========

.. Tip::
  Documentation for a module, including private members, can be viewed using
  `nim doc --docInternal foo.nim`:cmd:.


General Guidelines
------------------

* Authors should document anything that is exported; documentation for
  private procs can be useful too (visible via `nim doc --docInternal
  foo.nim`:cmd:).
* Within documentation, a period (`.`) should follow each sentence (or
  sentence fragment) in a comment block. The documentation may be limited
  to one sentence fragment, but if multiple sentences are within the
  documentation, each sentence after the first should be complete and in
  present tense.
* Documentation is parsed as a custom ReStructuredText (RST) with partial
  markdown support.
* In nim sources, prefer single backticks to double backticks since it's
  simpler and `nim doc`:cmd: supports it. Likewise with ``rst`` files: `nim
  rst2html`:cmd: will render those as monospace, and adding ``..
  default-role:: code`` to an ``rst`` file will also make those render as
  monospace when rendered directly in tools such as github.

.. code-block:: nim

  proc someproc*(s: string, foo: int) =
    ## Use single backticks for inline code, e.g.: `s` or `someExpr(true)`.
    ## Use a backlash to follow with alphanumeric char: `int8`\s are great.


Documentation Style Guidelines
------------------------------

When you specify an *RST role* (highlighting/interpretation marker) do it
in the postfix form for uniformity, that is after \`text in backticks\`.
For example an ``:idx:`` role for referencing a topic ("SQLite" in the
example below) from `NimSkull Index`_ can be used in doc comment this way:

.. code-block:: nim
  ## A higher level `SQLite`:idx: database wrapper.

.. _`NimSkull Index`: https://nim-works.github.io/nimskull/theindex.html

Inline monospaced text can be input using \`single backticks\` or
\`\`double backticks\`\`. The former are syntactically highlighted,
the latter are not.
To avoid accidental highlighting follow this rule in ``*.nim`` files:

* use single backticks for fragments of code in |NimSkull| and other
  programming languages, including identifiers, in ``*.nim`` files.

  For languages other than |NimSkull| add a role after final backtick,
  e.g. for C inline highlighting::

    `#include <stdio.h>`:c:

  For a currently unsupported language add the `:code:` role,
  like for SQL in this example::

    `SELECT * FROM <table_name>;`:code:

  Highlight shell commands by ``:cmd:`` role; for command line options use
  ``:option:`` role, e.g.: \`--docInternal\`:option:.

* prefer double backticks otherwise:

  * for file names: \`\`os.nim\`\`
  * for fragments of strings **not** enclosed by `"` and `"` and not
    related to code, e.g. text of compiler messages
  * also when code ends with a standalone ``\`` (otherwise a combination of
    ``\`` and a final \` would get escaped)

.. Note:: ``*.rst`` files have ``:literal:`` as their default role.
          So for them the rule above is only applicable if the ``:nim:`` role
          is set up manually as the default [*]_::

            .. role:: nim(code)
               :language: nim
            .. default-role:: nim

          The first 2 lines are for other RST implementations,
          including Github one.

          .. [*] this is fulfilled when ``doc/rstcommon.rst`` is included.

Runnable code examples are also encouraged, to show typical behavior with a few
test cases (typically 1 to 3 `assert` statements, depending on complexity).
These `runnableExamples` are automatically run by `nim doc mymodule.nim`:cmd:
as well as `testament`:cmd: and guarantee they stay in sync.

.. code-block:: nim
  proc addBar*(a: string): string =
    ## Adds "Bar" to `a`.
    runnableExamples:
      assert "baz".addBar == "bazBar"
    result = a & "Bar"

See `parentDir <os.html#parentDir,string>`_ example.

The RestructuredText |NimSkull| uses has a special syntax for including code snippets
embedded in documentation; these are not run by `nim doc`:cmd: and therefore are
not guaranteed to stay in sync, so `runnableExamples` is almost always preferred:

.. code-block:: nim

  proc someProc*(): string =
    ## Returns "something"
    ##
    ## .. code-block::
    ##  echo someProc() # "something"
    result = "something" # single-hash comments do not produce documentation

The ``.. code-block:: nim`` followed by a newline and an indentation instructs the
`nim doc`:cmd: command to produce syntax-highlighted example code with the
documentation (``.. code-block::`` is sufficient from inside a nim module).

Module-level documentation
<<<<<<<<<<<<<<<<<<<<<<<<<<

Documentation of a module is placed at the top of the module itself. Each
line of documentation begins with double hashes (`##`). Sometimes `##[
multiline docs containing code ]##` is preferable, see
``lib/pure/times.nim``. Code samples are encouraged, and should follow the
general RST syntax:

.. code-block:: Nim

  ## The `universe` module computes the answer to life, the universe, and
  ## everything.
  ##
  ## .. code-block::
  ##  doAssert computeAnswerString() == 42


Within this top-level comment, you can indicate the authorship and
copyright of the code, which will be featured in the produced
documentation.

.. code-block:: Nim

  ## This is the best module ever. It provides answers to everything!
  ##
  ## :Author: Steve McQueen
  ## :Copyright: 1965
  ##

Leave a space between the last line of top-level documentation and the
beginning of |NimSkull| code (the imports, etc.).

Procs, Templates, Macros, Converters, and Iterators
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

When contributing new procs or editing existing procs without documentation,
please document them, this is a must for stdlib public API. Even private procs
benefit from documentation.

Documentation begins on the line following the `proc` definition, and each line
prefixed with `##`.

When forward declaration is used, the documentation should be included with the
first appearance of the proc.

.. code-block:: nim

  proc hello*(): string
    ## Put documentation here
  proc nothing() = discard
  proc hello*(): string =
    ## ignore this
    echo "hello"

The preferred documentation style is to begin with a capital letter and use
the third-person singular. That is, between:

.. code-block:: nim

  proc hello*(): string =
    ## Returns "hello"
    result = "hello"

or

.. code-block:: nim

  proc hello*(): string =
    ## say hello
    result = "hello"

the first is preferred.

The documentation of a procedure should begin with a capital letter and
should be in present tense. Variables referenced in the documentation
should be surrounded by single tick marks:

.. code-block:: Nim

  proc example1*(x: int) =
    ## Prints the value of `x`.
    echo x

Whenever an example of usage would be helpful to the user, you should
include one within the documentation in RST format as below.

.. code-block:: Nim

  proc addThree*(x, y, z: int8): int =
    ## Adds three `int8` values, treating them as unsigned and
    ## truncating the result.
    ##
    ## .. code-block::
    ##  # things that aren't suitable for a `runnableExamples` go in code-block:
    ##  echo execCmdEx("git pull")
    ##  drawOnScreen()
    runnableExamples:
      # `runnableExamples` is usually preferred to ``code-block``, when possible.
      doAssert addThree(3, 125, 6) == -122
    result = x +% y +% z

The command `nim doc`:cmd: will then correctly syntax highlight the |NimSkull|
code within the documentation.

Types
<<<<<

Exported types should also be documented. This documentation can also
contain code samples, but those are better placed with the functions to
which they refer.

.. code-block:: Nim

  type
    NamedQueue*[T] = object ## Provides a linked data structure with names
                            ## throughout. It is named for convenience. I'm making
                            ## this comment long to show how you can, too.
      name*: string ## The name of the item
      val*: T ## Its value
      next*: ref NamedQueue[T] ## The next item in the queue


You have some flexibility when placing the documentation:

.. code-block:: Nim

  type
    NamedQueue*[T] = object
      ## Provides a linked data structure with names
      ## throughout. It is named for convenience. I'm making
      ## this comment long to show how you can, too.
      name*: string            ## The name of the item
      val*: T                  ## Its value
      next*: ref NamedQueue[T] ## The next item in the queue

Make sure to place the documentation beside or within the object.

.. code-block:: Nim

  type
    ## Bad: this documentation disappears because it annotates the `type` keyword
    ## above, not `NamedQueue`.
    NamedQueue*[T] = object
      name*: string            ## This becomes the main documentation for the
                               ## object, which is not what we want.
      val*: T                  ## Its value
      next*: ref NamedQueue[T] ## The next item in the queue

Var, Let, and Const
<<<<<<<<<<<<<<<<<<<

When declaring module-wide constants and values, documentation is
encouraged. The placement of doc comments is similar to the `type`
sections.

.. code-block:: Nim

  const
    X* = 42 ## An awesome number.
    SpreadArray* = [
      [1,2,3],
      [2,3,1],
      [3,1,2],
    ] ## Doc comment for `SpreadArray`.

Placement of comments in other areas is usually allowed, but will not
become part of the documentation output and should therefore be prefaced by
a single hash (`#`).

.. code-block:: Nim

  const
    BadMathVals* = [
      3.14, # pi
      2.72, # e
      0.58, # gamma
    ] ## A bunch of badly rounded values.

|NimSkull| supports Unicode in comments, so the above can be replaced with
the following:

.. code-block:: Nim

  const
    BadMathVals* = [
      3.14, # π
      2.72, # e
      0.58, # γ
    ] ## A bunch of badly rounded values.



Starter Contribution Areas
==========================

Writing or improving tests
--------------------------

We recommend starting with tests, as it aligns with our key principle of
|sustainability|, and the goal of language specification as tests. Building a
comprehensive specification is one of the main goals of |nimskull|, you can
learn more about the spec structure `here <spec.html#>`_.

Progress of the test suite improvements is tracked in the github
`project <https://github.com/nim-works/nimskull/projects/2>`_.

Types of testing approaches, in order of preference. If they're marked as
deprecated these are to be converted where possible and not used going
forward:

1. separate test files, e.g.: ``tests/stdlib/tos.nim``.
   In |NimSkull| repo, `testament`:cmd: (see below) runs all
   ``$nim/tests/*/t*.nim`` test files

   - Usage of ``echo`` in tests is discouraged and should be replaced with
     ``doAssert`` when possible. Assertion-based tests are easier to read
     (condition for test pass is written in the test itself) and move
     around when needed (for running single piece of code for debugging, or
     moving whole test into another file)

   - Don't forget to add `description:` field to the testament specification

   - Also comment on the test's parts. Well-written test with careful
     explanation of what is going on, or why particular element is tested
     in this manner can serve as a powerful teaching tool.

2. `runnableExamples` documentation comment tests, ran by `nim doc mymod.nim`:cmd:
   These end up in documentation and ensure documentation stays in sync with code.

.. note:: Not all the tests follow the convention here, feel free to update
          the ones that don't. Always leave the code cleaner than you found
          it. Usage of deprecated tests styles is almost guaranteed to lead
          to PR rejection.


Improving Language specification
--------------------------------

In order to have the confidence in the compiler implementation and it's
behavior we must provide a comprehensive suite of checks for the compiler
behavior. This is a complex undertaking as a whole, but it can be easily
split in a smaller contributions.


Cleaning up Existing Tests
--------------------------

Original collection of tests in the test suite contained a lot of files
that did not conform to the requirements listed above, and should
eventually be fixed. A list of known issues that should be fixed includes,
but not limited to:

- Check if test name makes sense - `t123123_b.nim` does not make sense,
  change it to something matching what is being tested. File names usually
  refer to the numbers of issues in the original repository.

- Reduce number of echo-based error testing. If you see direct echo in test
  consider changing it to the `doAssert` check instead.

  Added assertions should replace original sequence of checks with
  `doAssert a == <expected>` expression. If original check printed multiple
  values in sequence (for example in a for loop) you can collect them into
  a `seq[string]` variable and compare using `==` later.

- Link relevant issues in the test description (`description` field) or in
  comments.

  Huge number of original tests "referred" to issue numbers using file
  names or highly illegible comments such as `# XYZ123` placed at arbitrary
  locations all over the code. You should replace them with actual `url`
  links from https://github.com/nim-lang/Nim/issues so people can see the
  context quickly.

- If possible, provide explanation to the test logic. You can use the
  description of the linked issue as a basis.

- Adding labels to existing tests. For guidelines on test label usage and
  list of existing tags with documentation please see testament
  documentation `Labels
  <https://nim-works.github.io/nimskull/testament.html#writing-tests-labels>`_
  section.

