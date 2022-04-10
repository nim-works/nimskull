============
Contributing
============

.. default-role:: code
.. include:: ../doc/rstcommon.rst

.. contents::

If you're looking to make larger changes and even if not, please read this
short blurb on `the direction <https://github.com/nim-works/nimskull#direction>`_
of the language.

Specifically, we value |sustainability|. This means that the way we work
together, our customs and practices, the design and development of the language
and associated libraries, tools and so on, all work towards this goal.

Getting Context
===============

Not everything is being worked on and somethings may in fact be up for removal
outright. It's a big code base and not always easy to see where things are
headed or whether your change will be accepted. What trade-offs to make and all
sorts of questions that arise during software development.

For a very small change (typos, fixes, etc) please, open up a PR with tests and
a fix, with help from the rest of this guide this will be quick and easy.

For more impactful or sweeping changes or you'd like to do things work on
things more regularly, then please gain context through some of these suggested
activities:

- check the `current near-term development plan <https://github.com/nim-works/nimskull#near-term-development>`_
- see current efforts by reading the last `~10 commits messages <https://github.com/nim-works/nimskull/commits/devel>`_
- to understand the current broader ideas, `see recent discussions <https://github.com/nim-works/nimskull/discussions>`_
- check out roadmap progression `thread <https://github.com/nim-works/nimskull/discussions/142?sort=new>`_ for a high-level overview
- join our `matrix chat <https://matrix.to/#/#nimworks:envs.net>`_

With that said, contributing happens via "Pull requests" (PR) on github. Every
PR needs to be reviewed before it can be merged and the Continuous Integration
should be green.

The PR has to be approved by at least one core developer, if a breaking change
then mulitple core developers must weigh in.



Writing or improving tests
==========================

We recommend starting with tests, in fact one of our goals, and that is key
to |sustainability| is writing the language specification as tests. Getting
comprehensive specification is one of the main goals of the |nimskull| at
the moment - contributions are more than welcome. You can learn more
details about the spec structure `here <spec.html#>`_.

Types of testing approaches, in order of preference. Where marked as
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

.. note:: Not all the tests follow the convention here, feel free to change
          the ones that don't. Always leave the code cleaner than you found
          it. Usage of deprecated tests styles is almost guaranteed to lead
          to PR rejection.

.. note:: Progress of the test suite improvements is tracked in the github
          `project <https://github.com/nim-works/nimskull/projects/2>`_.

Cleaning up existing tests
--------------------------

Original collection of tests in the test suite contained a lot of files
that did not conform to the requirements listed above, and should
eventually be fixed. See `Clean up and reorder tests #41
<https://github.com/nim-works/nimskull/issues/41>`_ issue on github for
more context and relevant discussion.

Improving Language specification
--------------------------------

In order to have the confidence in the compiler implementation and it's
behaviour we must provide a comprehensive suite of checks for the compiler
behavior. This is a complex undertaking as a whole, but it can be easily
split in a smaller contributions.

Stdlib
------

Each stdlib module (anything under ``lib/``, e.g. ``lib/pure/os.nim``) should
have a corresponding separate tests folder and files, e.g.
``tests/stdOs/tos.nim``.

Each test should be in a separate `block:` statement, such that
each has its own scope. Use boolean conditions and `doAssert` for the
testing by itself, don't rely on echo statements or similar; in particular, avoid
things like `echo "done"`.

A few notes on things to watch out for and not repeat. An old convention was to
add a `when isMainModule:` block in the source file, which only gets executed
when the tester is building the file. Don't use `unittest.suite` and
`unittest.test`.

Please replace this if you're making changes associated to the module.

Sample test:

.. code-block:: nim

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

Always refer to a GitHub issue using the following exact syntax: ``bug #1234`` as shown
above, so that it's consistent and easier to search or for tooling. Some browser
extensions (e.g. https://github.com/sindresorhus/refined-github) will even turn those
in clickable links when it works.

Rationale for using a separate test file instead of `when isMainModule:` block:
* allows custom compiler flags or testing options (see details below)
* faster CI since they can be joined in ``megatest`` (combined into a single test)
* avoids making the parser do un-necessary work when a source file is merely imported
* avoids mixing source and test code when reporting line of code statistics or code coverage

Compiler
--------

The tests for the compiler use a testing tool called `testament`:cmd:. They are all
located in ``tests/`` (e.g.: ``tests/destructor/tdestructor3.nim``).
Each test has its own file. All test files are prefixed with `t`. If you want
to create a file for import into another test only, use the prefix `m`.

At the beginning of every test is the expected behavior of the test.
Possible keys are:

- `description`: Description for the overall goal of tests within this file
- `cmd`: A compilation command template e.g. `nim $target --threads:on $options $file`:cmd:
- `output`: The expected output (stdout + stderr), most likely via `echo`
- `exitcode`: Exit code of the test (via `exit(number)`)
- `errormsg`: The expected compiler error message
- `file`: The file the errormsg was produced at
- `line`: The line the errormsg was produced at

For a full spec, see here: ``testament/specs.nim``

An example of a test:

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


Running tests
=============

You can run the tests with

.. code-block:: cmd

  ./koch.py tests

which will run a good subset of tests. Some tests may fail. If you
only want to see the output of failing tests, go for

```cmd
  ./koch.py tests --failing all
```

You can also run only a single category of tests. A category is a subdirectory
in the ``tests/`` directory. There are a couple of special categories; for a
list of these, see ``testament/categories.nim``, at the bottom.

.. code:: cmd

  ./koch.py tests c lib # compiles / runs stdlib modules, including `isMainModule` tests
  ./koch.py tests c megatest # runs a set of tests that can be combined into 1

To run a single test:

.. code:: cmd

  ./koch.py test run <category>/<name>    # e.g.: tuples/ttuples_issues
  ./koch.py test run tests/stdlib/tos.nim # can also provide relative path

For reproducible tests (to reproduce an environment more similar to the one
run by Continuous Integration on GitHub Actions pipelines), you may want to disable your
local configuration (e.g. in ``~/.config/nim/nim.cfg``) which may affect some
tests; this can also be achieved by using
`export XDG_CONFIG_HOME=pathtoAlternateConfig`:cmd: before running `./koch.py`:cmd:
commands.


Comparing tests
===============

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


Deprecation
===========

Backward compatibility is important, so instead of a rename you need to deprecate
the old name and introduce a new name:

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


Documentation
=============

When contributing new procs, be sure to add documentation, especially if
the proc is public. Even private procs benefit from documentation and can be
viewed using `nim doc --docInternal foo.nim`:cmd:.
Documentation begins on the line
following the `proc` definition, and is prefixed by `##` on each line.

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
  e.g. for C++ inline highlighting::

    `#include <stdio.h>`:cpp:

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


Pr and commit message guidelines
================================

Commit message guidelines
-------------------------

One of the key goals of the |nimskull| project is sustainability. Writing
good commit messages is important, since future contributors need to have a
way to reason about decitions made in the past.

It should be clear as to *"what"* change was made. Additional details like
high-level oviewview of *"how"* and *"why"* are also welcome, but should go
into commit body. Of course *"what"* can be contiued in the body as well.

.. code::

   where: what? 50 chars or less

   *why*? format to 72 chars width

   Also consider writing about "how?" and/or more elaborate explanation of
   "what?"

- `"where"` is optional, but recommended. It can be a specific file, or a
  subsystem (like `sem` (semantic analysis in compiler), `lexer` (compiler
  lexer), `stdlib` (standard library), `tests`, `spec` (for language
  specification improvements) and so on)

- `"what"` implementation details. Unless the commit is trivial (e.g. test
  added, dead code removed), it is better to document what exactly
  changed - you already know it, and it might save a *huge* amount of time
  to the next contributor.

- `"why"` each change has a reason, however trivial it might be. Those
  reasons **must** be documented in a way that would allow future
  contributors to understand problems that you were trying to solve.


Rules
<<<<<

Commit message should contain enough information in order for end user to
understand what is going on by looking at the git log.

Example of bad commit message::

  Fixes #123; refs #124

indicates that issue ``#123`` is completely fixed (GitHub may automatically
close it when the PR is committed), wheres issue ``#124`` is referenced
(e.g.: partially fixed) and won't close the issue when committed. This
tells reader **nothing** about what the commit did, requires going into two
separate github pages and reading through multiple comments in order to
figure out what was *wrong* - not what *changed* in the commit.

Instead, commit title should contain a human-readable text (we are
optimizing for humans after all)

- Keep the title length to 50 characters, and commit message body to 72
  characters.
- If you want to use automatic "fixes" feature of the github, please supply
  full URL in the body instead, it makes it much easier to copy-paste and
  see (with pure hash you first need to understand whether change was in
  ``issues/`` or ``pull/``, then open random PR/issue and replace hash
  URL).
- Consider using "Fixes" for commits that fix bugs, and "Closes" for
  commits that implement features.
- Most of the points on this list were derived from `How To Write a Good
  Commit Message
  <https://api.coala.io/en/latest/Developers/Writing_Good_Commits.html>`_ -
  we suggest reading it as well.

Crafting good commit title might be hard at first. Some simple rules to
look out for:

- Avoid using phrases like ``"minor fixes for XXX"``, ``"various
  improvements"``, ``"tiny patch"`` - they don't add any substantial
  information and only clutter the title.
- No bare github hashes in the title. Things like ``fix #19193 (#19195)
  [backport:1.2]`` are absolutely illegible.

When writing commit message consider who are you writing this message
for:

- you or someone else in the far future found a bug and stumbled upon
  this in a git bisect
- you or someone else is going through recent commits trying to write a
  changelog
- you or someone else is trying to find a bug that can't be replicated
  reliably so they're reading commits seeing which might be related
- someone new to the code base is trying to learn bits and pieces about
  what's being worked on and what types of things are changing
- you or someone else is sorting out documentation or test gaps, or
  seeing if old issues can be closed with recent changes


Message examples
<<<<<<<<<<<<<<<<

This section is build in a do-don't manner.

- Implementation details of a particular commit can go into the body, title
  should only contain *what?*
  - ``fixes #9456 by only calling c_fclose if non nil`` -> ``Fix crash when
    closing an unopened file on debian``
    - Implementation details of the commit can be put in the commit body
  - ``Deprecate 'c', 'C' prefix for octal literals, fixes #8082 (#8178)``
    - Good, but commit hash should preferrably be replaced with a url.
  - ``fixes #12015 by also checking kind of typeNode`` -> ``Fix unmarshal
    of aliased tuple field using json.to``
    - Again, implementation details go into commit body
- No bare commit hashes:
  - ``fixes #19051 [backport:1.6] (#19133)`` -> ``Fix gcc codegen of `type
    G[T] = int``` or ``Fix gcc codegen of generic int alias``. Pr **body**
    should be

    .. code::

       Fixes https://github.com/nim-lang/Nim/issues/19051

  - ``update manual (#19130) [backport]`` -> ``Add InstantiationInfo and
    EAssertionFailed to manual`` (new title message is 52 characters long,
    but in this case it is acceptable to go two characters over limit)




Additional considerations
<<<<<<<<<<<<<<<<<<<<<<<<<

1. If you introduce changes which affect backward compatibility,
   make breaking changes, or have PR which is tagged as ``[feature]``,
   the changes should be mentioned in `the changelog
   <https://github.com/nim-works/nimskull/blob/devel/changelog.md>`_.

2. All changes introduced by the commit (diff lines) must be related to the
   subject of the commit.

   If you change something unrelated to the subject parts of the file, because
   your editor reformatted automatically the code or whatever different reason,
   this should be excluded from the commit.

   *Tip:* Never commit everything as is using `git commit -a`:cmd:, but review
   carefully your changes with `git add -p`:cmd:.

3. Changes should not introduce any trailing whitespace.

   Always check your changes for whitespace errors using `git diff --check`:cmd:
   or add the following ``pre-commit`` hook:

   .. code:: cmd

      #!/bin/sh
      git diff --check --cached || exit $?

4. Commits should be always be rebased against devel (so a fast forward
   merge can happen)

   e.g.: use `git pull --rebase origin devel`:cmd:. This is to avoid messing up
   git history.
   Exceptions should be very rare: when rebase gives too many conflicts, simply
   squash all commits using the script shown in
   https://github.com/nim-lang/Nim/pull/9356

5. Do not mix pure formatting changes (e.g. whitespace changes, nimpretty) or
   automated changes (e.g. nimfix) with other code changes: these should be in
   separate commits (and the merge on GitHub should not squash these into 1).


Pull requests message
---------------------

Pull request description and title generally follow the same guidelines as
commit messages.


The `git`:cmd: stuff
====================

PR should contain a single commit
---------------------------------

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
---------------------------------------

1. First check the CI logs and search for `FAIL` to find why CI failed; if the
   failure seems related to your PR, try to fix the code instead of restarting CI.

2. If CI failure seems unrelated to your PR, it could be caused by a flaky test.
   File a bug for it if it isn't already reported. A PR push (or opening/closing PR)
   will re-trigger all CI jobs (even successful ones, which can be wasteful). Instead,
   follow these instructions to only restart the jobs that failed:

   * GitHub actions: under "Checks" tab, click "Re-run jobs" in the right.
   * builds.sr.ht: create a sourcehut account so you can restart a PR job as illustrated.
     builds.sr.ht also allows you to ssh to a CI machine which can help a lot for debugging
     issues, see docs in https://man.sr.ht/builds.sr.ht/build-ssh.md and
     https://drewdevault.com/2019/08/19/Introducing-shell-access-for-builds.html; see
     https://man.sr.ht/tutorials/set-up-account-and-git.md to generate and upload ssh keys.




Documentation Style
===================

General Guidelines
------------------

* See also `nep1<https://nim-lang.github.io/Nim/nep1.html>`_.
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


Module-level documentation
--------------------------

Documentation of a module is placed at the top of the module itself. Each
line of documentation begins with double hashes (`##`). Sometimes `##[
multiline docs containing code ]##` is preferable, see
``lib/pure/times.nim``. Code samples are encouraged, and should follow the
general RST syntax:

.. code-block:: Nim

  ## The `universe` module computes the answer to life, the universe, and everything.
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
---------------------------------------------------

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
-----

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
      name*: string ## The name of the item
      val*: T ## Its value
      next*: ref NamedQueue[T] ## The next item in the queue

Make sure to place the documentation beside or within the object.

.. code-block:: Nim

  type
    ## Bad: this documentation disappears because it annotates the `type` keyword
    ## above, not `NamedQueue`.
    NamedQueue*[T] = object
      name*: string ## This becomes the main documentation for the object, which
                    ## is not what we want.
      val*: T ## Its value
      next*: ref NamedQueue[T] ## The next item in the queue

Var, Let, and Const
-------------------

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


Evolving the stdlib
===================

What the compiler itself needs must be part of the stdlib
---------------------------------------------------------

Maybe in the future the compiler itself can depend on Nimble packages but for
the time being, we strive to have zero dependencies in the compiler as the
compiler is the root of the bootstrapping process and is also used to build
Nimble.


Vocabulary types must be part of the stdlib
-------------------------------------------

These are types most packages need to agree on for better interoperability,
for example `Option[T]`. This rule also covers the existing collections like
`Table`, `CountTable` etc. "Sorted" containers based on a tree-like data
structure are still missing and should be added.

Time handling, especially the `Time` type are also covered by this rule.


Existing, battle-tested modules stay
------------------------------------

Reason: There is no benefit in moving them around just to fullfill some design
fashion as in "|NimSkull|'s core MUST BE SMALL". If you don't like an existing module,
don't import it. If a compilation target (e.g. JS) cannot support a module,
document this limitation.

This covers modules like `os`, `osproc`, `strscans`, `strutils`,
`strformat`, etc.


Syntactic helpers can start as experimental stdlib modules
----------------------------------------------------------

Reason: Generally speaking as external dependencies they are not exposed
to enough users so that we can see if the shortcuts provide enough benefit
or not. Many programmers avoid external dependencies, even moreso for
"tiny syntactic improvements". However, this is only true for really good
syntactic improvements that have the potential to clean up other parts of
the |NimSkull| library substantially. If in doubt, new stdlib modules should start
as external, successful Nimble packages.



Other new stdlib modules do not start as stdlib modules
-------------------------------------------------------

As we strive for higher quality everywhere, it's easier to adopt existing,
battle-tested modules eventually rather than creating modules from scratch.


Little additions are acceptable
-------------------------------

As long as they are documented and tested well, adding little helpers
to existing modules is acceptable. For two reasons:

1. It makes |NimSkull| easier to learn and use in the long run.
   ("Why does sequtils lack a `countIt`?
   Because version 1.0 happens to have lacked it? Silly...")
2. To encourage contributions. Contributors often start with PRs that
   add simple things and then they stay and also fix bugs. |NimSkull| is an
   open source project and lives from people's contributions and involvement.
   Newly introduced issues have to be balanced against motivating new people. We know where
   to find perfectly designed pieces of software that have no bugs -- these are the systems
   that nobody uses.

Conventions
-----------
1. New stdlib modules should go under ``Nim/lib/std/``. The rationale is to
   require users to import via `import std/foo` instead of `import foo`,
   which would cause potential conflicts with nimble packages.
   Note that this still applies for new modules in existing logical
   directories, e.g.: use ``lib/std/collections/foo.nim``,
   not ``lib/pure/collections/foo.nim``.

2. New module names should prefer plural form whenever possible, e.g.:
   ``std/sums.nim`` instead of ``std/sum.nim``. In particular, this reduces
   chances of conflicts between module name and the symbols it defines.
   Furthermore, module names should use `snake_case` and not use capital
   letters, which cause issues when going from an OS without case
   sensitivity to an OS with it.


Breaking Changes
================

Introducing breaking changes, no matter how well intentioned,
creates long-term problems for the community, in particular those looking to promote
reusable |NimSkull| code in libraries: In the |NimSkull| distribution, critical security and bugfixes,
language changes and community improvements are bundled in a single distribution - it is
difficult to make partial upgrades with only benign changes. When one library depends on
a legacy behavior, it can no longer be used together with another library that does not,
breaking all downstream applications - the standard library is unique in that it sits at
the root of **all** dependency trees.

There is a big difference between compile-time breaking changes and run-time breaking
changes.


Run-time breaking changes
-------------------------

Run-time breaking changes are to be avoided at almost all costs: |NimSkull| is used for
mission critical applications which depend on behaviours that
are not covered by the test suite. As such, it's important that changes to the
*stable* parts of the standard library are made avoiding changing the existing
behaviors, even when the test suite continues to pass.

Examples of run-time breaking changes:

- Raising exceptions of a new type, compared to what's currently being raised.

- Adding unconstrained or poorly constrained generic procs or macros
  ("hash now works for all `ref T`"): This may cause code to behave differently
  depending only on which modules are imported - common examples include `==` and `hash`.

- Changing behavior of existing functions like:

  * "|NimSkull|'s path handling procs like `getXDir` now consistently lack the trailing slash"
  * "|NimSkull|'s strformat implementation is now more consistent with Python"

Instead write new code that explicitly announces the feature you think we announced but
didn't. For example, `strformat` does not say "it's compatible with Python", it
says "inspired by Python's f-strings". This new code can be submitted to the stdlib
and the old code can be deprecated or it can be published as a Nimble package.

Sometimes, a run-time breaking change is most desirable: For example, a string
representation of a floating point number that "roundtrips" is much better than
a string represenation that doesn't. These run-time breaking changes must start in the
state "opt-in" via a new `-d:nimPreviewX` or command line flag and then should become
the new default later, in follow-up versions. This way users can track
regressions more easily. ("git bisect" is not an acceptable alternative, that's for
Nim compiler developers, not for |NimSkull| users.)

Above all else, additive approaches that don't change existing behaviors
should be preferred.


Compile-time breaking changes
-----------------------------

Compile-time breaking changes are usually easier to handle, but for large code bases
it can also be much work and it can hinder the adoption of a new |NimSkull| release.
Additive approaches are to be preferred here as well.

Examples of compile-time breaking changes include (but are not limited to):

* Renaming functions and modules, or moving things. Instead of a direct rename,
  deprecate the old name and introduce a new one.
* Renaming the parameter names: Thanks to |NimSkull|'s "named parameter" calling syntax
  like `f(x = 0, y = 1)` this is a breaking change. Instead live with the existing
  parameter names.
* Adding an enum value to an existing enum. |NimSkull|'s exhaustive case statements stop
  compiling after such a change. Instead consider to introduce new `bool`
  fields/parameters. This can be impractical though, so we use good judgement
  and our list of "important packages" to see if it doesn't break too much code
  out there in practice.
* Adding a new proc to an existing stdlib module. However, for aesthetic reasons
  this is often preferred over introducing a new module with just a single proc
  inside. We use good judgement and our list of "important packages" to see if
  it doesn't break too much code out there in practice. The new procs need to
  be annotated with a `.since` annotation.


Compiler/language spec bugfixes
-------------------------------

This can even be applied to compiler "bugfixes": If the compiler should have been
"pickier" in its handling of `typedesc`, instead of "fixing typedesc handling bugs",
consider the following solution:

- Spec out how `typedesc` should really work and also spec out the cases where it
  should not be allowed!
- Deprecate `typedesc` and name the new metatype something new like `typeArg`.
- Implement the spec.


Non-breaking changes
--------------------

Examples of changes that are considered non-breaking (or acceptable breaking changes) include:

* Creating a new module.
* Adding an overload to an already overloaded proc.
* Adding new default parameters to an existing proc. It is assumed that you do not
  use |NimSkull|'s stdlib procs's addresses (that you don't use them as first class entities).
* Changing the calling convention from `nimcall` to `inline`
  (but first RFC https://github.com/nim-lang/RFCs/issues/396 needs to be implemented).
* Changing the behavior from "crashing" into some other, well documented result (including
  raising a Defect, but not raising an exception that does not inherit from Defect).
* Adding new fields to an existing object.

Nim's introspection facilities imply that strictly speaking almost every addition can
break somebody's code. It is impractical to care about these cases, a change that only
affects introspection is not considered to be a breaking change.
