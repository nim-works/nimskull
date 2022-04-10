===========
 Testament
===========

.. default-role:: code
.. include:: rstcommon.rst
.. contents::

Testament is a test runner for running tests in the development of |NimSkull| itself.
It offers process isolation for tests, it can generate statistics about test cases,
supports multiple targets (C, C++, ObjectiveC, JavaScript, etc),
simulated `Dry-Runs <https://en.wikipedia.org/wiki/Dry_run_(testing)>`_,
has logging, can generate HTML reports, skip tests from a file, and more.


Test files location
===================

By default Testament looks for test files on ``"./tests/**/*.nim"``.
The default working directory path can be changed using
`--directory:"folder/subfolder/"`:option:.

Testament uses the `nim`:cmd: compiler on `PATH`.
You can change that using `--nim:"folder/subfolder/nim"`:option:.
Running JavaScript tests with `--targets:"js"`:option: requires
a working NodeJS on `PATH`.


Options
=======

--print                   Also print results to the console
--simulate                See what tests would be run but don't run them
                          (for debugging)
--failing                 Only show failing/ignored tests
--targets:"c cpp js objc"
                          Run tests for specified targets (default: all)
--nim:path                Use a particular nim executable (default: $PATH/nim)
--directory:dir           Change to directory dir before reading the tests
                          or doing anything else.
--colors:on|off           Turn messages coloring on|off.
--backendLogging:on|off   Disable or enable backend logging.
                          By default turned on.
--skipFrom:file           Read tests to skip from ``file`` - one test per
                          line, # comments ignored


Running a single test
=====================

This is a minimal example to understand the basics,
not very useful for production, but easy to understand:

.. code:: console

  $ mkdir tests
  $ echo "assert 42 == 42" > tests/test0.nim
  $ testament run test0.nim
  PASS: tests/test0.nim C                                    ( 0.2 sec)
  $ testament r test0
  PASS: tests/test0.nim C                                    ( 0.2 sec)


HTML Reports
============

Generate HTML Reports ``testresults.html`` from tests,
you have to run at least 1 test *before* generating a report:

.. code:: console

  $ testament html


Writing Tests
=============


``description`` - textual description of the test. **Highly** recomended to
add one - in the future testament might use this to provide better teardown
reports, or notify about ``knownIssue`` state transitions.

Test execution options
----------------------

- ``action`` - What action(s) to expect completion on.
  - ``"compile"``: expect successful compilation
  - ``"run"``: expect successful compilation and execution
  - ``"reject"``: expect failed compilation. The "reject" action can catch
    `{.error.}` pragmas but not `{.fatal.}` pragmas because `{.fatal.}` pragmas
    guarantee that compilation will be aborted.

- ``batchable``: Can be run in batch mode, or not.

- ``joinable``: Can be run Joined with other tests to run all togheter, or
  not. Defaults to `true`

- ``timeout`` Timeout seconds to run the test. Fractional values are supported.

- ``cmd``: Command used to run the test. If left out or an empty
  string is provided, the command is taken to be: ``"nim $target --hints:on
  -d:testing --nimblePath:build/deps/pkgs $options $file"`` You can use the
  ``$target``, ``$options``, and ``$file`` placeholders in your own
  command, too.

  example: ``"nim c -r $file"``

- ``targets`` supported backend compilation targets for test into (c,
  cpp, objc, js). Targets can be excluded via a `!`, eg: `!js` to exclude js.
  Additionally, a `native` target is supported in order to use the same target
  used for the compiler itself.

- ``matrix`` flags with which to run the test, delimited by `;`

- ``disabled`` Conditions that will skip this test. Use of multiple
  "disabled" clauses is permitted.

  .. code-block:: nim

    disabled: "bsd"   # Can disable OSes...
    disabled: "win"
    disabled: "32bit" # ...or architectures
    disabled: "i386"
    disabled: true    # ...or can disable the test entirely

- ``knownIssue``

Compiler output assertions
--------------------------

- ``errormsg``: Error message the test should print, if any.


- ``nimout`` Each line in the string given here appears in the same order
    in the compiler output, but there may be more lines that appear before,
    after, or in between them. Note that specifying multiline strings for
    testament spec inside of the `discard """` section requires using
    triple single quotes `'`

    .. code-block:: nim

        nimout: '''
        a very long,
        multi-line
        string'''

- ``nimoutFull``: true/false, controls whether full compiler output must be
  asserted, or only presence of error messages

- ``maxcodesize``: Max side of the resulting codegen file for a test

In addition to ``nimout`` message annotations testament also allows to
supply hints, warnings and error messages directly in the source code using
specially formatted comments, starting with ``#[tt.``. For example, if you
want to assert that error message is genrated, you can write a following
test:

.. code-block::

   {.error: "Error message".} #[tt.Error
     ^ "Error message"
   ]#

File, line and column information are automatically inferred from the
position of the ``^`` marker in the annotation body.


Binary output assertions
------------------------

- ``exitcode``: The exit code that the test is expected to return.
  Typically, the default value of 0 is fine. Note that if the test will be
  run by valgrind, then the test will exit with either a code of 0 on
  success or 1 on failure.

- ``output``, ``outsub``: Provide an `output` string to assert that the
  test prints to standard out exactly the expected string. Provide an
  `outputsub` string to assert that the string given here is a substring of
  the standard out output of the test.

- ``sortoutput`` Whether to sort the output lines before comparing them to
  the desired output.

- ``input``: this is the Standard Input the test should take, if any.


- ``valgrind`` On Linux 64-bit machines, whether to use Valgrind to check
    for bad memory accesses or memory leaks. On other architectures, the
    test will be run as-is, without Valgrind.

    - ``true``: run the test with Valgrind
    - ``false``: run the without Valgrind
    - ``"leaks"``: run the test with Valgrind, but do not check for memory leaks

* As you can see the "Spec" is just a `discard """ """`.
* Spec has sane defaults, so you don't need to provide them all, any simple assert will work just fine.
* `This is not the full spec of Testament, check the Testament Spec on GitHub, see parseSpec(). <https://github.com/nim-works/nimskull/blob/devel/testament/specs.nim#L238>`_
* `Nim itself uses Testament, so there are plenty of test examples. <https://github.com/nim-works/nimskull/tree/devel/tests>`_
* `Testament supports inlined error messages on tests, basically comments with the expected error directly on the code. <https://github.com/nim-works/nimskull/blob/9a110047cbe2826b1d4afe63e3a1f5a08422b73f/tests/effects/teffects1.nim>`_

Reading test outputs
====================

Testament supports two different modes of interaction with the compiler -
structured and unstructured. Unstructured interaction mode (currently
default) allows user to specify exact compiler output that should be
produced by the test and then compares it based on ``nimoutFull``
configuration options.

If there is a mismatch, a failure message is generated, showing the diffs.

.. code-block::

    discard """
    nimout: '''
    Expected unstructured compiler output
    '''
    """

    static:
      echo "Expected unstructured output"

In that case comparison is performed between two regular string blocks.
Since each entry is not wide enough (not wider than current terminal) they
are printed side-by side to make it easier to spot the difference.
Mismatches are also highlighted in the terminal.

.. code-block:: diff

    - Expected unstructured compiler output   + Expected unstructured output
    -                                         ?

.. note:: expected (on the left) outout has two lines deleted - trailing
          ``'''`` in the testament spec is placed on the next line, so it
          is considered to be a string literal of ``"Expected unstructured
          compiler output\n"``


Structured mismatches
---------------------

In structured output mode, the compiler writes out S-expressions for each
output diagnostic entry, one per line.

If testament is used in structured mode, all expected compiler reports -
both inline and written in ``nimout`` are collected in a single list that
is matched against produced output directly. The failure message shows the
best possible mismatch annotations for the given output. For example, given
the test below, testament output will contain two mismatches for both
failures.

.. code-block:: nim
    :linenos:

    discard """
    nimoutFormat: sexp
    cmd: "nim c --msgFormat=sexp --skipUserCfg --hints=on --hint=all:off --hint=User:on --filenames:canonical $file"
    nimout: '''
    (User :str "User Hint" :location ("tfile.nim" 8 _))
    '''
    """

    {.hint: "User hint".}

    {.hint: "Another hint".} #[tt.Hint
          ^ (User :str "Another hint") ]#


Both inline and ``nimout`` annotations are compared. Both have errors, so
the best possible mapping is presented as an error ('best' because it is
generally impossible to find correct place to insert inline annotation
somewhere in ``nimout``, without potentially messing up ordering.
Unstructured output simply sets inline annotations to a higher priority and
searches for them first)

.. code-block:: nim

    Expected inline Hint annotation at tfile.nim(11, 7):

    - (User :location ("tfile.nim" 11 7) :severity Hint :str "Another hint")

    Given:

    + (User :location ("tfile.nim" 11 6) :severity Hint :str "Another hint")


      :location[2] expected 7, but got 6 ([7->6])

    Expected:

    - (User :location ("tfile.nim" 8 _) :str "User Hint")

    Given:

    + (User :location ("tfile.nim" 9 6) :severity Hint :str "User hint")


      :str expected "User Hint", but got "User hint" ("User [Hint"->hint"])
      :location[1] expected 8, but got 9 ([8->9])

Compiler printed reports

.. code-block:: nim

    (User :severity Hint :str "User hint" :location ("tfile.nim" 8 6))
    (User :severity Hint :str "Another hint" :location ("tfile.nim" 10 6))

And they were matched against full list of expected entries. For the first
entry there is a mismatch in ``:location[2]``, and for second one there is
a string value error (in ``:str``) and another mismatch in location data.

To make it easier to spot differences between string values the inline diff
is added for the message.

Test Examples
==============

Structured test examples
------------------------

Unstructured old, style test examples
--------------------------------------

Expected to fail:

.. code-block:: nim

  discard """
    errormsg: "undeclared identifier: 'not_defined'"
  """
  assert not_defined == "not_defined", "not_defined is not defined"

Non-Zero exit code:

.. code-block:: nim

  discard """
    exitcode: 1
  """
  quit "Non-Zero exit code", 1

Standard output checking:

.. code-block:: nim

  discard """

    output: '''
  0
  1
  2
  3
  4
  5
  '''

  """
  for i in 0..5: echo i

JavaScript tests:

.. code-block:: nim

  discard """
    targets: "js"
  """
  when defined(js):
    import std/jsconsole
    console.log("My Frontend Project")

Compile-time tests:

.. code-block:: nim

  discard """
    action: "compile"
  """
  static: assert 9 == 9, "Compile time assert"

Tests without Spec:

.. code-block:: nim

  assert 1 == 1


See also:
* `Unittest <unittest.html>`_
