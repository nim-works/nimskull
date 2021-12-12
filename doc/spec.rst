======================
Language specification
======================

.. note:: Language specification implementation is an ongoing effort
          tracked in the `GitHub project
          <https://github.com/nim-works/nimskull/projects/2>`_

There are many ideas of what we could change or where to go next, but those
are often poorly thought out. The sheer volume of what we don't know about
the existing system, good or bad, is so large we're working in the dark.
This is a blindspot induced by the current state of the code base.

What we do know, with great certainty, is that we need tests that not only
test things but also:

- teach the reader
- are a useful reasoning tools
- remember decisions of the past
- help uncover lost ideas
- shine a light on dark corners and issues

Going through the exercise of improving tests will address the blindspot
and is going to teach all of us where to go next and which ideas are more
likely to get us there.


A language specification defines what's allowed, disallowed, undefined,
guaranteed, not guaranteed, and known failure modes. More than a written
document, our language specification is executable that, to the maximum
extent possible, performs automated conformance testing of candidate
compilers. Along with test code codifying the specification, technical
commentary describes to the reader the detailed aspects of the language.
The scope of the specification is the language and potentially any compiler
interfaces, but exclusive of the standard library unless necessary. The
specification is structured such that concepts are introduced as late as
possible, leading to a natural layering of language facilities that build
upon the previous. This layering allows for incremental learning for author
or consumer and natural path of progress for any compiler implementation as
a software engineering project.

Rationale
---------

The above is a highlevel description of the end goal. The specification is
a tool within a continuously improving development cycle that reinforces
quality, clear communication, development speed, and bold experimentation.

The biggest benefit will be upon conclusion of the project, but as it
progresses the intended use of the specification is as follows:

* document the language both as it is and make early decisions of what to
  no longer support due to issues explained through attempts at
  codification and testing
* identify bugs, and if relatively straightforward, resolve them as the
  appropriate behaviour is specified
* using the logical progression of concepts in the language, layering them
  upon each other in a progressive fashion, we will highlight the
  architecture of language features and spot potential simplifications of
  the language itself
* provide an entry point for those wishing to contribute by way of
  clarifying the specification
* and much more

After the language specification is complete, the specification will be the
undeniable source of truth and at the center of many or our processes:

* *reduce the burden of contribution and maintenance* - allow compiler
  developers to freely refactor and make bold changes with a strong
  assurance that the specification will be automatically checked
* *ongoing simplicity and quantifiable debate* - using the specification
  and seeing the impact of changes we can quantify the merit and simplicity
  of various proposals
* *less RFCs and other heavy processes and more code* - proposal of new
  features must work their way through the specification itself, allow the
  proposer, their collaborators, and others to see the impact not simply in
  some giant RFC, but executable code
* *regression tests* - as part of our CI we'll hae a continuous regression
  tests suite
* *clear communication of changes* - specifications can be diffed, older
  ones run against newer compilers and vice versa; tagging, reviews, and
  all the other tools many are familiar with are easily leveraged
* *teaching and education* - contributors and programmers alike can easily
  and gradually learn the language itself by way of the specification; not
  to the exclusion of our teaching materials of course

Frequently Asked Questions
~~~~~~~~~~~~~~~~~~~~~~~~~~

*What are some examples to follow?*

See the following:

* Code: https://github.com/nim-works/nimskull/tree/devel/tests/lang -
  implement language specification in form of proper tests
* PR: `Specification tests PR1
  <https://github.com/nim-works/nimskull/pull/28>`_
* PR: `Specification tests PR2
  <https://github.com/nim-works/nimskull/pull/43>`_
* PR: `Specification tests PR3
  <https://github.com/nim-works/nimskull/pull/59>`_

Implementation
--------------

Currenly, language specification is represented as a collection of tests in
the `tests/lang` subdirectory. This test suite provides a set of tests that
aim to fully describe the language features and interactions between them.
It starts from the very basic elements (such as expressions, ``if``
statements, control flow) and advances through the various parts of the
language.

These tests are written in a slightly 'detached' manner - instead of trying
to specify the *existing* behavior, their main aim is to provide examples
of the *correct* behavior. All other parts of the |nimskull| must try to
converge with these tests as much as possible (that is - fix bugs that
prevent correct-as-described-in-spec behavior).

For example, right now this code does not compile with a type mismatch
error, but it *should* compile and run as expected. Because of this, we
added a `knownIssue` field to the test specification (it contains a
one-line summary of the issue.

.. code:: nim

    discard """
    description: '''
    Generic constraints for enum and tuple fail to resolve (ambigouus call)
    if explicit generic parameter is used. When called without explicit parameter
    (type inferred from the argument), code compiles correctly.
    '''
    knownIssue: "enum/tuple generic constraint fails to resolve (ambiguous)"
    errormsg: "ambiguous call [...]"
    """

    proc store[T: enum](target: T) = discard
    proc store[T: tuple](target: T) = discard

    proc aux1[T](obj: T) = store(obj)

    ## aux1 can run and compile correctly
    aux1((1, 2))

    proc aux2[T](obj: T) = store[T](obj)

    ## aux2 fails compilation
    aux2((1, 2))

.. note:: Full error message is 302 characters wide at the moment, so it
          was trimmed to `[...]`

You can think of a spec as an abstract set of tests targeting a
hypothetical *fully correct* compiler - you should be able to take this
specification and start writing your own |NimSkull| compiler from scratch, or
adding a new backend.

Specification itself is separated into several categories -

1. Basics - fundamental language constructs such as identifiers, literals,
   comments, primitive types, control flow.
2. Core - User-defined data types, procedures, iterators, effect syste,
   templates, macros.
3. Module system
4. Warnings and hints - various compiler diagnostics with explanations
5. Pragmas - interop and other pragmas
6. Experimental - experimental features.

Each specification directory contains a readme that goes into more details
about how and *why* tests should be structured in a particular manner.
