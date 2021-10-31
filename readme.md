# <img src="https://raw.githubusercontent.com/nim-lang/assets/master/Art/logo-crown.png" height="28px"/> Nimskull

The Nim-Works compiler, stdlib, tools, and documentation repository. Nim-Works
is presently a derivative of [Nim][nim-site]. Its intention best described by:

> Nimskull is not an alternative implementation; it's a reimaging of the language
> that aims for a community where if one uses the language then they also
> contribute directly to the language. This is a community effort with a strong
> emphasis on sustainability, the contributor community comes first.
> 
> As to why this fork exists, it's a matter of taste and this would not exist
> if governance, language design, type system, implementation choices, standard
> library, package management, and more had an acceptable trajectory.

Ultimately this will mean a different community, language, standard library,
and much more. For now it's closer to an alternative with most deviations
appearing in the standard library, mostly in the form of deleting much of it.

PS. the name `Nimskull` is temporary

## Direction

A language (community, compiler, etc) that is *sustained* through the
*collective* efforts of its practitioners and their *diverse* backgrounds.

Attracting practitioners with diversity of experience and perspectives
requires a language with broad applicability, from the *Web to Systems*
*Programming* all the while remaining *efficient*.

Onboarding practitioners requires a langauge that is familiar enough to get
started in terms of syntax and initial concepts such as *structured and*
*modular programming*.

Supporting pracitioner driven innovation requires a language that allows for
experimentation without necessarily being an expert in all aspects of language
development. Compile time facilities integrated into the language, such as
*compile time evaluation* and a *macro system* provide a sandbox.

Practitioner collaboration and combining collective efforts is assisted through
logical contracts provided by a *static type system* that supports local
inference, tuples, sum, and generic types, along with effect analysis.

A language that develops in such a manner is going to encounter what some might
term as 'instability' via numerous backwards-compatibility breaking changes.
We consider this a feature, instead we:
* favour designs (language or API) that are resilient in the face of change
* employ tools that automatically migrate legacy code or assist in migration
* not ossify poor choices and be honest that we can't make such guarantees

Popular languages are maintained through incredible amounts of funding from
various entities; we do not see, nor seek, this happening for us.
Alternatively, there are a number of languages that require unhealthy amounts
of free labour from a few, we're not interested in that either. Instead as is
described this language will focus on practitioners able to affect their tools
and community.

## Near-Term Development

![](./doc/fixup_roadmap.png)

The current and key areas of development are as follows:
1. spec - clarify the language specification and memorialize it in tests
2. nkError - replace `localError` etc approach with an AST (`nkError`) one
3. comments - incrementally document compiler source for easier learning

There are more, the above have been carefully chosen based on the direction of
the language; moreover, their impact is far beyond as described and the
intention is to create a virtuous cycle, examples:

* clarifying the language specification will identify bugs and design flaws that
  in turn will be fixed.
* changes introduced via nkError result in more pure code (`func`) as control-
  flow and effects are no longer intertwined; lead to bug and language
  design fixes due to a broad audit, ease compiler as a library usage for tools

## Community

Presently this repository is our community hub, we'll introduce something more
interactive as things grow. At this time our community is small and our ability
to support users is minimal. As such our attention and efforts are reserved for
those who are able and eager to collaborate on improving the compiler and
associated tools.

If you'd like to participate an easy way would be a pull request, even if it's
simply documentation, that can act as an introduction.

## Compiling

The compiler currently officially supports the following platform and
architecture combinations:

  * Windows (Windows XP or greater) - x86 and x86_64
  * Linux (most, if not all, distributions) - x86, x86_64, ppc64 and armv6l
  * Mac OS X (10.04 or greater) - x86, x86_64, ppc64 and Apple Silicon (based on the ARM64 architecture)

More platforms are supported, however, they are not tested regularly and they
may not be as stable as the above-listed platforms.

Compiling the compiler is quite straightforward if you follow these steps:

First, the C source of an older version of the compiler is needed to
bootstrap the latest version because the compiler itself is written in the
programming language. Those C sources are available within the
[``nim-lang/csources_v1``][csources-v1-repo] repository.

Next, to build from source you will need:

  * A C compiler such as ``gcc`` 3.x/later or an alternative such as ``clang``,
    ``Visual C++`` or ``Intel C++``. It is recommended to use ``gcc`` 3.x or
    later.
  * Either ``git`` or ``wget`` to download the needed source repositories.
  * The ``build-essential`` package when using ``gcc`` on Ubuntu (and likely
    other distros as well).
  * On Windows MinGW 4.3.0 (GCC 8.10) is the minimum recommended compiler.
  * Nim hosts a known working MinGW distribution:
    * [MinGW32.7z](https://nim-lang.org/download/mingw32.7z)
    * [MinGW64.7z](https://nim-lang.org/download/mingw64.7z)

**Windows Note: Cygwin and similar POSIX runtime environments are not supported.**

Then, if you are on a \*nix system or Windows, the following steps should compile
Nim from source using ``gcc``, ``git``, and the ``koch`` build tool.

**Note: The following commands are for the development version of the compiler.**

First, get the compiler from github:

```
git clone https://github.com/nim-works/nim-works.git
cd nim-works
```

Next, run the appropriate build shell script for your platform:

* `build_all.sh` (Linux, Mac)
* `build_all.bat` (Windows)

Finally, once you have finished the build steps (on Windows, Mac, or Linux) you
should add the ``bin`` directory to your PATH.

## Koch

``koch`` is the build tool used to build various parts of Nim and to generate
documentation and the website, among other things. The ``koch`` tool can also
be used to run the Nim test suite.

Assuming that you added Nim's ``bin`` directory to your PATH, you may execute
the tests using ``./koch tests``. The tests take a while to run, but you
can run a subset of tests by specifying a category (for example
``./koch tests cat async``).

For more information on the ``koch`` build tool please see the documentation
within the [doc/koch.rst](doc/koch.rst) file.

## Contributing

Before you start contributing, you should familiarize yourself with the
following repository structure:

* ``bin/``, ``build/`` - these directories are empty, but are used when Nim is built.
* ``compiler/`` - the compiler source code. Also includes nimfix, and plugins within
  ``compiler/nimfix`` and ``compiler/plugins`` respectively.
* ``nimsuggest`` - the nimsuggest tool that previously lived in the [``nim-lang/nimsuggest``][nimsuggest-repo] repository.
* ``config/`` - the configuration for the compiler and documentation generator.
* ``doc/`` - the documentation files in reStructuredText format.
* ``lib/`` - the standard library, including:
    * ``pure/`` - modules in the standard library written in pure Nim.
    * ``impure/`` - modules in the standard library written in pure Nim with
    dependencies written in other languages.
    * ``wrappers/`` - modules that wrap dependencies written in other languages.
* ``tests/`` - contains categorized tests for the compiler and standard library.
* ``tools/`` - the tools including ``niminst`` and ``nimweb`` (mostly invoked via
  ``koch``).
* ``koch.nim`` - the tool used to bootstrap Nim, generate C sources, build the website,
  and generate the documentation.

If you are not familiar with making a pull request using GitHub and/or git, please
read [this guide][pull-request-instructions].

Ideally, you should make sure that all tests pass before submitting a pull request.
However, if you are short on time, you can just run the tests specific to your
changes by only running the corresponding categories of tests. Travis CI verifies
that all tests pass before allowing the pull request to be accepted, so only
running specific tests should be harmless.
Integration tests should go in ``tests/untestable``.

## License
MIT

[nim-site]: https://nim-lang.org
[csources-v1-repo]: https://github.com/nim-lang/csources_v1
