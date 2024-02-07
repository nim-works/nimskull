<div id="top"></div>

<br />

<div align="center">
  <a href="https://github.com/nim-works/nimskull">
    <img src="https://raw.githubusercontent.com/nim-lang/assets/master/Art/logo-crown.png" height="80px"/>
  </a>

  <h3 align="center">Nimskull</h3>
  <p align="center">
    The Nimskull compiler, stdlib, tools, and documentation repository.
    <br />
    <br />
    <a href="https://github.com/nim-works/nimskull/blob/devel/CODE_OF_CONDUCT.md">Code of Conduct</a>
    ·
    <a href="https://github.com/nim-works/nimskull/blob/devel/ETHOS.md">Code of Ethics</a>
    ·
    <a href="https://nim-works.github.io/nimskull/index.html">Documentation</a>
  </p>

[![Matrix](https://img.shields.io/badge/matrix-nim--works-success?style=flat&logo=matrix)][nim-works-matrix]
[![IRC](https://img.shields.io/badge/chat-%23nimworks%20on%20libera.chat-brightgreen?style=flat)](https://web.libera.chat/#nimworks)
[![IRC #nimworks-dev](https://img.shields.io/badge/chat-%23nimworks--dev%20on%20libera.chat-brightgreen?style=flat)](https://web.libera.chat/#nimworks-dev)

</div>

<br />

This repository contains the Nimskull compiler, stdlib, tools, and documentation.

## About the Project

Nimskull (temporary name) is a *statically typed* *structured* programming
language to create software (itself included) that is sustainable, it aims to be:

- *Safe:* statically typed, nil safe, with structured approach to resources and
  concurrency
- *Scalable:* target a variety of hardware architectures, with zero or low cost
  abstractions to run in constrained environments.
- *Adaptable:* producing native executables, running via the built-in VM, or
  using a JS runtime.
- *Evolving:* developed and maintained by its community of users, with a self-
  hosted compiler, support for metaprogramming to safely attempt language
  extension outside of the core, and support code migration to avoid legacy.

The project was started as a fork of [Nim][nim-site] as it provides a bootstrapped
compiler as a starting point. The overall language will be evolved into something
entirely different and incompatible.

We are currently working on the first phase of this, by slimming down the
language and compiler to a workable core and increasing compiler development
productivity. The following phase will starting with one of the following
possible features:
- Introduce Continuation Passing Style transform and Structured Concurrency
  into the language, this will undoubtedly lead to dramatic changes in memory
  management and FFI
- Ease Data Oriented Design through memory regions to support the common handle
  instead of reference approach

## Near-Term Development

![](./doc/fixup_roadmap.png)

For updates on the progress refer to the [milestones](https://github.com/nim-works/nimskull/milestones).
The current ones are used as a way to track progress on a topic, rather than
for representing fixed milestones. The old, out-of-date thread about roadmap
progress can be found [here](https://github.com/nim-works/nimskull/discussions/142?sort=new).

The current and key areas of development are as follows:

1. decouple the data types used by the different compilation stages
2. simplify the code generators - perform much of the transformation and lowering
   via passes over the mid-end IR ([Project](https://github.com/orgs/nim-works/projects/12))
3. improve tests - core specification as tests (see `slim the core` below).
   Reorganize existing tests. ([Project](https://github.com/nim-works/nimskull/projects/2))
4. nkError/tyError/skerror - replace `localError` etc approach with an AST
   (`nkError`) one ([Project](https://github.com/nim-works/nimskull/projects/1))
5. comments - incrementally document compiler source for easier learning
6. slim the core - remove dialects, backwards compatibility, etc ([Discussion](https://github.com/nim-works/nimskull/discussions/289))

There are more, the above have been carefully chosen based on the direction of
the language; moreover, their impact goes beyond what's been described and
intends to create a virtuous cycle. Examples:

* clarifying the language specification will identify bugs and design flaws that
  in turn will be fixed.
* moving decision making out of the code generators will make behaviour of the
  backends consistent with each other
* changes introduced via nkError result in more pure code (`func`) as control-
  flow and effects are no longer intertwined; lead to bug and language
  design fixes due to a broad audit, ease compiler as a library usage for tools

<p align="right">(<a href="#top">back to top</a>)</p>

## Community

Currently, this repository and our [matrix/irc][nim-works-matrix] are our primary community hubs; we'll introduce more as things grow. At this time our community is small but
passionate We welcome any who are able and eager to collaborate on improving the compiler and associated tools.

Check the [FAQ](#FAQ), or [Project Board](https://github.com/nim-works/nimskull/projects) for an idea of where to help.

There's plenty to be done, and we appreciate even the smallest contribution to
documentation! We look forward to seeing introductions and pull requests!

<p align="right">(<a href="#top">back to top</a>)</p>

## Compiling

The compiler currently aims to support the following platform and
architecture combinations:

  * Linux (most, if not all, distributions) - x86, x86_64, ppc64 and armv6l
  * Mac OS X (10.04 or greater) - x86, x86_64, ppc64 and Apple Silicon (based on the ARM64 architecture)
  * Windows (Windows XP or later) - x86 and x86_64

Other platforms may work but aren't regularly tested.

Compiling the compiler is quite straightforward if you follow these steps:

<details>
  <summary>Show</summary>
  <br />

To build from source you will need:

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
Nimskull from source using ``gcc``, ``git``, and the ``koch`` build tool.

```bash
git clone https://github.com/nim-works/nimskull.git
cd nimskull
./koch.py boot -d:release
./koch.py tools -d:release
```

Finally, once you have finished the build steps (on Windows, Mac, or Linux) you
should add the ``bin`` directory to your PATH.

</details>

<p align="right">(<a href="#top">back to top</a>)</p>

## Contributing

Contributing is simplified thanks to our [contribution guide](https://nim-works.github.io/nimskull/contributing.html).
Right now, the easiest and most important contribution is test suite improvement, also
[described](https://nim-works.github.io/nimskull/contributing.html#writing-or-improving-tests) in the guide.

## Koch

``koch`` is the build tool used to build various parts of Nim and to generate
documentation, among other things. The ``koch`` tool can also
be used to run the Nim test suite.

<details>
<summary>Show</summary>

You may execute the tests using ``./koch.py tests``. The tests take a while to
run, but you can run a subset of tests by specifying a category (for example
``./koch.py tests cat lang``).

For more information on the ``koch`` build tool please see the documentation
within the [doc/koch.rst](doc/koch.rst) file.

<p align="right">(<a href="#top">back to top</a>)</p>

</details>

## Direction

A language (community, compiler, etc) that is *sustained* through the
*collective* efforts of its practitioners and their *diverse* backgrounds.

Attracting practitioners with diversity of experience and perspectives
requires a language with broad applicability, from *Web* to *Systems*
*Programming* all the while remaining *efficient*.

Onboarding practitioners requires a language that is familiar enough to get
started in terms of syntax and initial concepts such as *structured and*
*modular programming*.

Supporting practitioner-driven innovation requires a language that allows for
experimentation without necessarily being an expert in all aspects of language
development. Compile time facilities integrated into the language, such as
*compile time evaluation* and a *macro system* provide an extension sandbox.

Practitioner collaborating and combining their software is assisted by a
*static type system* that supports local inference, tuples, sum, and generic types,
along with effect analysis.

A language that develops in such a manner is going to encounter what some might
term as 'instability' via numerous backwards incompatible changes.
We consider this a feature, instead we:
* favour designs (language or API) that are resilient in the face of change
* employ tools that automatically migrate legacy code or assist in migration
* not cement poor choices and be honest that we can't make such guarantees

Popular languages are maintained through incredible amounts of funding from
various entities; we do not see, nor seek, this happening for us.
Alternatively, there are a number of languages that require unhealthy amounts
of free labour from a few, we're not interested in that either. Instead as is
described this language will focus on practitioners able to affect their tools
and community.

<p align="right">(<a href="#top">back to top</a>)</p>


## FAQ
<details>
<summary class"blue">Why start with Nim?</summary>
</br>

It's convenient. Creating a compiler from scratch is labour intensive and the
existing contributors are already familiar with the current code base. We chose
to evolve it.

</details>

<details>
<summary class="blue">Will this break my Nim code?</summary>
</br>

This project aims to become a different programming language, if you want Nim go use that.

</details>

<details>
<summary class="blue">Can I help somehow?</summary>
</br>

There is lots to do, and we're very interested in people contributing to the compiler.
First step is getting a development environment setup, then join the
[matrix chat][nim-works-matrix] and introduce yourself. It's a small community so time
zones might not align, so please be patient.

We're presently reworking much of the compiler, removing dialects and half-baked
features to end up with a slim down core. Some areas of contribution:

- [reworking the internal error handling](https://github.com/nim-works/nimskull/projects)
- improving the compiler internal [debugging and tracing tools](https://nim-works.github.io/nimskull/debug.html)
- ["language spec as tests" effort](https://github.com/nim-works/nimskull/projects/2)

</details>

<details>
<summary class="blue">Any chat room on matrix/irc/discord?</summary>
</br>

Yes! Feel free to join us on our [nim-works channel][nim-works-matrix]! Please have a read of our [Code of Conduct](https://github.com/nim-works/nimskull/blob/devel/CODE_OF_CONDUCT.md)

</details>
<p align="right">(<a href="#top">back to top</a>)</p>

## License
MIT

[nim-site]: https://nim-lang.org
[csources-v1-repo]: https://github.com/nim-works/csources_v1
[nim-works-matrix]: https://matrix.to/#/#nimworks:envs.net?client=element.io
