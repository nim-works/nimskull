================================
          NimScript
================================

.. default-role:: code
.. include:: rstcommon.rst

Strictly speaking, `NimScript` is the subset of |Nimskull| that can be 
evaluated by |Nimskull|'s builtin virtual machine (VM). This VM is used for
|Nimskull|'s compiletime function evaluation features.

**Note**: nims for configuration is going to be removed.


Intro to Scripting
==================

NimScript can be used directly as a portable replacement for Bash and
Batch files. Use `nim myscript.nims`:cmd: to run ``myscript.nims``. For example,
installation of a program could be accomplished with this simple script:

.. code-block:: nim

  mode = ScriptMode.Verbose

  var id = 0
  while dirExists("foo" & $id):
    inc id

  exec "git clone https://github.com/bar/foo.git" & $id

  withDir "foo" & $id & "/src":
    exec "nim c foo"

  mvFile "foo" & $id & "/src/foo".toExe, "bin/foo".toExe

On Unix, you can also use the shebang `#!/usr/bin/env nim`, as long as your filename
ends with ``.nims``:

.. code-block:: nim

  #!/usr/bin/env nim
  mode = ScriptMode.Silent

  echo "hello world"

Use `#!/usr/bin/env -S nim --hints:off` to disable hints.


Dry-Run and Debugging
=====================

Defining `scriptmode` on the command-line like so:
`nim -d:scriptmode foo.nims`:cmd: ensures that it's run in `WhatIf` mode, where
effects such as shell execution, file IO, and so on are logged to the console
and never ran. There are additional options, see `nimscript <nimscript.html>`_
for details.



Extensions & Limitations
========================

Since the VM is limited in what it can do for security reasons, a set of procs
allowing for file IO, shell execution, and so on are availabe in the
`nimscript <nimscript.html>`_ module. There is also support for some OS native
package managers in the `distros <distros.html>`_ module.

NimScript is subject to some limitations caused by the implementation of the VM
(virtual machine):

* Nim's FFI (foreign function interface) is not available in NimScript. This
  means that any stdlib module which relies on `importc` can not be used in
  the VM.

* `ptr` operations are are hard to emulate with the symbolic representation
  the VM uses. They are available and tested extensively but there are bugs left.

* `var T` function arguments rely on `ptr` operations internally and might
  also be problematic in some cases.

* More than one level of `ref` is generally not supported (for example, the type
  `ref ref int`).

* Multimethods are not available.

* `random.randomize()` requires an `int64` explicitly passed as argument, you
  *must* pass a Seed integer.


Standard library modules
========================

At least the following standard library modules are available:

* `macros <macros.html>`_
* `os <os.html>`_
* `strutils <strutils.html>`_
* `math <math.html>`_
* `distros <distros.html>`_
* `sugar <sugar.html>`_
* `algorithm <algorithm.html>`_
* `base64 <base64.html>`_
* `bitops <bitops.html>`_
* `colors <colors.html>`_
* `complex <complex.html>`_
* `htmlgen <htmlgen.html>`_
* `httpcore <httpcore.html>`_
* `lenientops <lenientops.html>`_
* `mersenne <mersenne.html>`_
* `options <options.html>`_
* `parseutils <parseutils.html>`_
* `punycode <punycode.html>`_
* `random <punycode.html>`_
* `stats <stats.html>`_
* `strformat <strformat.html>`_
* `strmisc <strmisc.html>`_
* `strscans <strscans.html>`_
* `unicode <unicode.html>`_
* `uri <uri.html>`_
* `std/editdistance <editdistance.html>`_
* `std/wordwrap <wordwrap.html>`_
* `std/sums <sums.html>`_
* `parsecsv <parsecsv.html>`_
* `parsecfg <parsecfg.html>`_
* `parsesql <parsesql.html>`_
* `xmlparser <xmlparser.html>`_
* `htmlparser <htmlparser.html>`_
* `ropes <ropes.html>`_
* `json <json.html>`_
* `parsejson <parsejson.html>`_
* `strtabs <strtabs.html>`_
* `unidecode <unidecode.html>`_

In addition to the standard Nim syntax (`system <system.html>`_ module),
NimScripts support the procs and templates defined in the
`nimscript <nimscript.html>`_ module too.

See also:
* `Check the tests for more information about modules compatible with NimScript. <https://github.com/nim-works/nimskull/blob/devel/tests/test_nimscript.nims>`_


Benefits
========

Cross-Platform
--------------

It is a cross-platform scripting language that can run where |Nimskull| can
run, e.g. you can not run Batch or PowerShell on Linux or Mac,
the Bash for Linux might not run on Mac,
there are no unit tests tools for Batch, etc.

NimScript can detect on which platform, operating system,
architecture, and even which Linux distribution is running on,
allowing the same script to support a lot of systems.

See the following (incomplete) example:

.. code-block:: nim

  import std/distros

  # Architectures.
  if defined(amd64):
    echo "Architecture is x86 64Bits"
  elif defined(i386):
    echo "Architecture is x86 32Bits"
  elif defined(arm):
    echo "Architecture is ARM"

  # Operating Systems.
  if defined(linux):
    echo "Operating System is GNU Linux"
  elif defined(windows):
    echo "Operating System is Microsoft Windows"
  elif defined(macosx):
    echo "Operating System is Apple OS X"

  # Distros.
  if detectOs(Ubuntu):
    echo "Distro is Ubuntu"
  elif detectOs(ArchLinux):
    echo "Distro is ArchLinux"
  elif detectOs(Debian):
    echo "Distro is Debian"


Uniform Syntax
--------------

The syntax, style, and rest of the ecosystem is the same as for compiled Nim,
that means there is nothing new to learn, no context switch for developers.


Powerful Metaprogramming
------------------------

NimScript can use Nim's templates, macros, types, concepts, effect tracking
system, and more, you can create modules that work in both a compiled or
scripting context.

`func` will still check for side effects, `debugEcho` also works as expected,
making it ideal for functional scripting metaprogramming.

This is an example of a third party module that uses macros and templates to
translate text strings on unmodified NimScript:

.. code-block:: nim

  import nimterlingua
  nimterlingua("translations.cfg")
  echo "cat"  # Run with -d:RU becomes "kot", -d:ES becomes "gato", ...

translations.cfg

.. code-block:: none

  [cat]
  ES = gato
  IT = gatto
  RU = kot
  FR = chat


Graceful Fallback
-----------------

Some features of compiled |Nimskull| may not work on NimScript,
but often a graceful and seamless fallback degradation is used.

See the following NimScript:

.. code-block:: nim

  if likely(true):
    discard
  elif unlikely(false):
    discard

  proc foo() {.compiletime.} = echo NimVersion

  static:
    echo CompileDate


`likely()`, `unlikely()`, `static:` and `{.compiletime.}`
will produce no code at all when run on NimScript,
but still no error nor warning is produced and the code just works.

Evolving Scripting language
---------------------------

NimScript evolves together with |Nimskull|,
`occasionally new features might become available on NimScript <https://github.com/nim-lang/Nim/pulls?utf8=%E2%9C%93&q=nimscript>`_ ,
adapted from compiled |Nimskull| or added as new features on both.


DevOps Scripting
----------------

You can use NimScript to deploy to production, run tests, build projects, do benchmarks,
generate documentation, and all kinds of DevOps/SysAdmin specific tasks.
