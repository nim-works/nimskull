discard """
  errormsg: "cannot call method eval at compile time"
  line: 26
  description: '''
    . From https://github.com/nim-lang/Nim/issues/2574
      const expression calling methods not reported as error by the compiler
    . Of course it is not expected to work, since the documentation clearly
      states that
      "compile time evaluation or dead code elimination do not work with methods".
    . However it should perhaps cause the compiler to output an error message,
      since the user might be importing the methods from another module and thus
      the user might not know he is calling methods (instead of procedures).
    . https://github.com/LemonBoy/Nim/commit/c2e5faf959f006af67d1a054a92096d63eae192a
      The VM cannot call methods
  '''
"""

type
  PExpr = ref object of RootObj

method eval(e: PExpr): int =
  discard

static:
  let x = PExpr()
  discard x.eval