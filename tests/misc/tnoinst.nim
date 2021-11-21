discard """
  errormsg: "instantiate 'notConcrete' explicitly"
  line: 12
  knownIssue: "https://github.com/nim-lang/Nim/issues/1708"
  knownIssue: "https://github.com/nim-lang/Nim/issues/871"
"""

proc wrap[T]() =
  proc notConcrete[T](x, y: int): int =
    var dummy: T
    result = x - y

  var x: proc (x, y: T): int
  x = notConcrete


wrap[int]()
