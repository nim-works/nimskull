discard """
  targets: "c cpp"
  errormsg: "illegal capture 'v'"
  line: 8
"""

proc outer(v: int) =
  proc b {.nimcall.} = echo v
  b()
outer(5)
