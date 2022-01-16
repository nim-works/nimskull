discard """
  cmd: "nim check --warning=GcUnsafe:off --hints:on --experimental:strictFuncs --experimental:views compiler/nim.nim"
  action: "compile"
"""
