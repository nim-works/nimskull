discard """
  output: '''34'''
  targets: "c cpp"
"""

{.compile("cfunction.c", "-DNUMBER_HERE=34").}

proc cfunction(): cint {.importc.}

echo cfunction()
