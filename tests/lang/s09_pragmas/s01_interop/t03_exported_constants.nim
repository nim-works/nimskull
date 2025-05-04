discard """
  targets: "c"
  description: '''
    Constants marked with the `exportc` pragma are always emitted, even if
    not used anywhere in the |NimSkull| code
  '''
  ccodecheck: "theConst"
"""

const theConst {.exportc.} = 1