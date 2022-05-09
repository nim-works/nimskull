discard """
  output: "5"
  matrix: "--hints:on -d:release"
  ccodecheck: "'/*PROGMEM*/ myLetVariable = {'"
  targets: "c"
"""

var myLetVariable {.exportc, codegenDecl: "$# /*PROGMEM*/ $#".} = [1, 2, 3]

myLetVariable[0] = 5
echo myLetVariable[0]
