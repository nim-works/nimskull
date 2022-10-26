discard """
  cmd: "nim check $options $file"
  action: reject
  nimout: '''
tinvalidpragma.nim(11, 20) Error: invalid pragma: warning[XYZ]: off
tinvalidpragma.nim(12, 15) Error: invalid pragma: warning[XYZ]: off
tinvalidpragma.nim(14, 22) Error: invalid pragma: "expression" {.invalid.}
'''
"""

{.push warning[XYZ]: off.}
{.warning[XYZ]: off, push warning[XYZ]: off.}

discard "expression" {.invalid.}
