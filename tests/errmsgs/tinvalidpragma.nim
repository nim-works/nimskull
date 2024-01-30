discard """
  cmd: "nim check $options $file"
  action: reject
  nimout: '''
tinvalidpragma.nim(12, 23) Error: invalid pragma: warning[XYZ, 1]: off
tinvalidpragma.nim(13, 30) Warning: unknown warning: 'XYZ' [UnknownWarning]
tinvalidpragma.nim(13, 10) Error: invalid pragma: warning[XYZ]
tinvalidpragma.nim(15, 22) Error: invalid pragma: "expression" {.invalid.}
'''
"""

{.push warning[XYZ, 1]: off.}
{.warning[XYZ], push warning[XYZ]: off.}

discard "expression" {.invalid.}
