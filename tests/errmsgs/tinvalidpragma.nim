discard """
  cmd: "nim check $options $file"
  action: reject
  nimout: '''
tinvalidpragma.nim(10, 20) Error: invalid pragma: warning[XYZ]: off
tinvalidpragma.nim(11, 15) Error: invalid pragma: warning[XYZ]: off
'''
"""

{.push warning[XYZ]: off.}
{.warning[XYZ]: off, push warning[XYZ]: off.}
