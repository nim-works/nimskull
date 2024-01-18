discard """
  cmd: "nim check $file"
  action: reject
  nimout: '''
t10735.nim(12, 5) Error: 'let' symbol requires an initialization
t10735.nim(13, 10) Error: undeclared identifier: 'pos'
t10735.nim(13, 9) Error: selector must be of an ordinal type, float, or string
'''
  joinable: false
"""

let buf: cstring
case buf[pos]
else:
  case buf[pos]
