discard """
description: "Tests that nested errors are not being lost or output in the wrong order"
cmd: '''nim check $file'''
action: reject
nimout: '''
tnested_errors.nim(15, 12) Error: undeclared identifier: 'b'
tnested_errors.nim(16, 11) Error: undeclared identifier: 'b'
'''
"""





var a = if b:
          b
        else:
          1
