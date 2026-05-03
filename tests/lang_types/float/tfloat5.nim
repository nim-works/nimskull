discard """
output: '''
0 : 0.0
0 : 0.0
0 : 0.0
0 : 0.0
'''
"""

import std/parseutils

var f: float
echo "*".parseFloat(f), " : ", f
echo "/".parseFloat(f), " : ", f
echo "+".parseFloat(f), " : ", f
echo "-".parseFloat(f), " : ", f
