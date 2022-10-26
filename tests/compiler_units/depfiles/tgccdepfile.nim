discard """
description: "can write a GCC-format depfile"
output: '''
out\ binary: \
	module1.nim \
	module\#.nim \
	package/module.nim \
'''
"""

import
  compiler/front/depfiles

const paths = [
  "module1.nim",
  "module#.nim",
  "package/module.nim"
]
stdout.writeGccDepfile("out binary", paths)
