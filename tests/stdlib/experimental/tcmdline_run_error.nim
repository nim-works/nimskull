discard """
  description: "cmdline.run should exit on error"
  output: '''
error: unexpected positional argument 'invalid' found

Usage: error-test [OPTIONS]
'''
  exitcode: 1
  joinable: false
"""

import experimental/cmdline

var cli = commandBuilder(string)
  .name("error-test")
  .initCli()

discard cli.run(@["invalid"])

unreachable("should already be terminated")
