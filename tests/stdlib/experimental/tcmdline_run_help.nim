discard """
  description: "cmdline.run should exit cleanly on help"
  output: '''
Usage: help-test [OPTIONS]

Options:
  --help  display help message
'''
  joinable: false
"""

import experimental/cmdline

var cli = commandBuilder(string)
  .name("help-test")
  .initCli()

cli.addHelpFlag()

discard cli.run(@["--help"])

unreachable("should already be terminated")
