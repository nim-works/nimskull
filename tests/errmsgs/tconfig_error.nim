discard """
  description: '''
  Test that a config file with invalid option will yield an error with
  non-zero exit code.

  Ref https://github.com/nim-works/nimskull/issues/1112
  '''
  errormsg: "Invalid command line option - totally_valid_config_option"
  file: "tconfig_error.nim.cfg"
"""
