discard """
  targets: "cpp"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/4834
    cpp codegen: broken variable definition in defer outside a proc definition
'''
"""

block:
  defer:
    let x = 0


proc main() =
  block:
    defer:
      raise newException(Exception, "foo")

doAssertRaises(Exception):
  main()