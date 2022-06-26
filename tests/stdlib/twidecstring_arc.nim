discard """
  cmd: "nim c --gc:arc $file"
  output: "Test"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/15663
      (s: WideCString) changes result to repr on --gc:arc
    . At the exit point of proc `$`*(s: WideCString): string the result
      variable is as expected, but immediately after exit of the proc the
      result appears to be transformed into the repr of the
      WideCStringObj object.
    . Interestingly, either a proc or template proxy seems to fix the problem.
    . This is causes by converter.
      This is due to overloading $(from system) for object so
      converter doesn't work.
    . https://github.com/nim-lang/Nim/commit/8e1fa84b0d44bc6fb995992becf659d62b36713a
  '''
"""

let ws = newWideCString("Test")
echo ws
