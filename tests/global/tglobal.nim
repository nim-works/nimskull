discard """
  description: '''
    Tests for globals defined inside procedures via the `.global.` pragma
  '''
  targets: "c js vm"
  output: "in globalaux2: 10\ntotal globals: 2\nint value: 100\nstring value: second"
"""

import globalaux, globalaux2

echo "total globals: ", totalGlobals

globalInstance[int]().val = 100
echo "int value: ", globalInstance[int]().val

globalInstance[string]().val = "first"
globalInstance[string]().val = "second"
echo "string value: ", globalInstance[string]().val

block inline_procedure_with_global:
  # call the inline procedure and make sure that the global was not
  # re-initialized at the start of the current module
  testInline(2)