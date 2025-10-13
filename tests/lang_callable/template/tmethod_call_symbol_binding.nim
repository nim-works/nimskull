discard """
  description: '''
    Ensure that non-overloaded symbols from a template's definition scope are
    bound early when using the method-call syntax.

    Derived from https://github.com/nim-works/nimskull/issues/1292.
  '''
"""

import mmethod_call_symbol_binding

doAssert templ(1) == 1
