discard """
  description: '''
    Tests the order in which module top-level statements are executed
  '''
  output: '''init_order3
init_order2
init_order1
main
'''
  targets: c cpp js vm
"""

# `minit_order1` imports `minit_order2` which imports `minit_order1` again,
# causing a cyclic import

import minit_order1
import minit_order2
import minit_order3

echo "main"

doAssert value1 == 1
doAssert valueDep1 == 2
doAssert valueDep12 == 3
doAssert value2 == 2
doAssert valueDep2 == 3
doAssert value3 == 3