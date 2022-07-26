discard """
  errormsg: '''Undeclared identifier: 'x'
'tests/errmsgs/mcyclic_import1.nim' -> 'tests/errmsgs/mcyclic_import2.nim' -> 'tests/errmsgs/mcyclic_import1.nim'
'''
  file: "mcyclic_import2.nim"
"""

import mcyclic_import1