discard """
description: "non-existent symbol in import from clause errors out"
errormsg: "undeclared identifier: 'doesNotExist'"
line: 8
column: 25
"""

from definitions import doesNotExist