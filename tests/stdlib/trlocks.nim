discard """
  action: "compile"
  # Disallow joining to ensure it can compile in isolation.
  # See #15584
  joinable: false
  matrix: "--threads:on"
  targets: "!js"
"""

# bugfix #15584

import rlocks

var r: RLock
r.initRLock()
