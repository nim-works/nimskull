discard """
  description: "Regression test for relative imports"
"""

import ./[mbar]
import ./[mbaz, mfoo]
import ./[mqux1 as mqux]