discard """
  description: "Regression test for relative imports"
"""

# relative to current directory
import ./[mbar]
import ./[mbaz, mfoo]
import ./[mqux1 as mqux]
import ./mqux2