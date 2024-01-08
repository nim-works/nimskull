discard """
  output: "a\0b"
"""

# the string differ after the NULL byte
echo "a\0c"