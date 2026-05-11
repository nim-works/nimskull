discard """
  disabled: "posix"
"""

# bug 10952, UNC paths
import std/os

doAssert r"\\hostname\foo\bar" / "baz" == r"\\hostname\foo\bar\baz"
doAssert r"\\?\C:\foo" / "bar" == r"\\?\C:\foo\bar"
