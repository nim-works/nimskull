discard """
  disabled: "windows"
  targets: "native"
  matrix: "--gc:go"
  joinable: false
  action: "compile"
"""
# bug #11447
echo "Go GC test"
