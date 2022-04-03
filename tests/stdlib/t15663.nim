discard """
  targets: "c cpp"
  matrix: "--gc:arc"
  output: "Test"
"""

let ws = newWideCString("Test")
echo ws