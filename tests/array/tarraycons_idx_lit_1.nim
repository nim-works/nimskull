discard """
  errormsg: "size of array exceeds range of index type 'range 1..2(Color)' by 3 elements"
  line: 7
  labels: "enum array constructor index"
"""
type Color = enum Red, Green, Blue
let y = [Green: 0, 1, 2, 3, 4]
