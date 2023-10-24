discard """
description: '''
Covers numeric literals, in particular defaults and how suffixes are handled
'''
"""

# basic float literals
block:
  let
    a = 0.0
    b = 1.0
    c = -1.0
    d = 2e1
  doAssert typeof(a) is float, "ending in `.`number makes it a `float`, 0.0"
  doAssert typeof(b) is float, "ending in `.`number makes it a `float`, 1.0"
  doAssert typeof(c) is float, "ending in `.`number makes it a `float`, -1.0"
  doAssert typeof(d) is float, "ending in `e`exponent makes it a `float`"

block:
  let
    a = 0'f
    b = 1'f
    c = -1'f
  doAssert typeof(a) is float32, "0 float 32"
  doAssert typeof(b) is float32, "1 float 32"
  doAssert typeof(c) is float32, "-1 float 32"

block:
  let
    a = 0'd
    b = 1'd
    c = -1'd
  doAssert typeof(a) is float64, "0 float 64"
  doAssert typeof(b) is float64, "1 float 64"
  doAssert typeof(c) is float64, "-1 float 64"

block:
  let
    a = 0'f32
    b = 1'f32
    c = -1'f32
  doAssert typeof(a) is float32, "0 float 32"
  doAssert typeof(b) is float32, "1 float 32"
  doAssert typeof(c) is float32, "-1 float 32"

block:
  let
    a = 0'f64
    b = 1'f64
    c = -1'f64
  doAssert typeof(a) is float64, "0 float 64"
  doAssert typeof(b) is float64, "1 float 64"
  doAssert typeof(c) is float64, "-1 float 64"

# xxx: revisit floating point related checks