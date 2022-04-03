discard """
  output: '''1
2
3
100'''
"""

template hygienic(val) =
  var x = val
  echo x

var x = 100

hygienic 1
hygienic 2
hygienic 3

echo x

