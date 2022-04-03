discard """
  output: "works!"
"""
# this tests the new overflow literals

var
  i: int
i = int(0xffffffff'i32)
when defined(cpu64):
  if i == -1:
    echo "works!"
  else:
    echo "broken!"
else:
  if i == -1:
    echo "works!"
  else:
    echo "broken!"

#OUT works!
