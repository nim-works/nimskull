discard """
  matrix: "-d:danger; -d:release"
  output: '''
a
b
c
'''
"""

echo "a"
when defined(release):
  echo "b"
echo "c"
