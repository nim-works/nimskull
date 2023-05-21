discard """
  description: "Specifies basic echo operation used for testing."
  output: '''
  Hello, World!

'''
# the spec parser handles the triple single quotes, they behave like triple
# double quotes but they work in here.
"""

echo "Hello, World!" # prints the message and appends a new line
echo ""              # appends a new line