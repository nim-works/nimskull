discard """
  output: '''
{'a', 'b'}
'''
"""

var x = new(ref set[char])
var y = new(ref set[char])
x[] = {'a'}
y[] = {'b'}
echo x[] + y[]
