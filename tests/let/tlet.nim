discard """
  errormsg: "'name' cannot be assigned to"
  line: "10"
"""

echo("What's your name? ")
let name = "foo"
while name == "":
  echo("Please tell me your name: ")
  name = "bar"
