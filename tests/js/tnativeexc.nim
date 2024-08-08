discard """
  action: "run"
"""

import jsffi

proc throwError() {.importjs: "throw new Error('a new error')".}

# Can catch JS exceptions
try:
  throwError()
except JsError as e:
  doAssert e.message == "a new error"
except:
  doAssert false

proc parse() {.importjs: "JSON.parse(';;')".}

# Can distinguish different exceptions
try:
  parse()
except JsEvalError:
  doAssert false
except JsSyntaxError as se:
  doAssert se.message == "Unexpected token ; in JSON at position 0"
except JsError as e:
  doAssert false

proc throwSyntaxError() {.importjs: "throw new SyntaxError()".}

# Can catch parent exception
try:
  throwSyntaxError()
except JsError as e:
  discard
except:
  doAssert false
