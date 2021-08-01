discard """
  output: '''hi
bye'''
target: "js"
"""

import asyncjs, times

proc sleepAsync(t: int): Future[void] =
  var promise = newPromise() do(resolve: proc()):
    {.emit: """
    setTimeout(function(){
        `resolve`();
    }, `t`);
    """.}
  result = promise

proc foo() {.async.} =
  echo "hi"
  var s = epochTime()
  await sleepAsync(200)
  var e = epochTime()
  doAssert(e - s > 0.1)
  echo "bye"

discard foo()
