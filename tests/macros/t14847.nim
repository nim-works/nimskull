discard """
  output: "98"
  description: '''
create new proc with existing proc's body, previously this resulted in an error
due to stale information.
'''
"""
import macros

#bug #14847
proc hello*(b: string) =
  echo b

macro dispatch(pro: typed, params: untyped): untyped =
  var impl = pro.getImpl
  let id = ident(pro.strVal & "_coverage")
  impl[0] = id
  let call = newCall(id, params)

  result = newStmtList()
  result.add(impl)
  result.add(call)

dispatch(hello, "98")
