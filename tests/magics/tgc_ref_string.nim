discard """
  cmd: "nim c -d:useGcAssert $file"
  joinable: false
  description: '''
    . From https://github.com/nim-lang/Nim/issues/10307
      GC_ref on empty string fails with [GCASSERT] incRef: interiorPtr
    . Calling GC_ref() on an empty string when running with -d:useGcAssert
      causes a GCASSERT error (which is different than if the string is
      non-empty).
    . It has been fixed since 0.19.2.
  '''
"""

proc someProc(x:bool): cstring =
  var res:string = ""
  if x:
    res = "yes"
  GC_ref(res)
  result = res

doAssert someProc(true) == "yes".cstring
doAssert someProc(false) == "".cstring

