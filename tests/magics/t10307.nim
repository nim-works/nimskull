discard """
  targets: "c cpp"
  matrix: "-d:useGcAssert"
  joinable: false
"""

proc someProc(x:bool): cstring =
  var res:string = ""
  if x:
    res = "yes"
  GC_ref(res)
  result = res

doAssert someProc(true) == "yes".cstring
doAssert someProc(false) == "".cstring
