discard """
  errormsg: "Your case statement contains too many branches, consider using if/else instead!"
  description: '''
    . From https://github.com/nim-lang/Nim/issues/8821
      JS codegen can produce extreme switch statements with case a of range
    . Unless you have lots and lots of memory and disk space,
      running nim js isInt32.nim will make your system unusable.
      Nim should refuse to compile harmless looking stupidities like this one.
      For this code on the js backend Nim tries to produce a switch
      statement with 4294967295 case labels
    . The C codegen deals with this appropriately.
    '''
"""

proc isInt32(i: int): bool =
  case i 
  of 1 .. 70000:
    return true
  else:
    return false

discard isInt32(1)
