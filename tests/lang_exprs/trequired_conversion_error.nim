discard """
  description: '''
    Ensure that ref conversions that ultimately don't result in a differently-
    typed still result in a conversion error
  '''
  outputsub: "invalid object conversion"
  exitcode: 1
"""

type
  A = ref object of RootObj
  B = ref object of A

var a = A()
discard A(B(a))