discard """
  errormsg: "invalid order in array constructor"
  file: "tarraycons_enum.nim"
  line: 25
  labels: "enum array constructor"
  description: '''
    . When specifying an array, it is necessary to inform its static size.
    . Since enums are statically sized , it is possible to use them to specify
      the array size.
      When doing so, however, it is necessary for the array literal to be
      created by specifying the enum values in order of their creation.
    . In this test, we try to create the array out of enum order
      ( by swapping the eB and eC ), which should throw an error.
  '''
"""

type
  TEnum = enum
    eA, eB, eC, eD, eE, eF

const

  myMappiwng: array[TEnum, int] = [
    eA: 1,
    eC: 2,
    eB: 3,
    eD: 4,
    eE: 5,
    eF: 6
  ]


