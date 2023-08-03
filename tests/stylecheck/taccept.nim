discard """
  matrix: "--stylechecks --styleCheck:error"
"""

block consts_can_be_either_upper_or_lower:
  const a = 1
  const B = 1
  doAssert a == 1
  doAssert B == 1

block single_letter_uppper_case_non_types_are_allowed:
  proc L() =
    discard
  L()

block fields_and_variables_start_with_lower_case:
  type
    Name = object
      id: int

  template hello =
    var iD = "string"
    var name: Name
    doAssert name.id == 0
    doAssert iD == "string"

  hello()
