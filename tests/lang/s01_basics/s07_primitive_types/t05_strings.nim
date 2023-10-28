discard """
description: '''
String type related tests, covering key properties regarding:
- key memory allocation, raw byte handling, and mutable
- utf-8 encoding by default, but not runtime enforced
- key operations: length, contcatenation, etc...
- indexing is per byte (char)
- lexographic comparison
'''
"""

block strings_valid_from_empty_string:
  let
    s = ""
    length = s.len
    other = ""
  doAssert s == "",     "equals an equivalent literal"
  doAssert s == other,  "equals an equivalent runtime value"
  doAssert length == 0, "empty string has a length of 0"

block string_vm_and_runtime_handling:
  # compare compile time (vm) and run time (platform/plt) handling of strings
  # and null bytes
  const
    vmStr = "test\0test"
    vmLen = vmStr.len
    vmOther = "test\0test"
  let
    pltStr = "test\0test"
    pltLen = pltStr.len
    pltOther = "test\0test"

  doAssert pltStr == vmStr,    "equals the equivalent literal VM string"
  doAssert pltStr == pltOther, "equals lexographical equivalent runtime value"
  doAssert vmStr == vmOther,   "equals lexographical equivalent VM value"
  doAssert pltLen == 9,        "string length excludes last null byte"
  doAssert pltLen == vmLen,    "VM and runtime length are the same"

block string_concatenation:
  # indexing, assignment as copy, and mutation
  var strA = "abd"
  let strB = strA

  doAssert strA == strB,   "strings should start out the same"
  doAssert strA[2] == 'd', "indexing into a string"
  strA[2] = 'c' # change the last letter, strA and strB are no longer equal
  doAssert strA != strB,   "strings copy on assignment, therefore not equal"
  doAssert strA == "abc",  "after changing strings are lexographically equal"

block string_unicode_handling:
  let s = "\u0101"
  doAssert s[0] == '\xC4', "string indexing results in char/byte - first"
  doAssert s[1] == '\x81', "string indexing results in char/byte - second"

block string_copy_on_assignment:
  # indexing, assignment as copy, and mutation
  let strA = "ab" & "c"

  doAssert strA == "abc", "strings are concatenated and equal"
