discard """
  targets: "cpp"
  action: "compile"
  description: '''
  . From https://github.com/nim-lang/Nim/issues/10241
    Invalid C++ code generation when returning discardable var T
  . Nim generates c++ code with unintiated references
  . Its the discardable pragma that causes an issue, this works
    (with more fleshed out importcpp) but using a discardable instead doesn't
  . https://github.com/nim-lang/Nim/commit/15584879b91e14565156ca140eef1dc100cf34c4
    Properly wrap discarded statements Failing to do so lead the codegen to
    emit invalid code sometimes, especially when C++ references were involved.
'''
"""

type
  String* {.importcpp: "std::string", header: "string".} = object

proc initString*(): String
    {.importcpp: "std::string()", header: "string".}

proc append*(this: var String, str: String): var String
    {.importcpp: "append", header: "string", discardable.}

var
  s1 = initString()
  s2 = initString()

s1.append s2