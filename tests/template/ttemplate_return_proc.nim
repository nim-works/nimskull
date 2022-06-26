discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/17433
    proc parameter in return type expression is not gensym'd inside templates
  . When using a (proc) parameter in the return type inside a template,
    it is not gensym'd
  . https://github.com/saem/Nim/commit/e9ad9ae0dedcff7661b93b23af12a8de69fe6289
    Fixes nim-lang#17433; gensym callDef return in templ body
  . Inside template bodies, ensure return types referencing a param are
    replaced. This helps guarantee that return parameter analysis happens after
    argument analysis.
'''
"""
from std/macros import expandMacros

proc bar(a: typedesc): a = default(a)
doAssert bar(float) == 0.0
doAssert bar(string) == ""

template main =
  proc baz(a: typedesc): a = default(a)
  doAssert baz(float) == 0.0
main()

