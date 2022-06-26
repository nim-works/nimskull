discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/17385
    Len used before its definition
  . Caused by https://github.com/nim-lang/Nim/pull/16959
  . https://github.com/xflywind/Nim/commit/6728141c558481f31ba61f7ded9e6140a99b2154
    Len must be declared before items
'''
"""
import mset_len_before_definition

let a = Diff[int]()
a.test()

