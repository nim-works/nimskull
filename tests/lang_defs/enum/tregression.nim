discard """
  action: "compile"
"""
import std/options, mregression

type
  MyEnum = enum
    Success

template t =
  echo some(Success)

t()
