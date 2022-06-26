discard """
description: '''
  . From https://github.com/nim-lang/Nim/issues/17177
    asyncjs: declaring .async proc inside an .async proc gives CT error
'''
"""
import std/asyncjs

proc fn1(n: int): Future[int] {.async.} = return n
proc main2() =
  proc fn2(n: int): Future[int] {.async.} = return n
proc main3(a: auto) =
  proc fn3(n: int): Future[int] {.async.} = return n
proc main4() {.async.} =
  proc fn4(n: int): Future[int] {.async.} = return n
  discard

