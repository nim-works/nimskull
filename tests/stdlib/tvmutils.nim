discard """
  targets: "native"
  joinable: false
  nimout: '''
0
1
2
tvmutils.nim(29, 13) [LdImmInt]     if i == 4:
tvmutils.nim(29, 10) [EqInt]     if i == 4:
tvmutils.nim(29, 10) [FJmp]     if i == 4:
tvmutils.nim(29, 13) [LdImmInt]     if i == 4:
tvmutils.nim(29, 10) [EqInt]     if i == 4:
tvmutils.nim(29, 10) [FJmp]     if i == 4:
tvmutils.nim(30, 7) [LdConst]       vmTrace(false)
tvmutils.nim(30, 15) [LdImmInt]       vmTrace(false)
tvmutils.nim(30, 14) [IndCall]       vmTrace(false)
5
6
'''
"""
# line 21 (only showing a subset of nimout to avoid making the test rigid)
import std/vmutils

proc main() =
  for i in 0..<7:
    echo i
    if i == 2:
      vmTrace(true)
    if i == 4:
      vmTrace(false)

static: main()
