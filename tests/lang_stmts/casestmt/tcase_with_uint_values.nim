discard """
  targets: "c js vm"
  description: '''
    Ensures that case statements work with uint operands and of-branch values
    not representable with the same-sized signed integer type
  '''
"""

proc test[T: uint64|uint; S]() =
  const Border = T(high(S)) # the highest possible signed integer value

  proc inRange(x: T): int {.noinline.} = # don't fold at compile-time
    type Range = range[Border-5..Border+5]
      # work-around so that the set construction expression compiles

    case x
    of (high(T)-3)..high(T):        0 # range that's fully beyond the border
    of (Border-4)..(Border+4):      1 # range that crosses the border
    of {Range(Border-5), Border+5}: 2 # set with values in and beyond the border
    else:                           3

  doAssert inRange(0) == 3
  doAssert inRange(high(T)) == 0
  when defined(vm):
    # knownIssue: ranges in of-branches that cross the signed/unsigned border
    #             don't work correctly in the VM, due to their values being
    #             treated as signed integers
    doAssert inRange(Border) == 3
    doAssert inRange(Border+1) == 3
  else:
    doAssert inRange(Border) == 1
    doAssert inRange(Border+1) == 1
  doAssert inRange(Border-5) == 2
  doAssert inRange(Border+5) == 2

test[uint64, int64]()
test[uint, int]()