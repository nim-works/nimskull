discard """
  output: '''7
8 8
-2'''
  targets: "c js vm"
"""

#bug 1063

const
  KeyMax = 227
  myconst = int((KeyMax + 31) / 32)

type
  FU = array[int((KeyMax + 31) / 32), cuint]

echo FU.high

type
  PKeyboard* = ptr object
  TKeyboardState* = object
    display*: pointer
    internal: array[int((KeyMax + 31)/32), cuint]

echo myconst, " ", int((KeyMax + 31) / 32)

#bug 1304 or something:

const constArray: array[-3..2, int] = [-3, -2, -1, 0, 1, 2]

echo constArray[-2]

block bigIntOffset:
  let bigOffset: array[-100_001 .. -100_000, int] = [1, 2]
  let idx = -100_001
  doAssert bigOffset[idx] == 1
  # static indexing
  doAssert bigOffset[-100_001] == 1

block bigUintOffset:
  let bigOffset: array[200_000u..200_001u, int] = [3, 4]
  let idx = 200_001u
  doAssert bigOffset[idx] == 4
  # static indexing
  doAssert bigOffset[200_001] == 4
