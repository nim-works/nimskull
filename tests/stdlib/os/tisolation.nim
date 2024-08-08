discard """
  targets: "c"
"""

import std/[isolation, json]



proc main() =
  block:
    type
      Empty = ref object


    var x = isolate(Empty())
    discard extract(x)

  block: # string literals
    var data = isolate("string")
    doAssert data.extract == "string"
    doAssert data.extract == ""

  block: # string literals
    var data = isolate("")
    doAssert data.extract == ""
    doAssert data.extract == ""

  block:
    var src = "string"
    var data = isolate(move src)
    doAssert data.extract == "string"
    doAssert src.len == 0

  block: # int literals
    var data = isolate(1)
    doAssert data.extract == 1
    doAssert data.extract == 0

  block: # float literals
    var data = isolate(1.6)
    doAssert data.extract == 1.6
    doAssert data.extract == 0.0

  block:
    var data = isolate(@["1", "2"])
    doAssert data.extract == @["1", "2"]
    doAssert data.extract == @[]

  block:
    var data = isolate(@["1", "2", "3", "4", "5"])
    doAssert data.extract == @["1", "2", "3", "4", "5"]
    doAssert data.extract == @[]

  block:
    var data = isolate(@["", ""])
    doAssert data.extract == @["", ""]
    doAssert data.extract == @[]

  block:
    var src = @["1", "2"]
    var data = isolate(move src)
    doAssert data.extract == @["1", "2"]
    doAssert src.len == 0

  block:
    var data = isolate(@[1, 2])
    doAssert data.extract == @[1, 2]
    doAssert data.extract == @[]

  block:
    var data = isolate(["1", "2"])
    doAssert data.extract == ["1", "2"]
    doAssert data.extract == ["", ""]

  block:
    var data = isolate([1, 2])
    doAssert data.extract == [1, 2]
    doAssert data.extract == [0, 0]

  block:
    type
      Test = object
        id: int

    var data = isolate(Test(id: 12))
    doAssert data.extract.id == 12

  block:
    type
      Test = object
        id: int

    var src = Test(id: 12)
    var data = isolate(src)
    doAssert data.extract.id == 12

  block:
    type
      Test = object
        id: int

    var src = Test(id: 12)
    var data = isolate(move src)
    doAssert data.extract.id == 12

  block:
    type
      Test = ref object
        id: int

    var data = isolate(Test(id: 12))
    doAssert data.extract.id == 12

  block:
    var x: seq[Isolated[JsonNode]]
    x.add isolate(newJString("1234"))

    doAssert $x == """@[(value: "1234")]"""


static: main()
main()
