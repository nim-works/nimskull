discard """
targets: "!js !vm"
"""

block union_pragma:
  ## The union pragma can be applied to any object type. It means all of the
  ## object's fields are overlaid in memory. This produces a union instead of a
  ## struct in the generated C/C++ code.
  type
    Union {.union.} = object
      field1: int
      field2: array[12, int]
      field3: tuple[idx1, idx2: int]

  doAssert sizeof(Union) == max(sizeof(int), sizeof(array[12, int]))

  var uni = Union()

  uni.field1 = 10

  doAssert uni.field2[0] == 10

  doAssert uni.field3.idx1 == 10

  uni = Union(field2: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12])

  doAssert uni.field1 == 1
  doAssert uni.field3 == (1, 2)