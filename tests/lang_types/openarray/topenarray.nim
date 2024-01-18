discard """
  targets: "c js"
"""

proc fn1[T](a: openArray[T]): seq[T] =
  for ai in a: result.add ai

proc fn2[T](a: var openArray[T]): seq[T] =
  for ai in a: result.add ai

proc fn3[T](a: var openArray[T]) =
  for i, ai in mpairs(a): ai = i * 10

proc main =
  var a = [1,2,3,4,5]

  doAssert fn1(a.toOpenArray(1,3)) == @[2,3,4]

  doAssert fn2(toOpenArray(a, 1, 3)) == @[2,3,4]
  doAssert fn2(a.toOpenArray(1,3)) == @[2,3,4]

  fn3(a.toOpenArray(1,3))
  when defined(js): discard # xxx bug #15952: `a` left unchanged
  else: doAssert a == [1, 0, 10, 20, 5]

  block: # bug #12521
    block:
      type slice[T] = openArray[T]

      # Proc using that alias
      proc testing(sl: slice[int]): seq[int] =
        for item in sl:
          result.add item

      let mySeq = @[1, 2, 3, 4, 5, 6, 7, 8, 9]
      doAssert testing(mySeq) == mySeq
      doAssert testing(mySeq[2..^2]) == mySeq[2..^2]

    block:
      type slice = openArray[int]

      # Proc using that alias
      proc testing(sl: slice): seq[int] =
        for item in sl:
          result.add item

      let mySeq = @[1, 2, 3, 4, 5, 6, 7, 8, 9]
      doAssert testing(mySeq) == mySeq
      doAssert testing(mySeq[2..^2]) == mySeq[2..^2]


main()
# static: main() # xxx bug #15952: Error: cannot generate code for: mSlice

block array_modification_not_visible:
  # modifications of an array through a ``var openArray`` parameter
  # were not visible on the array if the ``openArray`` parameter was
  # created via ``toOpenArray`` and the array was *potentially* modified
  # during evaluation of the lower or upper bound argument expression
  var arr = [1, 2]

  proc mut(x: var openArray[int]) =
    x[0] = 3

  # modifying `arr` as part of either the lower or upper bound argument
  # expression triggered the bug
  mut(toOpenArray(arr, 0, (arr[0] = 4; 0)))
  when defined(js):
    # knownIssue
    doAssert arr != [3, 2], "works now"
  else:
    doAssert arr == [3, 2]