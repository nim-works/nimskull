discard """
  description: "Ensure that the standalone heap option works"
  matrix: "-d:StandaloneHeapSize:4166656 -d:useSysAssert"
  targets: "c"
"""

# test with ``-d:useSysAssert`` so that violated allocator invariants are
# caught

# allocate, write, read back, and deallocate again
let p = create(int)
p[] = 100
doAssert p[] == 100
dealloc(p)
