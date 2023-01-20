discard """
errormsg: "static expressions where the result contains non-nil 'ref' values are not supported"
line: 18
"""

# bug #5870
type SomeRefObj = ref object of RootObj
    someIntMember: int

proc createSomeRefObj(v: int): SomeRefObj=
    result.new()
    result.someIntMember = v

# embedded nil values work:
const seqOfNilRefs = @[SomeRefObj(nil), SomeRefObj(nil)]

# but embedded non-nil values don't:
const compileTimeSeqOfRefObjs = @[createSomeRefObj(100500), createSomeRefObj(2)]

for i in 0..1:
  echo compileTimeSeqOfRefObjs[i].someIntMember
