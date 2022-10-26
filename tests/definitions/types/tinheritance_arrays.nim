discard """
  cmd: "nim check --hints:off $file"
  action: reject
  nimout: '''
tinheritance_arrays.nim(20, 18) Error: type mismatch: got <B> but expected 'A = object'
tinheritance_arrays.nim(20, 23) Error: type mismatch: got <C> but expected 'A = object'
tinheritance_arrays.nim(21, 18) Error: type mismatch: got <B> but expected 'C = object'
tinheritance_arrays.nim(21, 23) Error: type mismatch: got <A> but expected 'C = object'
tinheritance_arrays.nim(22, 23) Error: type mismatch: got <A> but expected 'B = object'
'''
"""


block: # Value test
  type
    A = object of RootObj
    B = object of A
    C = object of A
  
  discard [A(), B(), C()]
  discard [C(), B(), A()]
  discard [B(), B(), A()]
  discard [B(), B(), B()]
  discard [A(), A(), A()]

block: # ref test
  type
    A = ref object of RootObj
    B = ref object of A
    C = ref object of A
  
  discard [A(), B(), C()]
  discard [C(), B(), A()]
  discard [B(), B(), A()]
  discard [B(), B(), B()]
  discard [A(), A(), A()]

block: # ptr test
  type
    A = object of RootObj
    B = object of A
    C = object of A

  template make(t: typedesc): ptr t =
    let res = createU(t)
    res[] = t()
    res

  discard [make A, make B, make C]
  discard [make C, make B, make A]
  discard [make B, make B, make A]
  discard [make B, make B, make B]
  discard [make A, make A, make A]
