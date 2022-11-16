discard """
  output: '''
(v: [(v: [0.0, 1.1]), (v: [2.2, 3.3])])
(v: [(v: [0.0, 1.1]), (v: [2.2, 3.3])])
'''
  description: '''
    . From https://github.com/nim-lang/Nim/issues/12753
      assignment of objects erroneously zeroed LHS before object construction on the RHS
    . When assigning an object constructor to an object, the left hand side
      got zeroed before the object construction happens.
      Thus it gives wrong results, when the right hand side uses the left hand
      side object in the constructor.
    . The generated C code contains unnecessary call to nimZeroMem() for b
      before calculation of b += a. Therefore, the output is given as b = 0 + a.
    . https://github.com/nim-lang/Nim/pull/12814
      Fixed objects being erroneously zeroed out before object construction
'''
"""

type
  V = object
    v:array[2,float]
  M = object
    v:array[2,V]

var
  a = M(v:[ V(v:[0.0,1.0]), V(v:[2.0,3.0]) ])
  b = M(v:[ V(v:[0.0,0.1]), V(v:[0.2,0.3]) ])

echo M(v: [V(v: [b.v[0].v[0] + a.v[0].v[0], b.v[0].v[1] + a.v[0].v[1]]),
       V(v: [b.v[1].v[0] + a.v[1].v[0], b.v[1].v[1] + a.v[1].v[1]])])
b = M(v: [V(v: [b.v[0].v[0] + a.v[0].v[0], b.v[0].v[1] + a.v[0].v[1]]),
      V(v: [b.v[1].v[0] + a.v[1].v[0], b.v[1].v[1] + a.v[1].v[1]])])

echo b

