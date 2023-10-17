discard """
  description: "Test explicit generic params with the various call syntaxes"
"""

proc void0[T]() = discard
proc void1[X](x: X) = discard
proc void2[X, Y](x: X, y: Y) = discard


void0[int]()
void0[:int]()


void1[int] 1
void1[:int] 1
#void1[int] x = 1
#void1[:int] y = 1

void1[int]: 1
void1[:int]: 1

void1[int](1)
void1[:int](1)
void1[int](x = 1)
void1[:int](x = 1)

1.void1[:int]()
1.void1[:int]


void2[int, int] 1, 2
void2[:int, int] 1, 2
void2[int, int] 1, y = 2
void2[:int, int] 1, y = 2
#void2[int, int] x = 1, 2
#void2[:int, int] x = 1, 2

void2[int, int] 1: 2
void2[:int, int] 1: 2
void2[int, int](1): 2
void2[:int, int](1): 2
void2[int, int](x = 1): 2
void2[:int, int](x = 1): 2

void2[int, int](1, 2)
void2[:int, int](1, 2)
void2[int, int](x = 1, 2)
void2[:int, int](x = 1, 2)
void2[int, int](1, y = 2)
void2[:int, int](1, y = 2)
void2[int, int](x = 1, y = 2)
void2[:int, int](x = 1, y = 2)

#1.void2[:int, int] 2
#1.void2[:int, int] y = 2
1.void2[:int, int]: 2
1.void2[:int, int](2)
1.void2[:int, int](y = 2)

