discard """
  errormsg: "type mismatch: got <proc [T](x: vector[vectFunc.T]): vector[vectFunc.T], vector[3]>"
  line: 36
  description: '''
    . From https://github.com/nim-lang/Nim/issues/6137
      Illegal storage access (Attempt to read from nil?): when attempting
      to pass proc to proc
    . I think it has to do with the fact that type inference does not
     (unfortunately) perform backtracking, and Nim is not able to figure out
     T from the first parameter (it needs the vector to resolve T).
  '''
"""

type
  # simple vector of declared fixed length
  vector[N : static[int]] = array[0..N-1, float]

proc `*`[T](x: float, a: vector[T]): vector[T] =
  # multiplication by scalar
  for ii in 0..high(a):
    result[ii] = a[ii]*x

let
  # define a vector of length 3
  x: vector[3] = [1.0, 3.0, 5.0]

proc vectFunc[T](x: vector[T]): vector[T] =
  # Define a vector function
  result = 2.0*x

proc passVectFunction[T](g: proc(x: vector[T]): vector[T], x: vector[T]): vector[T] =
  # pass a vector function as input in another procedure
  result = g(x)

let
  xNew = passVectFunction(vectFunc,x)

