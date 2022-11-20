discard """
  errormsg: "size of array exceeds range of index type 'range 2147483646..2147483647(int32)' by 1 elements"
  line: 17
  labels: "array constructor index"
  description: '''
    . It is possible to only specify the index of the first element
      and let the compiler infer the other elements' indices.
    . Here, we create an array with 3 elements. The
        1st element is at index `2^31 - 2` (explicit)
        2nd element is at index `2^31 - 1` (inferred)
        3rd element is at index `2^31 + 0` (inferred)
    . the index value of the last element (2^31 + 0) doesn't fit
      into an int32, prompting an error.
  '''
"""

let a = [ high(int32)-1 : 1,
                          2,
                          3 ]
