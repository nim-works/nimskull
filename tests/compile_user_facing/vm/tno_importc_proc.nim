discard """
  action: reject
  description: "C FFI doesn't work at compile time"
  errormsg: "cannot 'importc' variable/proc at compile time: c_memset"
  line: 13
"""

static:
  # Declaring the proc must not fail
  proc c_memset(p: pointer, v: cint, l: c_size) {.importc: "memset".}

  # Using the proc is an error
  c_memset(nil, 0, 0) # arguments don't matter