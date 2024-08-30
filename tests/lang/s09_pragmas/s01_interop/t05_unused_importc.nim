discard """
  description: '''
    Let and var bindings for imported entities are only included in the
    build if they're assigned to, read from, or have their address taken,
    within code part of the final build
  '''
"""

var a {.importc, header: "<doesnt_exist.h>".}: int
let b {.importc, header: "<doesnt_exist2.h>".}: int

# an initial assignment (e.g.: `` = 0``) would count as a usage and result in
# a compiler error, since the headers are non-existent

proc f() =
  # while this is a usage of `a`, `f` is not part of the final build, so
  # `a` is not pulled in
  a = 1
