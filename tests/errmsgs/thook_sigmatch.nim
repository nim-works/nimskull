discard """
  cmd: "nim check --hints:off $file"
"""

type typ = ref object

proc `=copy`(dest: var typ, src: typ) #[tt.Error
        '=copy' must satisfy the signature 'proc(x: var T, y: T)' where T is 'distinct' or 'object'
    ]# = discard


proc `=destroy`(x: var typ) #[tt.Error
        '=destroy' must satisfy the signature 'proc(x: var T)' where T is 'distinct' or 'object'
    ]# = discard


proc `=sink`(x: var typ, y: typ) #[tt.Error
        '=sink' must satisfy the signature 'proc(x: var T, y: T)' where T is 'distinct' or 'object'
    ]# = discard


proc `=trace`(x: var typ, env: pointer) #[tt.Error
        '=trace' must satisfy the signature 'proc(x: var T, env: pointer)' where T is 'distinct' or 'object'
    ]# = discard