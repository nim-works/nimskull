discard """
  cmd: "nim check --hints:off $file"
"""


proc `=deepCopy`(x: int): int #[tt.Error
        '=deepCopy' must satisfy the signature 'proc (x: var T, y: T){.noSideEffect.}' where T is 'ptr' or 'ref' of either 'distinct' or 'object'
    ]# = discard


proc `=`(dest: var int, src: int) #[tt.Error
        '=' must satisfy the signature 'proc (dest: var T, src: T){.noSideEffect.}' where T is 'distinct' or 'object'
    ]# = discard


proc `=copy`(dest: var int, src: int) #[tt.Error
        '=copy' must satisfy the signature 'proc (dest: var T, src: T){.noSideEffect.}' where T is 'distinct' or 'object'
    ]# = discard


proc `=destroy`(x: var int) #[tt.Error
        '=destroy' must satisfy the signature 'proc (x: var T){.noSideEffect.}' where T is 'distinct' or 'object'
    ]# = discard


proc `=sink`(x: var int, y: int) #[tt.Error
        '=sink' must satisfy the signature 'proc (x: var T, y: T){.noSideEffect.}' where T is 'distinct' or 'object'
    ]# = discard


proc `=trace`(x: var int, env: pointer) #[tt.Error
        '=trace' must satisfy the signature 'proc (x: var T, env: pointer){.noSideEffect.}' where T is 'distinct' or 'object'
    ]# = discard