import libcrypto

import types
export BIO

type
  BIO_METHOD* {.final.} = object

proc BIO_new*(typ: ptr BIO_METHOD): ptr BIO {.importc, cdecl.}
proc BIO_free*(a: ptr BIO): cint {.importc, cdecl.}
proc BIO_free_all*(a: ptr BIO) {.importc, cdecl.}
proc BIO_up_ref*(a: ptr BIO): cint {.importc, cdecl.}

proc BIO_s_fd*(): ptr BIO_METHOD {.importc, cdecl.}
proc BIO_set_fd*(b: ptr BIO, fd: cint, c: cint): cint {.importc, cdecl.}
proc BIO_get_fd*(b: ptr BIO, c: ptr cint): cint {.importc, cdecl.}
proc BIO_new_fd*(fd: cint, close_flag: cint): ptr BIO {.importc, cdecl.}

proc BIO_s_socket*(): ptr BIO_METHOD {.importc, cdecl.}
proc BIO_new_socket*(sock: cint, close_flag: cint): ptr BIO {.importc, cdecl.}

proc BIO_s_bio*(): ptr BIO_METHOD {.importc, cdecl.}
proc BIO_new_bio_pair*(bio1: ptr ptr BIO, writebuf1: csize_t, bio2: ptr ptr BIO, writebuf2: csize_t): cint {.importc, cdecl.}

proc BIO_s_mem*(): ptr BIO_METHOD {.importc, cdecl.}
proc BIO_s_secmem*(): ptr BIO_METHOD {.importc, cdecl.}
proc BIO_new_mem_buf*(buf: pointer, len: cint): ptr BIO {.importc, cdecl.}

proc BIO_read_ex*(b: ptr BIO, data: pointer, dlen: csize_t, readbytes: ptr csize_t): cint {.importc, cdecl.}
proc BIO_write_ex*(b: ptr BIO, data: pointer, dlen: csize_t, written: ptr csize_t): cint {.importc, cdecl.}

proc BIO_read*(b: ptr BIO, data: pointer, dlen: cint): cint {.importc, cdecl.}
proc BIO_gets*(b: ptr BIO, buf: cstring, size: cint): cint {.importc, cdecl.}
proc BIO_get_line*(b: ptr BIO, buf: cstring, size: cint): cint {.importc, cdecl.}
proc BIO_write*(b: ptr BIO, data: pointer, dlen: cint): cint {.importc, cdecl.}
proc BIO_puts*(b: ptr BIO, buf: cstring): cint {.importc, cdecl.}
