#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## OpenSSL support
##
## When OpenSSL is dynamically linked, the wrapper provides partial forward and backward
## compatibility for OpenSSL versions 1.1.1 and above.
##
## OpenSSL can also be statically linked using `--dynlibOverride:ssl` for OpenSSL >= 1.1.1.
##
## Build and test examples:
##
## .. code-block::
##   ./bin/nim c -d:ssl -p:. -r tests/untestable/tssl.nim
##   ./bin/nim c -d:ssl -p:. --dynlibOverride:ssl --passl:-lcrypto --passl:-lssl -r tests/untestable/tssl.nim

when defined(nimHasStyleChecks):
  {.push styleChecks: off.}

const useWinVersion = defined(windows) or defined(nimdoc)

# To force openSSL version use -d:sslVersion=1.0.0
# See: #10281, #10230
# General issue:
# Other dynamic libraries (like libpg) load different openSSL version then what nim loads.
# Having two different openSSL loaded version causes a crash.
# Use this compile time define to force the openSSL version that your other dynamic libraries want.
const sslVersion {.strdefine.}: string = ""
when sslVersion != "":
  when defined(macosx):
    const
      DLLSSLName* = "libssl." & sslVersion & ".dylib"
      DLLUtilName* = "libcrypto." & sslVersion & ".dylib"
    from std/posix import SocketHandle
  elif defined(windows):
    const
      DLLSSLName* = "libssl-" & sslVersion & ".dll"
      DLLUtilName* =  "libcrypto-" & sslVersion & ".dll"
    from std/winlean import SocketHandle
  else:
    const
      DLLSSLName* = "libssl.so." & sslVersion
      DLLUtilName* = "libcrypto.so." & sslVersion
    from std/posix import SocketHandle

elif useWinVersion:
  when defined(cpu64):
    const
      DLLSSLName* = "(libssl-3-x64|libssl-1_1-x64).dll"
      DLLUtilName* = "(libcrypto-3-x64|libcrypto-1_1-x64).dll"
  else:
    const
      DLLSSLName* = "(libssl-3|libssl-1_1).dll"
      DLLUtilName* = "(libssl-3|libcrypto-1_1).dll"

  from std/winlean import SocketHandle
else:
  const versions = "(.3|.1.1)"

  when defined(macosx):
    const
      DLLSSLName* = "libssl" & versions & ".dylib"
      DLLUtilName* = "libcrypto" & versions & ".dylib"
  else:
    const
      DLLSSLName* = "libssl.so" & versions
      DLLUtilName* = "libcrypto.so" & versions
  from std/posix import SocketHandle

import std/dynlib

type
  SslStruct {.final, pure.} = object
  SslPtr* = ptr SslStruct
  PSslPtr* = ptr SslPtr
  SslCtx* = SslPtr
  PSSL_METHOD* = SslPtr
  PSTACK* = SslPtr
  PX509* = SslPtr
  PX509_NAME* = SslPtr
  PEVP_MD* = SslPtr
  PBIO_METHOD* = SslPtr
  BIO* = SslPtr
  EVP_PKEY* = SslPtr
  PRSA* = SslPtr
  PASN1_UTCTIME* = SslPtr
  PASN1_cInt* = SslPtr
  PPasswdCb* = SslPtr
  EVP_MD* = SslPtr
  EVP_MD_CTX* = SslPtr
  EVP_PKEY_CTX* = SslPtr
  ENGINE* = SslPtr
  PFunction* = proc () {.cdecl.}
  DES_cblock* = array[0..7, int8]
  PDES_cblock* = ptr DES_cblock
  des_ks_struct*{.final.} = object
    ks*: DES_cblock
    weak_key*: cint

  des_key_schedule* = array[1..16, des_ks_struct]

  pem_password_cb* = proc(buf: cstring, size, rwflag: cint, userdata: pointer): cint {.cdecl.}

  PaddingType* = enum
    RSA_PKCS1_PADDING = 1.cint,
    RSA_SSLV23_PADDING = 2.cint,
    RSA_NO_PADDING = 3.cint,
    RSA_PKCS1_OAEP_PADDING = 4.cint,
    RSA_X931_PADDING = 5.cint,
    RSA_PKCS1_PSS_PADDING = 6.cint


const
  SSL_SENT_SHUTDOWN* = 1
  SSL_RECEIVED_SHUTDOWN* = 2
  EVP_MAX_MD_SIZE* = 16 + 20
  SSL_ERROR_NONE* = 0
  SSL_ERROR_SSL* = 1
  SSL_ERROR_WANT_READ* = 2
  SSL_ERROR_WANT_WRITE* = 3
  SSL_ERROR_WANT_X509_LOOKUP* = 4
  SSL_ERROR_SYSCALL* = 5      #look at error stack/return value/errno
  SSL_ERROR_ZERO_RETURN* = 6
  SSL_ERROR_WANT_CONNECT* = 7
  SSL_ERROR_WANT_ACCEPT* = 8
  SSL_CTRL_NEED_TMP_RSA* = 1
  SSL_CTRL_SET_TMP_RSA* = 2
  SSL_CTRL_SET_TMP_DH* = 3
  SSL_CTRL_SET_TMP_ECDH* = 4
  SSL_CTRL_SET_TMP_RSA_CB* = 5
  SSL_CTRL_SET_TMP_DH_CB* = 6
  SSL_CTRL_SET_TMP_ECDH_CB* = 7
  SSL_CTRL_GET_SESSION_REUSED* = 8
  SSL_CTRL_GET_CLIENT_CERT_REQUEST* = 9
  SSL_CTRL_GET_NUM_RENEGOTIATIONS* = 10
  SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS* = 11
  SSL_CTRL_GET_TOTAL_RENEGOTIATIONS* = 12
  SSL_CTRL_GET_FLAGS* = 13
  SSL_CTRL_EXTRA_CHAIN_CERT* = 14
  SSL_CTRL_SET_MSG_CALLBACK* = 15
  SSL_CTRL_SET_MSG_CALLBACK_ARG* = 16 # only applies to datagram connections
  SSL_CTRL_SET_MTU* = 17      # Stats
  SSL_CTRL_SESS_NUMBER* = 20
  SSL_CTRL_SESS_CONNECT* = 21
  SSL_CTRL_SESS_CONNECT_GOOD* = 22
  SSL_CTRL_SESS_CONNECT_RENEGOTIATE* = 23
  SSL_CTRL_SESS_ACCEPT* = 24
  SSL_CTRL_SESS_ACCEPT_GOOD* = 25
  SSL_CTRL_SESS_ACCEPT_RENEGOTIATE* = 26
  SSL_CTRL_SESS_HIT* = 27
  SSL_CTRL_SESS_CB_HIT* = 28
  SSL_CTRL_SESS_MISSES* = 29
  SSL_CTRL_SESS_TIMEOUTS* = 30
  SSL_CTRL_SESS_CACHE_FULL* = 31
  SSL_CTRL_OPTIONS* = 32
  SSL_CTRL_MODE* = 33
  SSL_CTRL_GET_READ_AHEAD* = 40
  SSL_CTRL_SET_READ_AHEAD* = 41
  SSL_CTRL_SET_SESS_CACHE_SIZE* = 42
  SSL_CTRL_GET_SESS_CACHE_SIZE* = 43
  SSL_CTRL_SET_SESS_CACHE_MODE* = 44
  SSL_CTRL_GET_SESS_CACHE_MODE* = 45
  SSL_CTRL_GET_MAX_CERT_LIST* = 50
  SSL_CTRL_SET_MAX_CERT_LIST* = 51 #* Allow SSL_write(..., n) to return r with 0 < r < n (i.e. report success
                                   # * when just a single record has been written): *
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB = 53
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG = 54
  SSL_CTRL_SET_TLSEXT_HOSTNAME = 55
  SSL_CTRL_SET_ECDH_AUTO* = 94
  TLSEXT_NAMETYPE_host_name* = 0
  SSL_TLSEXT_ERR_OK* = 0
  SSL_TLSEXT_ERR_ALERT_WARNING* = 1
  SSL_TLSEXT_ERR_ALERT_FATAL* = 2
  SSL_TLSEXT_ERR_NOACK* = 3
  SSL_MODE_ENABLE_PARTIAL_WRITE* = 1 #* Make it possible to retry SSL_write() with changed buffer location
                                     # * (buffer contents must stay the same!); this is not the default to avoid
                                     # * the misconception that non-blocking SSL_write() behaves like
                                     # * non-blocking write(): *
  SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER* = 2 #* Never bother the application with retries if the transport
                                           # * is blocking: *
  SSL_MODE_AUTO_RETRY* = 4    #* Don't attempt to automatically build certificate chain *
  SSL_MODE_NO_AUTO_CHAIN* = 8
  SSL_OP_NO_SSLv2* = 0x01000000
  SSL_OP_NO_SSLv3* = 0x02000000
  SSL_OP_NO_TLSv1* = 0x04000000
  SSL_OP_NO_TLSv1_1* = 0x08000000
  SSL_OP_ALL* = 0x000FFFFF
  SSL_VERIFY_NONE* = 0x00000000
  SSL_VERIFY_PEER* = 0x00000001
  SSL_ST_CONNECT* = 0x1000
  SSL_ST_ACCEPT* = 0x2000
  SSL_ST_INIT* = SSL_ST_CONNECT or SSL_ST_ACCEPT
  OPENSSL_DES_DECRYPT* = 0
  OPENSSL_DES_ENCRYPT* = 1
  X509_V_OK* = 0
  X509_V_ILLEGAL* = 1
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT* = 2
  X509_V_ERR_UNABLE_TO_GET_CRL* = 3
  X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE* = 4
  X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE* = 5
  X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY* = 6
  X509_V_ERR_CERT_SIGNATURE_FAILURE* = 7
  X509_V_ERR_CRL_SIGNATURE_FAILURE* = 8
  X509_V_ERR_CERT_NOT_YET_VALID* = 9
  X509_V_ERR_CERT_HAS_EXPIRED* = 10
  X509_V_ERR_CRL_NOT_YET_VALID* = 11
  X509_V_ERR_CRL_HAS_EXPIRED* = 12
  X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD* = 13
  X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD* = 14
  X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD* = 15
  X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD* = 16
  X509_V_ERR_OUT_OF_MEM* = 17
  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT* = 18
  X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN* = 19
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY* = 20
  X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE* = 21
  X509_V_ERR_CERT_CHAIN_TOO_LONG* = 22
  X509_V_ERR_CERT_REVOKED* = 23
  X509_V_ERR_INVALID_CA* = 24
  X509_V_ERR_PATH_LENGTH_EXCEEDED* = 25
  X509_V_ERR_INVALID_PURPOSE* = 26
  X509_V_ERR_CERT_UNTRUSTED* = 27
  X509_V_ERR_CERT_REJECTED* = 28 #These are 'informational' when looking for issuer cert
  X509_V_ERR_SUBJECT_ISSUER_MISMATCH* = 29
  X509_V_ERR_AKID_SKID_MISMATCH* = 30
  X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH* = 31
  X509_V_ERR_KEYUSAGE_NO_CERTSIGN* = 32
  X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER* = 33
  X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION* = 34 #The application is not happy
  X509_V_ERR_APPLICATION_VERIFICATION* = 50
  SSL_FILETYPE_ASN1* = 2
  SSL_FILETYPE_PEM* = 1
  EVP_PKEY_RSA* = 6           # libssl.dll

  BIO_C_SET_CONNECT = 100
  BIO_C_DO_STATE_MACHINE = 101
  BIO_C_GET_SSL = 110

when compileOption("dynlibOverride", "ssl") or defined(noOpenSSLHacks):
  when not defined(openssl111):
    proc SSL_get_peer_certificate*(ssl: SslCtx): PX509 {.cdecl, dynlib: DLLSSLName, importc: "SSL_get1_peer_certificate".}

    proc SSL_CTX_load_verify_store*(ssl: SslCtx, CAstore: cstring): cint {.cdecl, dynlib: DLLSSLName, importc.}
  else:
    proc SSL_get_peer_certificate*(ssl: SslCtx): PX509 {.cdecl, dynlib: DLLSSLName, importc: "SSL_get_peer_certificate".}
else:
  proc thisModule(): LibHandle {.inline.} =
    var thisMod {.global.}: LibHandle
    if thisMod.isNil: thisMod = loadLib()

    result = thisMod

  proc sslModule(): LibHandle {.inline.} =
    var sslMod {.global.}: LibHandle
    if sslMod.isNil: sslMod = loadLibPattern(DLLSSLName)

    result = sslMod

  proc symNullable(dll: LibHandle, name: string, alternativeName = ""): pointer =
    # Load from DLL.
    if not dll.isNil:
      result = symAddr(dll, name)
      if result.isNil and alternativeName.len > 0:
        result = symAddr(dll, alternativeName)

    # Attempt to load from current exe.
    if result.isNil:
      let thisDynlib = thisModule()
      if thisDynlib.isNil: return nil
      result = symAddr(thisDynlib, name)
      if result.isNil and alternativeName.len > 0:
        result = symAddr(thisDynlib, alternativeName)

  proc sslSymNullable(name: string, alternativeName = ""): pointer =
    sslModule().symNullable(name, alternativeName)

  proc sslSymThrows(name: string, alternativeName = ""): pointer =
    result = sslSymNullable(name, alternativeName)
    if result.isNil: raiseInvalidLibrary(name)

  when not defined(nimDisableCertificateValidation):
    proc SSL_get_peer_certificate*(ssl: SslCtx): PX509 {.gcsafe, tags: [].} =
      {.cast(tags: []), cast(gcsafe).}:
        let thisProc {.global.} = cast[proc (ssl: SslCtx): PX509 {.cdecl.}](
          sslSymThrows("SSL_get1_peer_certificate", "SSL_get_peer_certificate")
        )
        if not thisProc.isNil: result = thisProc(ssl)

    proc SSL_CTX_load_verify_store*(ssl: SslCtx, CAstore: cstring): cint {.gcsafe, tags: [].} =
      {.cast(tags: []), cast(gcsafe).}:
        var thisProc {.global.}: proc (ssl: SslCtx, CAstore: cstring): cint {.cdecl.}
        if thisProc.isNil:
          thisProc = cast[typeof(thisProc)](sslSymThrows("SSL_CTX_load_verify_store"))
        result = thisProc(ssl, CAstore)

proc TLS_method*(): PSSL_METHOD {.cdecl, dynlib: DLLSSLName, importc.}
proc OpenSSL_version_num*(): culong {.cdecl, dynlib: DLLUtilName, importc.}

proc SSL_in_init*(ssl: SslPtr): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_ciphersuites*(ctx: SslCtx, str: cstring): cint {.cdecl, dynlib: DLLSSLName, importc.}

proc SSL_new*(context: SslCtx): SslPtr{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_free*(ssl: SslPtr){.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_get_SSL_CTX*(ssl: SslPtr): SslCtx {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_set_SSL_CTX*(ssl: SslPtr, ctx: SslCtx): SslCtx {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_session_id_context*(context: SslCtx, sid_ctx: string, sid_ctx_len: int){.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_get0_verified_chain*(ssl: SslPtr): PSTACK {.cdecl, dynlib: DLLSSLName,
    importc.}
proc SSL_CTX_new*(meth: PSSL_METHOD): SslCtx{.cdecl,
    dynlib: DLLSSLName, importc.}
proc SSL_CTX_load_verify_locations*(ctx: SslCtx, CAfile: cstring,
    CApath: cstring): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_free*(arg0: SslCtx){.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_verify*(s: SslCtx, mode: int, cb: proc (a: int, b: pointer): int {.cdecl.}){.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_get_verify_result*(ssl: SslPtr): int{.cdecl,
    dynlib: DLLSSLName, importc.}

proc SSL_CTX_set_cipher_list*(s: SslCtx, ciphers: cstring): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_use_certificate_file*(ctx: SslCtx, filename: cstring, typ: cint): cint{.
    stdcall, dynlib: DLLSSLName, importc.}
proc SSL_CTX_use_certificate_chain_file*(ctx: SslCtx, filename: cstring): cint{.
    stdcall, dynlib: DLLSSLName, importc.}
proc SSL_CTX_use_PrivateKey_file*(ctx: SslCtx,
    filename: cstring, typ: cint): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_check_private_key*(ctx: SslCtx): cint{.cdecl, dynlib: DLLSSLName,
    importc.}

proc SSL_CTX_get_ex_new_index*(argl: clong, argp: pointer, new_func: pointer, dup_func: pointer, free_func: pointer): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_ex_data*(ssl: SslCtx, idx: cint, arg: pointer): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_get_ex_data*(ssl: SslCtx, idx: cint): pointer {.cdecl, dynlib: DLLSSLName, importc.}

proc SSL_set_fd*(ssl: SslPtr, fd: SocketHandle): cint{.cdecl, dynlib: DLLSSLName, importc.}

proc SSL_shutdown*(ssl: SslPtr): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_set_shutdown*(ssl: SslPtr, mode: cint) {.cdecl, dynlib: DLLSSLName, importc: "SSL_set_shutdown".}
proc SSL_get_shutdown*(ssl: SslPtr): cint {.cdecl, dynlib: DLLSSLName, importc: "SSL_get_shutdown".}
proc SSL_connect*(ssl: SslPtr): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_read*(ssl: SslPtr, buf: pointer, num: int): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_write*(ssl: SslPtr, buf: cstring, num: int): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_get_error*(s: SslPtr, ret_code: cint): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_accept*(ssl: SslPtr): cint{.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_pending*(ssl: SslPtr): cint{.cdecl, dynlib: DLLSSLName, importc.}

proc BIO_new_mem_buf*(data: pointer, len: cint): BIO{.cdecl,
    dynlib: DLLUtilName, importc.}
proc BIO_new_ssl_connect*(ctx: SslCtx): BIO{.cdecl,
    dynlib: DLLSSLName, importc.}
proc BIO_ctrl*(bio: BIO, cmd: cint, larg: int, arg: cstring): int{.cdecl,
    dynlib: DLLUtilName, importc.}
proc BIO_get_ssl*(bio: BIO, ssl: ptr SslPtr): int =
  return BIO_ctrl(bio, BIO_C_GET_SSL, 0, cast[cstring](ssl))
proc BIO_set_conn_hostname*(bio: BIO, name: cstring): int =
  return BIO_ctrl(bio, BIO_C_SET_CONNECT, 0, name)
proc BIO_do_handshake*(bio: BIO): int =
  return BIO_ctrl(bio, BIO_C_DO_STATE_MACHINE, 0, nil)
proc BIO_do_connect*(bio: BIO): int =
  return BIO_do_handshake(bio)

when not defined(nimfix):
  proc BIO_read*(b: BIO, data: cstring, length: cint): cint{.cdecl,
      dynlib: DLLUtilName, importc.}
  proc BIO_write*(b: BIO, data: cstring, length: cint): cint{.cdecl,
      dynlib: DLLUtilName, importc.}

proc BIO_free*(b: BIO): cint{.cdecl, dynlib: DLLUtilName, importc.}

proc ERR_print_errors_fp*(fp: File){.cdecl, dynlib: DLLUtilName, importc.}

proc ERR_error_string*(e: culong, buf: cstring): cstring{.cdecl,
    dynlib: DLLUtilName, importc.}
proc ERR_get_error*(): culong{.cdecl, dynlib: DLLUtilName, importc.}
proc ERR_peek_last_error*(): culong{.cdecl, dynlib: DLLUtilName, importc.}

proc OPENSSL_config*(configName: cstring){.cdecl, dynlib: DLLUtilName, importc.}

proc OPENSSL_sk_num*(stack: PSTACK): int {.cdecl, dynlib: DLLSSLName, importc.}

proc OPENSSL_sk_value*(stack: PSTACK, index: int): pointer {.cdecl,
    dynlib: DLLSSLName, importc.}

proc d2i_X509*(px: ptr PX509, i: ptr ptr uint8, len: cint): PX509 {.cdecl,
    dynlib: DLLUtilName, importc.}

proc i2d_X509*(cert: PX509; o: ptr ptr uint8): cint {.cdecl,
    dynlib: DLLUtilName, importc.}

proc d2i_X509*(b: string): PX509 =
  ## decode DER/BER bytestring into X.509 certificate struct
  var bb = b.cstring
  let i = cast[ptr ptr uint8](addr bb)
  let ret = d2i_X509(addr result, i, b.len.cint)
  if ret.isNil:
    raise newException(Exception, "X.509 certificate decoding failed")

proc i2d_X509*(cert: PX509): string =
  ## encode `cert` to DER string
  let encoded_length = i2d_X509(cert, nil)
  result = newString(encoded_length)
  var q = result.cstring
  let o = cast[ptr ptr uint8](addr q)
  let length = i2d_X509(cert, o)
  if length.int <= 0:
    raise newException(Exception, "X.509 certificate encoding failed")

when not useWinVersion and not defined(macosx) and not defined(android) and not defined(nimNoAllocForSSL):
  proc CRYPTO_set_mem_functions(a,b,c: pointer){.cdecl,
    dynlib: DLLUtilName, importc.}

  proc allocWrapper(size: int): pointer {.cdecl.} = allocShared(size)
  proc reallocWrapper(p: pointer; newSize: int): pointer {.cdecl.} =
    if p == nil:
      if newSize > 0: result = allocShared(newSize)
    elif newSize == 0: deallocShared(p)
    else: result = reallocShared(p, newSize)
  proc deallocWrapper(p: pointer) {.cdecl.} =
    if p != nil: deallocShared(p)

proc CRYPTO_malloc_init*() =
  when not useWinVersion and not defined(macosx) and not defined(android) and not defined(nimNoAllocForSSL):
    CRYPTO_set_mem_functions(allocWrapper, reallocWrapper, deallocWrapper)

proc SSL_CTX_ctrl*(ctx: SslCtx, cmd: cint, larg: clong, parg: pointer): clong{.
  cdecl, dynlib: DLLSSLName, importc.}

proc SSL_CTX_callback_ctrl(ctx: SslCtx, typ: cint, fp: PFunction): int{.
  cdecl, dynlib: DLLSSLName, importc.}

proc SSLCTXSetMode*(ctx: SslCtx, mode: int): int =
  result = SSL_CTX_ctrl(ctx, SSL_CTRL_MODE, clong mode, nil)

proc SSL_ctrl*(ssl: SslPtr, cmd: cint, larg: int, parg: pointer): int{.
  cdecl, dynlib: DLLSSLName, importc.}

proc SSL_set_tlsext_host_name*(ssl: SslPtr, name: cstring): int =
  ## Set the SNI server name extension to be used in a client hello.
  ## Returns 1 if SNI was set, 0 if current SSL configuration doesn't support SNI.
  result = SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, name)

proc SSL_get_servername*(ssl: SslPtr, typ: cint = TLSEXT_NAMETYPE_host_name): cstring {.cdecl, dynlib: DLLSSLName, importc.}
  ## Retrieve the server name requested in the client hello. This can be used
  ## in the callback set in `SSL_CTX_set_tlsext_servername_callback` to
  ## implement virtual hosting. May return `nil`.

proc SSL_CTX_set_tlsext_servername_callback*(ctx: SslCtx, cb: proc(ssl: SslPtr, cb_id: int, arg: pointer): int {.cdecl.}): int =
  ## Set the callback to be used on listening SSL connections when the client hello is received.
  ##
  ## The callback should return one of:
  ## * SSL_TLSEXT_ERR_OK
  ## * SSL_TLSEXT_ERR_ALERT_WARNING
  ## * SSL_TLSEXT_ERR_ALERT_FATAL
  ## * SSL_TLSEXT_ERR_NOACK
  result = SSL_CTX_callback_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, cast[PFunction](cb))

proc SSL_CTX_set_tlsext_servername_arg*(ctx: SslCtx, arg: pointer): int =
  ## Set the pointer to be used in the callback registered to `SSL_CTX_set_tlsext_servername_callback`.
  result = SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, arg)

type
  PskClientCallback* = proc (ssl: SslPtr;
    hint: cstring; identity: cstring; max_identity_len: cuint; psk: ptr uint8;
    max_psk_len: cuint): cuint {.cdecl.}

  PskServerCallback* = proc (ssl: SslPtr;
    identity: cstring; psk: ptr uint8; max_psk_len: cint): cuint {.cdecl.}

proc SSL_CTX_set_psk_client_callback*(ctx: SslCtx; callback: PskClientCallback) {.cdecl, dynlib: DLLSSLName, importc.}
  ## Set callback called when OpenSSL needs PSK (for client).

proc SSL_CTX_set_psk_server_callback*(ctx: SslCtx; callback: PskServerCallback) {.cdecl, dynlib: DLLSSLName, importc.}
  ## Set callback called when OpenSSL needs PSK (for server).

proc SSL_CTX_use_psk_identity_hint*(ctx: SslCtx; hint: cstring): cint {.cdecl, dynlib: DLLSSLName, importc.}
  ## Set PSK identity hint to use.

proc SSL_get_psk_identity*(ssl: SslPtr): cstring {.cdecl, dynlib: DLLSSLName, importc.}
  ## Get PSK identity.

proc bioNew*(b: PBIO_METHOD): BIO{.cdecl, dynlib: DLLUtilName, importc: "BIO_new".}
proc bioFreeAll*(b: BIO){.cdecl, dynlib: DLLUtilName, importc: "BIO_free_all".}
proc bioSMem*(): PBIO_METHOD{.cdecl, dynlib: DLLUtilName, importc: "BIO_s_mem".}
proc bioCtrlPending*(b: BIO): cint{.cdecl, dynlib: DLLUtilName, importc: "BIO_ctrl_pending".}
proc bioRead*(b: BIO, Buf: cstring, length: cint): cint{.cdecl,
    dynlib: DLLUtilName, importc: "BIO_read".}
proc bioWrite*(b: BIO, Buf: cstring, length: cint): cint{.cdecl,
    dynlib: DLLUtilName, importc: "BIO_write".}

proc sslSetConnectState*(s: SslPtr) {.cdecl,
    dynlib: DLLSSLName, importc: "SSL_set_connect_state".}
proc sslSetAcceptState*(s: SslPtr) {.cdecl,
    dynlib: DLLSSLName, importc: "SSL_set_accept_state".}

proc sslRead*(ssl: SslPtr, buf: cstring, num: cint): cint{.cdecl,
      dynlib: DLLSSLName, importc: "SSL_read".}
proc sslPeek*(ssl: SslPtr, buf: cstring, num: cint): cint{.cdecl,
    dynlib: DLLSSLName, importc: "SSL_peek".}
proc sslWrite*(ssl: SslPtr, buf: cstring, num: cint): cint{.cdecl,
    dynlib: DLLSSLName, importc: "SSL_write".}

proc sslSetBio*(ssl: SslPtr, rbio, wbio: BIO) {.cdecl,
    dynlib: DLLSSLName, importc: "SSL_set_bio".}

proc sslDoHandshake*(ssl: SslPtr): cint {.cdecl,
    dynlib: DLLSSLName, importc: "SSL_do_handshake".}


proc ErrClearError*(){.cdecl, dynlib: DLLUtilName, importc: "ERR_clear_error".}
proc ErrFreeStrings*(){.cdecl, dynlib: DLLUtilName, importc: "ERR_free_strings".}
proc ErrRemoveState*(pid: cint){.cdecl, dynlib: DLLUtilName, importc: "ERR_remove_state".}

proc PEM_read_bio_RSA_PUBKEY*(bp: BIO, x: ptr PRSA, pw: pem_password_cb, u: pointer): PRSA {.cdecl,
    dynlib: DLLUtilName, importc.}
proc PEM_read_RSA_PUBKEY*(fp: pointer; x: ptr PRSA; cb: pem_password_cb, u: pointer): PRSA {.cdecl,
    dynlib: DLLUtilName, importc.}
proc RSA_verify*(kind: cint, origMsg: pointer, origMsgLen: cuint, signature: pointer,
    signatureLen: cuint, rsa: PRSA): cint {.cdecl, dynlib: DLLUtilName, importc.}
proc PEM_read_RSAPrivateKey*(fp: pointer; x: ptr PRSA; cb: pem_password_cb, u: pointer): PRSA {.cdecl,
    dynlib: DLLUtilName, importc.}
proc PEM_read_RSAPublicKey*(fp: pointer; x: ptr PRSA; cb: pem_password_cb, u: pointer): PRSA {.cdecl,
    dynlib: DLLUtilName, importc.}
proc PEM_read_bio_RSAPublicKey*(bp: BIO, x: ptr PRSA, cb: pem_password_cb, u: pointer): PRSA {.cdecl,
    dynlib: DLLUtilName, importc.}
proc PEM_read_bio_RSAPrivateKey*(bp: BIO, x: ptr PRSA, cb: pem_password_cb, u: pointer): PRSA {.cdecl,
    dynlib: DLLUtilName, importc.}
proc RSA_private_encrypt*(flen: cint, fr: ptr uint8, to: ptr uint8, rsa: PRSA, padding: PaddingType): cint {.cdecl,
    dynlib: DLLUtilName, importc.}
proc RSA_public_encrypt*(flen: cint, fr: ptr uint8, to: ptr uint8, rsa: PRSA, padding: PaddingType): cint {.cdecl,
    dynlib: DLLUtilName, importc.}
proc RSA_private_decrypt*(flen: cint, fr: ptr uint8, to: ptr uint8, rsa: PRSA, padding: PaddingType): cint {.cdecl,
    dynlib: DLLUtilName, importc.}
proc RSA_public_decrypt*(flen: cint, fr: ptr uint8, to: ptr uint8, rsa: PRSA, padding: PaddingType): cint {.cdecl,
    dynlib: DLLUtilName, importc.}
proc RSA_free*(rsa: PRSA) {.cdecl, dynlib: DLLUtilName, importc.}
proc RSA_size*(rsa: PRSA): cint {.cdecl, dynlib: DLLUtilName, importc.}

# sha types
proc EVP_md_null*(): EVP_MD   {.cdecl, importc.}
proc EVP_md2*(): EVP_MD       {.cdecl, importc.}
proc EVP_md4*(): EVP_MD       {.cdecl, importc.}
proc EVP_md5*(): EVP_MD       {.cdecl, importc.}
proc EVP_sha*(): EVP_MD       {.cdecl, importc.}
proc EVP_sha1*(): EVP_MD      {.cdecl, importc.}
proc EVP_dss*(): EVP_MD       {.cdecl, importc.}
proc EVP_dss1*(): EVP_MD      {.cdecl, importc.}
proc EVP_ecdsa*(): EVP_MD     {.cdecl, importc.}
proc EVP_sha224*(): EVP_MD    {.cdecl, importc.}
proc EVP_sha256*(): EVP_MD    {.cdecl, importc.}
proc EVP_sha384*(): EVP_MD    {.cdecl, importc.}
proc EVP_sha512*(): EVP_MD    {.cdecl, importc.}
proc EVP_mdc2*(): EVP_MD      {.cdecl, importc.}
proc EVP_ripemd160*(): EVP_MD {.cdecl, importc.}
proc EVP_whirlpool*(): EVP_MD {.cdecl, importc.}
proc EVP_MD_size*(md: EVP_MD): cint {.cdecl, importc.}

# hmac functions
proc HMAC*(evp_md: EVP_MD; key: pointer; key_len: cint; d: cstring; n: csize_t; md: cstring; md_len: ptr cuint): cstring {.cdecl, importc.}

# RSA key functions
proc PEM_read_bio_PrivateKey*(bp: BIO, x: ptr EVP_PKEY, cb: pointer, u: pointer): EVP_PKEY {.cdecl, importc.}
proc EVP_PKEY_free*(p: EVP_PKEY)  {.cdecl, importc.}
proc EVP_DigestSignInit*(ctx: EVP_MD_CTX, pctx: ptr EVP_PKEY_CTX, typ: EVP_MD, e: ENGINE, pkey: EVP_PKEY): cint {.cdecl, importc.}
proc EVP_DigestInit_ex*(ctx: EVP_MD_CTX, typ: PEVP_MD, engine: SslPtr = nil): cint {.cdecl, importc.}
proc EVP_DigestUpdate*(ctx: EVP_MD_CTX, data: pointer, len: cuint): cint {.cdecl, importc.}
proc EVP_DigestFinal_ex*(ctx: EVP_MD_CTX, buffer: pointer, size: ptr cuint): cint {.cdecl, importc.}
proc EVP_DigestSignFinal*(ctx: EVP_MD_CTX, data: pointer, len: ptr csize_t): cint {.cdecl, importc.}
proc EVP_PKEY_CTX_new*(pkey: EVP_PKEY, e: ENGINE): EVP_PKEY_CTX {.cdecl, importc.}
proc EVP_PKEY_CTX_free*(pkeyCtx: EVP_PKEY_CTX) {.cdecl, importc.}
proc EVP_PKEY_sign_init*(c: EVP_PKEY_CTX): cint {.cdecl, importc.}

when defined(macosx) or defined(windows):
  proc EVP_MD_CTX_create*(): EVP_MD_CTX {.cdecl, importc.}
  proc EVP_MD_CTX_destroy*(ctx: EVP_MD_CTX) {.cdecl, importc.}
  proc EVP_MD_CTX_cleanup*(ctx: EVP_MD_CTX): cint {.cdecl, importc.}
else:
  # some times you will need this instead:
  proc EVP_MD_CTX_create*(): EVP_MD_CTX {.cdecl, importc: "EVP_MD_CTX_new".}
  proc EVP_MD_CTX_destroy*(ctx: EVP_MD_CTX) {.cdecl, importc: "EVP_MD_CTX_free".}
  proc EVP_MD_CTX_cleanup*(ctx: EVP_MD_CTX): cint {.cdecl, importc: "EVP_MD_CTX_cleanup".}

# <openssl/md5.h>
type
  MD5_LONG* = cuint
const
  MD5_CBLOCK* = 64
  MD5_LBLOCK* = int(MD5_CBLOCK div 4)
  MD5_DIGEST_LENGTH* = 16
type
  MD5_CTX* = object
    A,B,C,D,Nl,Nh: MD5_LONG
    data: array[MD5_LBLOCK, MD5_LONG]
    num: cuint

{.push callconv:cdecl, dynlib:DLLUtilName.}
proc md5_Init*(c: var MD5_CTX): cint{.importc: "MD5_Init".}
proc md5_Update*(c: var MD5_CTX; data: pointer; len: csize_t): cint{.importc: "MD5_Update".}
proc md5_Final*(md: cstring; c: var MD5_CTX): cint{.importc: "MD5_Final".}
proc md5*(d: ptr uint8; n: csize_t; md: ptr uint8): ptr uint8{.importc: "MD5".}
proc md5_Transform*(c: var MD5_CTX; b: ptr uint8){.importc: "MD5_Transform".}
{.pop.}

from std/strutils import toHex, toLowerAscii

proc hexStr(buf: cstring): string =
  # turn md5s output into a nice hex str
  result = newStringOfCap(32)
  for i in 0 ..< 16:
    result.add toHex(buf[i].ord, 2).toLowerAscii

proc md5_File*(file: string): string {.raises: [IOError,Exception].} =
  ## Generate MD5 hash for a file. Result is a 32 character
  # hex string with lowercase characters (like the output
  # of `md5sum`
  const
    sz = 512
  let f = open(file,fmRead)
  var
    buf: array[sz,char]
    ctx: MD5_CTX

  discard md5_Init(ctx)
  while (let bytes = f.readChars(buf); bytes > 0):
    discard md5_Update(ctx, buf[0].addr, cast[csize_t](bytes))

  discard md5_Final(buf[0].addr, ctx)
  f.close

  result = hexStr(addr buf)

proc md5_Str*(str: string): string =
  ## Generate MD5 hash for a string. Result is a 32 character
  ## hex string with lowercase characters
  var
    ctx: MD5_CTX
    res: array[MD5_DIGEST_LENGTH,char]
    input = str.cstring
  discard md5_Init(ctx)

  var i = 0
  while i < str.len:
    let L = min(str.len - i, 512)
    discard md5_Update(ctx, input[i].addr, cast[csize_t](L))
    i += L

  discard md5_Final(addr res, ctx)
  result = hexStr(addr res)

when defined(nimHasStyleChecks):
  {.pop.}


# Certificate validation
# On old openSSL version some of these symbols are not available
when not defined(nimDisableCertificateValidation):
  proc X509_get_subject_name*(a: PX509): PX509_NAME{.cdecl, dynlib: DLLSSLName, importc.}

  proc X509_get_issuer_name*(a: PX509): PX509_NAME{.cdecl, dynlib: DLLUtilName, importc.}

  proc X509_NAME_oneline*(a: PX509_NAME, buf: cstring, size: cint): cstring {.
    cdecl, dynlib:DLLSSLName, importc.}

  proc X509_NAME_get_text_by_NID*(subject:cstring, NID: cint, buf: cstring, size: cint): cint{.
    cdecl, dynlib:DLLSSLName, importc.}

  proc X509_check_host*(cert: PX509, name: cstring, namelen: cint, flags:cuint, peername: cstring): cint {.cdecl, dynlib: DLLSSLName, importc.}

  proc X509_free*(cert: PX509) {.cdecl, dynlib: DLLSSLName, importc.}

  # Certificates store

  type PX509_STORE* = SslPtr
  type PX509_OBJECT* = SslPtr

  {.push callconv:cdecl, dynlib:DLLUtilName, importc.}

  proc X509_OBJECT_new*(): PX509_OBJECT
  proc X509_OBJECT_free*(a: PX509_OBJECT)

  proc X509_STORE_new*(): PX509_STORE
  proc X509_STORE_free*(v: PX509_STORE)
  proc X509_STORE_lock*(ctx: PX509_STORE): cint
  proc X509_STORE_unlock*(ctx: PX509_STORE): cint
  proc X509_STORE_up_ref*(v: PX509_STORE): cint
  proc X509_STORE_set_flags*(ctx: PX509_STORE; flags: culong): cint
  proc X509_STORE_set_purpose*(ctx: PX509_STORE; purpose: cint): cint
  proc X509_STORE_set_trust*(ctx: PX509_STORE; trust: cint): cint
  proc X509_STORE_add_cert*(ctx: PX509_STORE; x: PX509): cint

  {.pop.}

  when isMainModule:
    # A simple certificate test
    let certbytes = readFile("certificate.der")
    let cert = d2i_X509(certbytes)
    let encoded = cert.i2d_X509()
    assert encoded == certbytes

# Application Layer Protocol Negociation extension (TLS-ALPN, RFC7301)
# Available in at least OpenSSL 1.1.1 and later, not sure if earlier
# --Iced Quinn

proc SSL_CTX_set_alpn_protos*(ctx: SslCtx; protos: cstring; protos_len: cuint): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_set_alpn_protos*(ssl: SslPtr; protos: cstring; protos_len: cuint): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_alpn_select_cb*(ctx: SslCtx; cb: proc(ssl: SslPtr; out_proto: ptr cstring; outlen: cstring; in_proto: cstring; inlen: cuint; arg: pointer): cint {.cdecl.}; arg: pointer): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_get0_alpn_selected*(ssl: SslPtr; data: ptr cstring; len: ptr cuint) {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_next_protos_advertised_cb*(ctx: SslCtx; cb: proc(ssl: SslPtr; out_proto: ptr cstring; outlen: ptr cuint; arg: pointer): cint {.cdecl.}; arg: pointer) {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_CTX_set_next_proto_select_cb*(ctx: SslCtx; cb: proc(s: SslPtr; out_proto: cstring; outlen: cstring; in_proto: cstring; inlen: cuint; arg: pointer): cint {.cdecl.}; arg: pointer) {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_select_next_proto*(out_proto: ptr cstring; outlen: cstring; server: cstring; server_len: cuint; client: cstring; client_len: cuint): cint {.cdecl, dynlib: DLLSSLName, importc.}
proc SSL_get0_next_proto_negotiated*(s: SslPtr; data: ptr cstring; len: ptr cuint) {.cdecl, dynlib: DLLSSLName, importc.}

