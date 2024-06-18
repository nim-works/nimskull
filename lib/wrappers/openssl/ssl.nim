import libssl

import safestack
import x509

import types
export SSL, SSL_CTX

type
  SSL_METHOD* {.pure.} = object
  SSL_SESSION* {.pure.} = object
  SSL_psk_client_cb_func* = proc (ssl: ptr SSL, hint: cstring, identity: cstring, max_identity_len: cuint, psk: ptr UncheckedArray[cuchar], max_psk_len: cuint): cuint {.cdecl.}
  SSL_psk_server_cb_func* = proc (ssl: ptr SSL, identity: cstring, psk: ptr UncheckedArray[cuchar], max_psk_len: cuint): cuint {.cdecl.}
  SSL_psk_use_session_cb_func* = proc (ssl: ptr SSL, md: ptr EVP_MD, id: ptr cstring, idlen: ptr csize_t, sess: ptr ptr SSL_SESSION) {.cdecl.}
  SSL_psk_find_session_cb_func* = proc (ssl: ptr SSL, identity: cstring, identity_len: csize_t, sess: ptr ptr SSL_SESSION) {.cdecl.}
  SSL_verify_cb* = proc (preverify_ok: cint, x509_ctx: ptr X509_STORE_CTX): cint {.cdecl.}

const
  SSL_CTRL_GET_CLIENT_CERT_REQUEST* = 9
  SSL_CTRL_GET_NUM_RENEGOTIATIONS* = 10
  SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS* = 11
  SSL_CTRL_GET_TOTAL_RENEGOTIATIONS* = 12
  SSL_CTRL_GET_FLAGS* = 13
  SSL_CTRL_EXTRA_CHAIN_CERT* = 14
  SSL_CTRL_SET_MSG_CALLBACK* = 15
  SSL_CTRL_SET_MSG_CALLBACK_ARG* = 16
  SSL_CTRL_SET_MTU* = 17
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
  SSL_CTRL_MODE* = 33
  SSL_CTRL_GET_READ_AHEAD* = 40
  SSL_CTRL_SET_READ_AHEAD* = 41
  SSL_CTRL_SET_SESS_CACHE_SIZE* = 42
  SSL_CTRL_GET_SESS_CACHE_SIZE* = 43
  SSL_CTRL_SET_SESS_CACHE_MODE* = 44
  SSL_CTRL_GET_SESS_CACHE_MODE* = 45
  SSL_CTRL_GET_MAX_CERT_LIST* = 50
  SSL_CTRL_SET_MAX_CERT_LIST* = 51
  SSL_CTRL_SET_MAX_SEND_FRAGMENT* = 52
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB* = 53
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG* = 54
  SSL_CTRL_SET_TLSEXT_HOSTNAME* = 55
  SSL_CTRL_SET_TLSEXT_DEBUG_CB* = 56
  SSL_CTRL_SET_TLSEXT_DEBUG_ARG* = 57
  SSL_CTRL_GET_TLSEXT_TICKET_KEYS* = 58
  SSL_CTRL_SET_TLSEXT_TICKET_KEYS* = 59
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB* = 63
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG* = 64
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE* = 65
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS* = 66
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS* = 67
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS* = 68
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS* = 69
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP* = 70
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP* = 71
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB* = 75
  SSL_CTRL_SET_SRP_VERIFY_PARAM_CB* = 76
  SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB* = 77
  SSL_CTRL_SET_SRP_ARG* = 78
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME* = 79
  SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH* = 80
  SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD* = 81
  DTLS_CTRL_GET_TIMEOUT* = 73
  DTLS_CTRL_HANDLE_TIMEOUT* = 74
  SSL_CTRL_GET_RI_SUPPORT* = 76
  SSL_CTRL_CLEAR_MODE* = 78
  SSL_CTRL_SET_NOT_RESUMABLE_SESS_CB* = 79
  SSL_CTRL_GET_EXTRA_CHAIN_CERTS* = 82
  SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS* = 83
  SSL_CTRL_CHAIN* = 88
  SSL_CTRL_CHAIN_CERT* = 89
  SSL_CTRL_GET_GROUPS* = 90
  SSL_CTRL_SET_GROUPS* = 91
  SSL_CTRL_SET_GROUPS_LIST* = 92
  SSL_CTRL_GET_SHARED_GROUP* = 93
  SSL_CTRL_SET_SIGALGS* = 97
  SSL_CTRL_SET_SIGALGS_LIST* = 98
  SSL_CTRL_CERT_FLAGS* = 99
  SSL_CTRL_CLEAR_CERT_FLAGS* = 100
  SSL_CTRL_SET_CLIENT_SIGALGS* = 101
  SSL_CTRL_SET_CLIENT_SIGALGS_LIST* = 102
  SSL_CTRL_GET_CLIENT_CERT_TYPES* = 103
  SSL_CTRL_SET_CLIENT_CERT_TYPES* = 104
  SSL_CTRL_BUILD_CERT_CHAIN* = 105
  SSL_CTRL_SET_VERIFY_CERT_STORE* = 106
  SSL_CTRL_SET_CHAIN_CERT_STORE* = 107
  SSL_CTRL_GET_PEER_SIGNATURE_NID* = 108
  SSL_CTRL_GET_PEER_TMP_KEY* = 109
  SSL_CTRL_GET_RAW_CIPHERLIST* = 110
  SSL_CTRL_GET_EC_POINT_FORMATS* = 111
  SSL_CTRL_GET_CHAIN_CERTS* = 115
  SSL_CTRL_SELECT_CURRENT_CERT* = 116
  SSL_CTRL_SET_CURRENT_CERT* = 117
  SSL_CTRL_SET_DH_AUTO* = 118
  DTLS_CTRL_SET_LINK_MTU* = 120
  DTLS_CTRL_GET_LINK_MIN_MTU* = 121
  SSL_CTRL_GET_EXTMS_SUPPORT* = 122
  SSL_CTRL_SET_MIN_PROTO_VERSION* = 123
  SSL_CTRL_SET_MAX_PROTO_VERSION* = 124
  SSL_CTRL_SET_SPLIT_SEND_FRAGMENT* = 125
  SSL_CTRL_SET_MAX_PIPELINES* = 126
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE* = 127
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB* = 128
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB_ARG* = 129
  SSL_CTRL_GET_MIN_PROTO_VERSION* = 130
  SSL_CTRL_GET_MAX_PROTO_VERSION* = 131
  SSL_CTRL_GET_SIGNATURE_NID* = 132
  SSL_CTRL_GET_TMP_KEY* = 133
  SSL_CTRL_GET_NEGOTIATED_GROUP* = 134

  SSL_ERROR_NONE* = 0
  SSL_ERROR_SSL* = 1
  SSL_ERROR_WANT_READ* = 2
  SSL_ERROR_WANT_WRITE* = 3
  SSL_ERROR_WANT_X509_LOOKUP* = 4
  SSL_ERROR_SYSCALL* = 5
  SSL_ERROR_ZERO_RETURN* = 6
  SSL_ERROR_WANT_CONNECT* = 7
  SSL_ERROR_WANT_ACCEPT* = 8
  SSL_ERROR_WANT_ASYNC* = 9
  SSL_ERROR_WANT_ASYNC_JOB* = 10
  SSL_ERROR_WANT_CLIENT_HELLO_CB* = 11
  SSL_ERROR_WANT_RETRY_VERIFY* = 12

  SSL_FILETYPE_ASN1* = X509_FILETYPE_ASN1
  SSL_FILETYPE_PEM* = X509_FILETYPE_PEM

  SSL_MAX_SSL_SESSION_ID_LENGTH* = 32
  SSL_MAX_SID_CTX_LENGTH* = 32

  SSL_VERIFY_NONE* = 0x00
  SSL_VERIFY_PEER* = 0x01
  SSL_VERIFY_FAIL_IF_NO_PEER_CERT* = 0x02
  SSL_VERIFY_CLIENT_ONCE* = 0x04
  SSL_VERIFY_POST_HANDSHAKE* = 0x08

  SSL3_VERSION* = 0x0300
  TLS1_VERSION* = 0x0301
  TLS1_1_VERSION* = 0x0302
  TLS1_2_VERSION* = 0x0303
  TLS1_3_VERSION* = 0x0304

  TLSEXT_NAMETYPE_host_name* = 0

proc SSL_CTX_new*(`method`: ptr SSL_METHOD): ptr SSL_CTX {.importc, cdecl.}
proc SSL_CTX_up_ref*(ctx: ptr SSL_CTX): cint {.importc, cdecl.}
proc SSL_CTX_free*(ctx: ptr SSL_CTX) {.importc, cdecl.}

proc SSL_CTX_ctrl*(ctx: ptr SSL_CTX, cmd: cint, large: clong, parg: pointer): clong {.importc, cdecl.}
proc SSL_CTX_set_min_proto_version*(ctx: ptr SSL_CTX, version: cint): cint {.inline.} =
  cint: SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION, clong(version), nil)

proc SSL_CTX_set_cipher_list*(ctx: ptr SSL_CTX, str: cstring): cint {.importc, cdecl.}
proc SSL_CTX_set_ciphersuites*(ctx: ptr SSL_CTX, str: cstring): cint {.importc, cdecl.}
proc SSL_CTX_set_default_verify_dir*(ctx: ptr SSL_CTX, CApath: cstring): cint {.importc, cdecl.}
proc SSL_CTX_set_default_verify_file*(ctx: ptr SSL_CTX, CAfile: cstring): cint {.importc, cdecl.}
proc SSL_CTX_set_default_verify_paths*(ctx: ptr SSL_CTX, CAfile: cstring): cint {.importc, cdecl.}
proc SSL_CTX_set_default_verify_store*(ctx: ptr SSL_CTX, CAstore: cstring): cint {.importc, cdecl.}
proc SSL_CTX_set_ex_data*(ctx: ptr SSL_CTX, idx: cint, data: pointer): cint {.importc, cdecl.}
proc SSL_CTX_set_psk_client_callback*(ctx: ptr SSL_CTX, cb: SSL_psk_client_cb_func) {.importc, cdecl.}
proc SSL_CTX_set_psk_find_session_callback*(ctx: ptr SSL_CTX, cb: SSL_psk_find_session_cb_func) {.importc, cdecl.}
proc SSL_CTX_set_psk_server_callback*(ctx: ptr SSL_CTX, cb: SSL_psk_server_cb_func) {.importc, cdecl.}
proc SSL_CTX_set_psk_use_session_callback*(ctx: ptr SSL_CTX, cb: SSL_psk_use_session_cb_func) {.importc, cdecl.}
proc SSL_CTX_set_session_id_context*(ctx: ptr SSL_CTX, sid_ctx: ptr UncheckedArray[cuchar], sid_ctx_len: cuint): cint {.importc, cdecl.}
proc SSL_CTX_set_verify*(ctx: ptr SSL_CTX, mode: cint, verify_callback: SSL_verify_cb) {.importc, cdecl.}

proc SSL_CTX_get_ex_data*(ssl: ptr SSL_CTX, idx: cint): pointer {.importc, cdecl.}

proc SSL_CTX_load_verify_file*(ctx: ptr SSL_CTX, CAfile: cstring): cint {.importc, cdecl.}
proc SSL_CTX_load_verify_dir*(ctx: ptr SSL_CTX, CApath: cstring): cint {.importc, cdecl.}
proc SSL_CTX_load_verify_store*(ctx: ptr SSL_CTX, CAstore: cstring): cint {.importc, cdecl.}
proc SSL_CTX_load_verify_locations*(ctx: ptr SSL_CTX, CAfile: cstring, CApath: cstring): cint {.importc, cdecl.}

proc SSL_CTX_use_PrivateKey_file*(ctx: ptr SSL_CTX, file: cstring, typ: cint): cint {.importc, cdecl.}
proc SSL_CTX_use_certificate_chain_file*(ctx: ptr SSL_CTX, file: cstring): cint {.importc, cdecl.}
proc SSL_CTX_use_psk_identity_hint*(ctx: ptr SSL_CTX, hint: cstring): cint {.importc, cdecl.}
proc SSL_CTX_check_private_key*(ctx: ptr SSL_CTX): cint {.importc, cdecl.}

proc TLS_method*(): ptr SSL_METHOD {.importc, cdecl.}
proc TLS_server_method*(): ptr SSL_METHOD {.importc, cdecl.}
proc TLS_client_method*(): ptr SSL_METHOD {.importc, cdecl.}

proc SSL_new*(ctx: ptr SSL_CTX): ptr SSL {.importc, cdecl.}
proc SSL_up_ref*(ssl: ptr SSL): cint {.importc, cdecl.}
proc SSL_free*(ssl: ptr SSL) {.importc, cdecl.}

proc SSL_add1_host*(ssl: ptr SSL, hostname: cstring): cint {.importc, cdecl.}

proc SSL_in_init*(ssl: ptr SSL): cint {.importc, cdecl.}
proc SSL_in_before*(ssl: ptr SSL): cint {.importc, cdecl.}
proc SSL_is_init_finished*(ssl: ptr SSL): cint {.importc, cdecl.}

proc SSL_ctrl*(ssl: ptr SSL, cmd: cint, large: clong, parg: pointer): clong {.importc, cdecl.}
proc SSL_set_tlsext_host_name*(ssl: ptr SSL, name: cstring): cint {.inline.} =
  cint: SSL_ctrl(ssl, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name, name)

proc SSL_set_bio*(ssl: ptr SSL, rbio: ptr BIO, wbio: ptr BIO) {.importc, cdecl.}
proc SSL_set_fd*(ssl: ptr SSL, fd: cint): cint {.importc, cdecl.}
proc SSL_set_rfd*(ssl: ptr SSL, fd: cint): cint {.importc, cdecl.}
proc SSL_set_verify*(ctx: ptr SSL, mode: cint, verify_callback: SSL_verify_cb) {.importc, cdecl.}
proc SSL_set_wfd*(ssl: ptr SSL, fd: cint): cint {.importc, cdecl.}

proc SSL_set1_host*(ssl: ptr SSL, hostname: cstring): cint {.importc, cdecl.}

proc SSL_get_SSL_CTX*(ssl: ptr SSL): ptr SSL_CTX {.importc, cdecl.}
proc SSL_get_error*(ssl: ptr SSL, ret: cint): cint {.importc, cdecl.}
proc SSL_get_peer_cert_chain*(ssl: ptr SSL): ptr STACK_OF[X509] {.importc, cdecl.}
proc SSL_get_psk_identity*(ssl: ptr SSL): cstring {.importc, cdecl.}
proc SSL_get_psk_identity_hint*(ssl: ptr SSL): cstring {.importc, cdecl.}
proc SSL_get_verify_result*(ssl: ptr SSL): clong {.importc, cdecl.}

proc SSL_get0_verified_chain*(ssl: ptr SSL): ptr STACK_OF[X509] {.importc, cdecl.}

when not defined(nimOpenssl111):
  proc SSL_get0_peer_certificate*(ssl: ptr SSL): ptr X509 {.importc, cdecl.}
  proc SSL_get1_peer_certificate*(ssl: ptr SSL): ptr X509 {.importc, cdecl.}
else:
  proc SSL_get_peer_certificate*(ssl: ptr SSL): ptr X509 {.importc, cdecl.}

proc SSL_accept*(ssl: ptr SSL): cint {.importc, cdecl.}
proc SSL_connect*(ssl: ptr SSL): cint {.importc, cdecl.}
proc SSL_read*(ssl: ptr SSL, buf: pointer, num: cint): cint {.importc, cdecl.}
proc SSL_read_ex*(ssl: ptr SSL, buf: pointer, num: csize_t, readbytes: ptr csize_t): cint {.importc, cdecl.}
proc SSL_shutdown*(ssl: ptr SSL): cint {.importc, cdecl.}
proc SSL_write*(ssl: ptr SSL, buf: pointer, num: cint): cint {.importc, cdecl.}
proc SSL_write_ex*(ssl: ptr SSL, buf: pointer, num: csize_t, written: ptr csize_t): cint {.importc, cdecl.}
proc SSL_pending*(ssl: ptr SSL): cint {.importc, cdecl.}
