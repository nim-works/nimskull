discard """
  cmd: "nim $target --threads:on -d:ssl $options $file"
  disabled: "openbsd"
"""

#            Nim - Basic SSL integration tests
#        (c) Copyright 2018 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#
## Warning: this test performs local networking.
## Test with:
## ./bin/nim c -d:ssl -p:. --threads:on -r tests/stdlib/thttpclient_ssl.nim

import
  httpclient,
  net,
  openssl/[ssl, err],
  os,
  strutils,
  times,
  unittest

# bogus self-signed certificate
const
  certFile = "tests/stdlib/web/thttpclient_ssl_cert.pem"
  keyFile = "tests/stdlib/web/thttpclient_ssl_key.pem"

proc log(msg: string) =
  when defined(ssldebug):
    echo "    [" & $epochTime() & "] " & msg
  # FIXME
  echo "    [" & $epochTime() & "] " & msg
  discard

proc sslErrCallback(str: cstring, len: csize_t, u: pointer): cint {.cdecl.} =
  cint: writeBuffer(stderr, str, len)

proc runServer(port: Port) {.thread.} =
  ## Run a trivial HTTPS server in a {.thread.}
  ## Exit after serving one request

  var socket = newSocket()
  defer: close(socket)
  socket.setSockOpt(OptReusePort, true)
  socket.bindAddr(port)

  var ctx = newContext(certFile=certFile, keyFile=keyFile)
  defer: destroyContext(ctx)

  ##  Handle one connection
  socket.listen()

  var client: Socket
  var address = ""

  log "server: ready"
  socket.acceptAddr(client, address)
  defer: close(client)
  log "server: incoming connection"

  var ssl: ptr SSL = SSL_new(ctx.context)
  defer: SSL_free(ssl)
  discard SSL_set_fd(ssl, client.getFd().cint)
  log "server: accepting connection"
  ErrClearError()
  if SSL_accept(ssl) <= 0:
    ERR_print_errors_cb(sslErrCallback, nil)
    return

  const reply = "HTTP/1.0 200 OK\r\nServer: test\r\nContent-type: text/html\r\nContent-Length: 0\r\n\r\n"
  log "server: sending reply"
  discard SSL_write(ssl, reply.cstring, reply.len)

  log "server: receiving data"
  var buf: array[4096, char]
  let read = SSL_read(ssl, addr buf[0], buf.len.cint)
  if read <= 0:
    ERR_print_errors_cb(sslErrCallback, nil)
    return
  log "server: received $# bytes" % $read
  log "closing"
  log "server: exited"


suite "SSL self signed certificate check":
  test "TCP socket":
    const port = 12347.Port
    let t = createThread(runServer, port)
    defer: joinThread(t)
    sleep(100)
    var sock = newSocket()
    defer: close(sock)
    var ctx = newContext()
    defer: destroyContext(ctx)
    ctx.wrapSocket(sock)
    try:
      log "client: connect"
      sock.connect("127.0.0.1", port)
      fail()
    except:
      let msg = getCurrentExceptionMsg()
      check(msg.contains("certificate verify failed"))

  test "HttpClient default: no check":
    const port = 12345.Port
    let t = createThread(runServer, port)
    defer: joinThread(t)
    sleep(100)

    var client = newHttpClient(sslContext=newContext(verifyMode=CVerifyNone))
    defer: close(client)
    try:
      log "client: connect"
      discard client.getContent("https://127.0.0.1:12345")
    except:
      let msg = getCurrentExceptionMsg()
      log "client: unexpected exception: " & msg
      fail()

  test "HttpClient with CVerifyPeer":
    const port = 12346.Port
    let t = createThread(runServer, port)
    defer: joinThread(t)
    sleep(100)

    var client = newHttpClient(sslContext=newContext(verifyMode=CVerifyPeer))
    defer: close(client)
    try:
      log "client: connect"
      discard client.getContent("https://127.0.0.1:12346")
      log "getContent should have raised an exception"
      fail()
    except:
      let msg = getCurrentExceptionMsg()
      log "client: exception: " & msg
      # SSL_shutdown:shutdown while in init
      if not (msg.contains("alert number 48") or
        msg.contains("certificate verify failed")):
        echo "CVerifyPeer exception: " & msg
        check(false)

  test "HttpClient with CVerifyPeerUseEnvVars":
    const port = 12346.Port
    let t = createThread(runServer, port)
    defer: joinThread(t)
    sleep(100)

    putEnv("SSL_CERT_FILE", getCurrentDir() / certFile)
    var client = newHttpClient(sslContext=newContext(verifyMode=CVerifyPeerUseEnvVars))
    defer: close(client)
    try:
      log "client: connect"
      discard client.getContent("https://127.0.0.1:12346")
    except:
      let msg = getCurrentExceptionMsg()
      log "client: exception: " & msg
      log "getContent should not have raised an exception"
      fail()
