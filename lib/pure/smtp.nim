#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Dominik Picheta
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements the SMTP client protocol as specified by RFC 5321,
## this can be used to send mail to any SMTP Server.
##
## This module also implements the protocol used to format messages,
## as specified by RFC 2822.
##
## Example gmail use:
##
##
## .. code-block:: Nim
##   var msg = createMessage("Hello from Nim's SMTP",
##                           "Hello!.\n Is this awesome or what?",
##                           @["foo@gmail.com"])
##   let smtpConn = newSmtp(useSsl = true, debug=true)
##   smtpConn.connect("smtp.gmail.com", Port 465)
##   smtpConn.auth("username", "password")
##   smtpConn.sendmail("username@gmail.com", @["foo@gmail.com"], $msg)
##
##
## Example for startTls use:
##
##
## .. code-block:: Nim
##   var msg = createMessage("Hello from Nim's SMTP",
##                           "Hello!.\n Is this awesome or what?",
##                           @["foo@gmail.com"])
##   let smtpConn = newSmtp(debug=true)
##   smtpConn.connect("smtp.mailtrap.io", Port 2525)
##   smtpConn.startTls()
##   smtpConn.auth("username", "password")
##   smtpConn.sendmail("username@gmail.com", @["foo@gmail.com"], $msg)
##
##
## For SSL support this module relies on OpenSSL. If you want to
## enable SSL, compile with `-d:ssl`.

import std/[net, strutils, strtabs, base64, os]

export Port

type
  Message* = object
    msgTo: seq[string]
    msgCc: seq[string]
    msgSubject: string
    msgOtherHeaders: StringTableRef
    msgBody: string

  ReplyError* = object of IOError

  SmtpBase[SocketType] = ref object
    sock: SocketType
    address: string
    debug: bool

  Smtp* = SmtpBase[Socket]

proc containsNewline(xs: seq[string]): bool =
  for x in xs:
    if x.contains({'\c', '\L'}):
      return true

proc debugSend*(smtp: Smtp, cmd: string) =
  ## Sends `cmd` on the socket connected to the SMTP server.
  ##
  ## If the `smtp` object was created with `debug` enabled,
  ## debugSend will invoke `echo("C:" & cmd)` before sending.
  ##
  ## This is a lower level proc and not something that you typically
  ## would need to call when using this module. One exception to
  ## this is if you are implementing any
  ## `SMTP extensions<https://en.wikipedia.org/wiki/Extended_SMTP>`_.

  if smtp.debug:
    echo("C:" & cmd)
  smtp.sock.send(cmd)

proc debugRecv*(smtp: Smtp): string =
  ## Receives a line of data from the socket connected to the
  ## SMTP server.
  ##
  ## If the `smtp` object was created with `debug` enabled,
  ## debugRecv will invoke `echo("S:" & result.string)` after
  ## the data is received.
  ##
  ## This is a lower level proc and not something that you typically
  ## would need to call when using this module. One exception to
  ## this is if you are implementing any
  ## `SMTP extensions<https://en.wikipedia.org/wiki/Extended_SMTP>`_.

  result = smtp.sock.recvLine()
  if smtp.debug:
    echo("S:" & result)

proc quitExcpt(smtp: Smtp, msg: string) =
  smtp.debugSend("QUIT")
  raise newException(ReplyError, msg)

const compiledWithSsl = defined(ssl)

when not defined(ssl):
  let defaultSSLContext: SslContext = nil
else:
  var defaultSSLContext {.threadvar.}: SslContext

  proc getSSLContext(): SslContext =
    if defaultSSLContext == nil:
      defaultSSLContext = newContext(verifyMode = CVerifyNone)
    result = defaultSSLContext

proc createMessage*(mSubject, mBody: string, mTo, mCc: seq[string],
                otherHeaders: openArray[tuple[name, value: string]]): Message =
  ## Creates a new MIME compliant message.
  ##
  ## You need to make sure that `mSubject`, `mTo` and `mCc` don't contain
  ## any newline characters. Failing to do so will raise `AssertionDefect`.
  doAssert(not mSubject.contains({'\c', '\L'}),
           "'mSubject' shouldn't contain any newline characters")
  doAssert(not (mTo.containsNewline() or mCc.containsNewline()),
           "'mTo' and 'mCc' shouldn't contain any newline characters")

  result.msgTo = mTo
  result.msgCc = mCc
  result.msgSubject = mSubject
  result.msgBody = mBody
  result.msgOtherHeaders = newStringTable()
  for n, v in items(otherHeaders):
    result.msgOtherHeaders[n] = v

proc createMessage*(mSubject, mBody: string, mTo,
                    mCc: seq[string] = @[]): Message =
  ## Alternate version of the above.
  ##
  ## You need to make sure that `mSubject`, `mTo` and `mCc` don't contain
  ## any newline characters. Failing to do so will raise `AssertionDefect`.
  doAssert(not mSubject.contains({'\c', '\L'}),
           "'mSubject' shouldn't contain any newline characters")
  doAssert(not (mTo.containsNewline() or mCc.containsNewline()),
           "'mTo' and 'mCc' shouldn't contain any newline characters")
  result.msgTo = mTo
  result.msgCc = mCc
  result.msgSubject = mSubject
  result.msgBody = mBody
  result.msgOtherHeaders = newStringTable()

proc `$`*(msg: Message): string =
  ## stringify for `Message`.
  result = ""
  if msg.msgTo.len() > 0:
    result = "TO: " & msg.msgTo.join(", ") & "\c\L"
  if msg.msgCc.len() > 0:
    result.add("CC: " & msg.msgCc.join(", ") & "\c\L")
  # TODO: Folding? i.e when a line is too long, shorten it...
  result.add("Subject: " & msg.msgSubject & "\c\L")
  for key, value in pairs(msg.msgOtherHeaders):
    result.add(key & ": " & value & "\c\L")

  result.add("\c\L")
  result.add(msg.msgBody)

proc newSmtp*(useSsl = false, debug = false,
              sslContext: SslContext = nil): Smtp =
  ## Creates a new `Smtp` instance.
  new result
  result.debug = debug
  result.sock = newSocket()
  if useSsl:
    when compiledWithSsl:
      if sslContext == nil:
        getSSLContext().wrapSocket(result.sock)
      else:
        sslContext.wrapSocket(result.sock)
    else:
      {.error: "SMTP module compiled without SSL support".}

proc checkReply*(smtp: Smtp, reply: string) =
  ## Calls `debugRecv<#debugRecv,Smtp>`_ and checks that the received
  ## data starts with `reply`. If the received data does not start
  ## with `reply`, then a `QUIT` command will be sent to the SMTP
  ## server and a `ReplyError` exception will be raised.
  ##
  ## This is a lower level proc and not something that you typically
  ## would need to call when using this module. One exception to
  ## this is if you are implementing any
  ## `SMTP extensions<https://en.wikipedia.org/wiki/Extended_SMTP>`_.

  var line = smtp.debugRecv()
  if not line.startsWith(reply):
    quitExcpt(smtp, "Expected " & reply & " reply, got: " & line)

proc helo*(smtp: Smtp) =
  # Sends the HELO request
  smtp.debugSend("HELO " & smtp.address & "\c\L")
  smtp.checkReply("250")

proc connect*(smtp: Smtp,
              address: string, port: Port) =
  ## Establishes a connection with a SMTP server.
  ## May fail with ReplyError or with a socket error.
  smtp.address = address
  smtp.sock.connect(address, port)
  smtp.checkReply("220")
  smtp.helo()

proc startTls*(smtp: Smtp, sslContext: SslContext = nil) =
  ## Put the SMTP connection in TLS (Transport Layer Security) mode.
  ## May fail with ReplyError
  smtp.debugSend("STARTTLS\c\L")
  smtp.checkReply("220")
  when compiledWithSsl:
    if sslContext == nil:
      getSSLContext().wrapConnectedSocket(smtp.sock, handshakeAsClient)
    else:
      sslContext.wrapConnectedSocket(smtp.sock, handshakeAsClient)
    smtp.helo()
  else:
    {.error: "SMTP module compiled without SSL support".}

proc auth*(smtp: Smtp, username, password: string) =
  ## Sends an AUTH command to the server to login as the `username`
  ## using `password`.
  ## May fail with ReplyError.

  smtp.debugSend("AUTH LOGIN\c\L")
  smtp.checkReply("334") # TODO: Check whether it's asking for the "Username:"
                               # i.e "334 VXNlcm5hbWU6"
  smtp.debugSend(encode(username) & "\c\L")
  smtp.checkReply("334") # TODO: Same as above, only "Password:" (I think?)

  smtp.debugSend(encode(password) & "\c\L")
  smtp.checkReply("235") # Check whether the authentication was successful.

proc sendMail*(smtp: Smtp, fromAddr: string,
               toAddrs: seq[string], msg: string) =
  ## Sends `msg` from `fromAddr` to the addresses specified in `toAddrs`.
  ## Messages may be formed using `createMessage` by converting the
  ## Message into a string.
  ##
  ## You need to make sure that `fromAddr` and `toAddrs` don't contain
  ## any newline characters. Failing to do so will raise `AssertionDefect`.
  doAssert(not (toAddrs.containsNewline() or fromAddr.contains({'\c', '\L'})),
           "'toAddrs' and 'fromAddr' shouldn't contain any newline characters")

  smtp.debugSend("MAIL FROM:<" & fromAddr & ">\c\L")
  smtp.checkReply("250")
  for address in items(toAddrs):
    smtp.debugSend("RCPT TO:<" & address & ">\c\L")
    smtp.checkReply("250")

  # Send the message
  smtp.debugSend("DATA " & "\c\L")
  smtp.checkReply("354")
  smtp.sock.send(msg & "\c\L")
  smtp.debugSend(".\c\L")
  smtp.checkReply("250")

proc close*(smtp: Smtp) =
  ## Disconnects from the SMTP server and closes the socket.
  smtp.debugSend("QUIT\c\L")
  smtp.sock.close()
