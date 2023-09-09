import std/[json, parseutils, streams, strformat,
            strutils]
when defined(debugCommunication):
  import logger

type
  BaseProtocolError* = object of CatchableError

  MalformedFrame* = object of BaseProtocolError
  UnsupportedEncoding* = object of BaseProtocolError

proc skipWhitespace(x: string, pos: int): int =
  result = pos
  while result < x.len and x[result] in Whitespace:
    inc result

proc sendFrame*(s: Stream, frame: string) =
  when defined(debugCommunication):
    frameLog(Out, frame)
  when s is Stream:
    s.write frame
    s.flush
  else:
    s.write frame

proc formFrame*(data: JsonNode): string =
  var frame = newStringOfCap(1024)
  toUgly(frame, data)
  result = &"Content-Length: {frame.len}\r\n\r\n{frame}"

proc sendJson*(s: Stream, data: JsonNode) =
  let frame = formFrame(data)
  s.sendFrame(frame)

proc readFrame*(s: Stream): string =
  var contentLen = -1
  var headerStarted = false
  var ln: string
  while true:
    ln = s.readLine()
    if ln.len != 0:
      headerStarted = true
      let sep = ln.find(':')
      if sep == -1:
        raise newException(MalformedFrame, "invalid header line: " & ln)

      let valueStart = ln.skipWhitespace(sep + 1)

      case ln[0 ..< sep]
      of "Content-Type":
        if ln.find("utf-8", valueStart) == -1 and ln.find("utf8", valueStart) == -1:
          raise newException(UnsupportedEncoding, "only utf-8 is supported")
      of "Content-Length":
        if parseInt(ln, contentLen, valueStart) == 0:
          raise newException(MalformedFrame, "invalid Content-Length: " &
                                              ln.substr(valueStart))
      else:
        # Unrecognized headers are ignored
        discard
      when defined(debugCommunication):
        frameLog(In, ln)
    elif not headerStarted:
      continue
    else:
      when defined(debugCommunication):
        frameLog(In, ln)
      if contentLen != -1:
        when s is Stream:
          var buf = s.readStr(contentLen)
        else:
          var
            buf = newString(contentLen)
            head = 0
          while contentLen > 0:
            let bytesRead = s.readBuffer(buf[head].addr, contentLen)
            if bytesRead == 0:
              raise newException(MalformedFrame, "Unexpected EOF")
            contentLen -= bytesRead
            head += bytesRead
        when defined(debugCommunication):
          frameLog(In, buf)
        return buf
      else:
        raise newException(MalformedFrame, "missing Content-Length header")

