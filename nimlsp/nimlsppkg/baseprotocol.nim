import std/[json, parseutils, streams, strformat,
            strutils, os]
from std/uri import decodeUrl, parseUri
when defined(debugCommunication):
  import logger

type
  BaseProtocolError* = object of CatchableError

  MalformedFrame* = object of BaseProtocolError
  UnsupportedEncoding* = object of BaseProtocolError

  UriParseError* = object of Defect
    uri*: string

proc pathToUri*(path: string): string =
  # This is a modified copy of encodeUrl in the uri module. This doesn't encode
  # the / character, meaning a full file path can be passed in without breaking
  # it.
  result = newStringOfCap(path.len + path.len shr 2) # assume 12% non-alnum-chars
  when defined(windows):
    result.add '/'
  for c in path:
    case c
    # https://tools.ietf.org/html/rfc3986#section-2.3
    of 'a'..'z', 'A'..'Z', '0'..'9', '-', '.', '_', '~', '/': result.add c
    of '\\':
      when defined(windows):
        result.add '/'
      else:
        result.add '%'
        result.add toHex(ord(c), 2)
    else:
      result.add '%'
      result.add toHex(ord(c), 2)

proc uriToPath*(uri: string): string =
  ## Convert an RFC 8089 file URI to a native, platform-specific, absolute path.
  #let startIdx = when defined(windows): 8 else: 7
  #normalizedPath(uri[startIdx..^1])
  let parsed = parseUri(uri)
  if parsed.scheme != "file":
    var e = newException(UriParseError, &"Invalid scheme: {parsed.scheme}, only \"file\" is supported")
    e.uri = uri
    raise e
  if parsed.hostname != "":
    var e = newException(UriParseError, &"Invalid hostname: {parsed.hostname}, only empty hostname is supported")
    e.uri = uri
    raise e
  return normalizedPath(
    when defined(windows):
      parsed.path[1..^1]
    else:
      parsed.path).decodeUrl

proc parseId*(node: JsonNode): string =
  if node == nil: return
  if node.kind == JString:
    node.getStr
  elif node.kind == JInt:
    $node.getInt
  else:
    ""

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

