discard """
  targets: "!js"
  matrix: "--threads:on -d:ssl"
  disabled: "openbsd"
  disabled: "freebsd"
  disabled: "windows"
"""

#[
disabled: see https://github.com/timotheecour/Nim/issues/528
]#

import strutils

import nativesockets, os, httpclient

proc syncTest() =
  var client = newHttpClient()
  var resp = client.request("http://example.com/", HttpGet)
  doAssert(resp.code.is2xx)
  doAssert("<title>Example Domain</title>" in resp.body)

  resp = client.request("http://example.com/404")
  doAssert(resp.code.is4xx)
  doAssert(resp.code == Http404)
  doAssert(resp.status == $Http404)

  resp = client.request("https://google.com/")
  doAssert(resp.code.is2xx or resp.code.is3xx)

  # getContent
  try:
    discard client.getContent("https://google.com/404")
    doAssert(false, "HttpRequestError should have been raised")
  except HttpRequestError:
    discard
  except:
    doAssert(false, "HttpRequestError should have been raised")

  client.close()

  # SIGSEGV on HEAD body read: issue #16743
  block:
    let client = newHttpClient()
    let resp = client.head("http://httpbin.org/head")
    doAssert(resp.body == "")

syncTest()
