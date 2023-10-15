discard """
  errormsg: "type mismatch: got <proc [TScgi](s: TScgi: ScgiState or AsyncScgiState) | proc (client: AsyncSocket, headers: StringTableRef, input: string){.noSideEffect, gcsafe, locks: 0.}>"
  line: 24
"""

# Fake ScgiState objects, from now-deprecated scgi module
type
  ScgiState* = object of RootObj ## SCGI state object
  AsyncScgiState* = object of RootObj ## SCGI state object
  AsyncSocket* = object of RootObj

#bug #442
import strtabs
proc handleSCGIRequest[TScgi: ScgiState | AsyncScgiState](s: TScgi) =
  discard
proc handleSCGIRequest(client: AsyncSocket, headers: StringTableRef,
                       input: string) =
  discard

proc test(handle: proc (client: AsyncSocket, headers: StringTableRef,
                        input: string), b: int) =
  discard

test(handleSCGIRequest)
