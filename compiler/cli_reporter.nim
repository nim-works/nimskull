import reports
import std/[strformat, strutils, options]

import hmisc/hasts/json_serde

## Implementation of the default command-line error hook. All the
## pretty-printed messages are constructed in this module.

using writer: var JsonSerializer

jsonSerdeFor(FileIndex, loadJsonDistinct, writeJsonDistinct)

proc writeJson(writer; node: PNode) = writer.writeJson("[TODO write PNode]")
proc writeJson(writer; node: PSym) = writer.writeJson("[TODO write PSym]")
proc writeJson(writer; node: PType) = writer.writeJson("[TODO write PType]")

proc report(r: SemReport)      = echo r
proc report(r: ParserReport)   = echo r
proc report(r: LexerReport)    = echo r
proc report(r: InternalReport) = echo r
proc report(r: ExternalReport) = echo r
proc report(r: DebugReport)    = echo r
proc report(r: BackendReport)  = echo r
proc report(r: CmdReport)      = echo r

proc reportHook*(r: Report) = echo toJson(r)
