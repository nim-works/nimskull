import reports, options

proc reportHook*(conf: ConfigRef, r: Report) =
  echo r
