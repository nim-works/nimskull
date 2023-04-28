discard """
  description: "test compile subcommand outputs an error for no filename arg"
  cmd: "nim c"
  target: native
  joinable: false
  action: reject
  errormsg: "command requires a filename"
  file: ""
"""