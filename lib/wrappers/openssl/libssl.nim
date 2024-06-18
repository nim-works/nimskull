import os

const nimOpensslLibDir {.strdefine.} = getEnv("OPENSSL_LIB_DIR")

const nimLibsslName {.strdefine.} =
  when defined(vcc):
    "libssl"
  else:
    "ssl"

proc generateLinkFlags(): string =
  if nimOpensslLibDir.len > 0:
    result.add quoteShell("-L" & nimOpensslLibDir)
    result.add ' '

  result.add quoteShell("-l" & nimLibsslName)

const nimLibsslLinkFlags {.strdefine.} = generateLinkFlags()

{.used.} # This module is only used for the side-effect
{.passl: nimLibsslLinkFlags.}
