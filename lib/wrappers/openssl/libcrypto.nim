import os

const nimOpensslLibDir {.strdefine.} = getEnv("OPENSSL_LIB_DIR")

const nimLibcryptoName {.strdefine.} =
  when defined(vcc):
    "libcrypto"
  else:
    "crypto"

proc generateLinkFlags(): string =
  if nimOpensslLibDir.len > 0:
    result.add quoteShell("-L" & nimOpensslLibDir)
    result.add ' '

  result.add quoteShell("-l" & nimLibcryptoName)

const nimLibcryptoLinkFlags {.strdefine.} = generateLinkFlags()

{.used.} # This module is only used for the side-effect
{.passl: nimLibcryptoLinkFlags.}

