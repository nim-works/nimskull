const nimLibcryptoLinkFlags {.strdefine.} = "-lcrypto"

{.used.} # This module is only used for the side-effect
{.passl: nimLibcryptoLinkFlags.}
