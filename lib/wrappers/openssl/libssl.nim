const nimLibsslLinkFlags {.strdefine.} = "-lssl"

{.used.} # This module is only used for the side-effect
{.passl: nimLibsslLinkFlags.}
