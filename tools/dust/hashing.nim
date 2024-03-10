include compiler/sem/sighashes # because too few things are exported

const
  considerAll = {ConsiderFlag.low .. ConsiderFlag.high}

proc hashNode*(n: PNode; flags: set[ConsiderFlag] = considerAll): SigHash =
  var c: MD5Context
  md5Init c
  hashTree(c, n, flags)
  md5Final(c, result.MD5Digest)