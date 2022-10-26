import mresolution_identifier_2

type
  TExport* = enum x, y, z
  TExport2* = enum mDec, mInc, mAssign

proc localDefinitionShadowsImportedSuccessfully() =
  var x: TExport
  discard
