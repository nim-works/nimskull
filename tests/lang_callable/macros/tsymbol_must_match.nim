discard """
  description: "Symbols not matching the definition must be rejected"
  action: reject
  cmd: "nim check --msgFormat:sexp $options $file"
  nimoutFormat: sexp
"""

import std/macros

proc nameExpr(n: NimNode): NimNode =
  # Extracts the expression containing the replacement name
  case n.kind
  of nnkStmtList:
    nameExpr n[^1]
  of nnkLetSection, nnkVarSection, nnkConstSection:
    n[0][0]
  of RoutineNodes:
    n.name
  else:
    unreachable("unsupported kind: " & $n.kind)

macro replaceName(sym: typed, def: untyped): untyped =
  ## Replaces the identifier in the name slot of `def` with the symbol in the
  ## last expression of `sym`.
  let def = if def.kind == nnkStmtList: def[0] else: def
  let sym = nameExpr sym

  case def.kind
  of RoutineNodes:
    copyLineInfo(sym, def.name)
    def.name = sym
    def
  of nnkVarSection, nnkLetSection, nnkConstSection, nnkTypeSection:
    if def.len != 1 or def[0].len != 3:
      error "there should only be one identifier in a var/let/const/type section", def
    # Replace the name in the first IdentDef
    copyLineInfo(sym, def[0][0])
    def[0][0] = sym
    def
  else:
    unreachable()

# test each routine definition

replaceName:
  const p = 10
do:
  proc p() = #[tt.Error
      ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  proc f() = discard
do:
  func f() = #[tt.Error
      ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  var iter = 0
do:
  iterator iter() = #[tt.Error
          ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  proc conv() = discard
do:
  converter conv() = #[tt.Error
           ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  proc meth() = discard
do:
  method meth(x: RootObj) {.base.} = #[tt.Error
        ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  template m() = discard
do:
  macro m() = #[tt.Error
       ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  macro t() = discard
do:
  template t() = #[tt.Error
          ^ (SemSymbolKindMismatch)]#
    discard

replaceName:
  let a = 0
do:
  var a: int #[tt.Error
     ^ (SemSymbolKindMismatch)]#

replaceName:
  proc c() = discard
do:
  const c = 10 #[tt.Error
       ^ (SemSymbolKindMismatch)]#

replaceName:
  func T() = discard
do:
  type T = int #[tt.Error
      ^ (SemSymbolKindMismatch)]#
