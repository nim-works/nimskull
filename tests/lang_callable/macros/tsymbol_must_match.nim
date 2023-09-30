discard """
  description: "Generated symbols not matching the definition must be rejected"
  action: reject
  cmd: "nim check --msgFormat:sexp --filenames:canonical $options $file"
  nimoutFormat: sexp
"""

import std/macros

macro replaceName(kind: static[NimSymKind], def: untyped): untyped =
  ## Replaces the identifier in the name slot of `def` with a generated symbol
  ## of the given `kind`.
  let def = if def.kind == nnkStmtList: def[0] else: def

  def.expectKind(RoutineNodes)

  proc genSym(kind: NimSymKind, ident: NimNode): NimNode =
    result = genSym(kind, ident.strVal)
    copyLineInfo(result, ident)

  def.name = genSym(kind, def.name)
  result = def

# test each routine definition

replaceName(nskConst):
  proc p() = #[tt.Error
      ^ (SemSymbolKindMismatch)]#
    discard

replaceName(nskProc):
  func f() = #[tt.Error
      ^ (SemSymbolKindMismatch)]#
    discard

replaceName(nskVar):
  iterator iter() = #[tt.Error
          ^ (SemSymbolKindMismatch)]#
    discard

replaceName(nskForVar):
  converter conv() = #[tt.Error
           ^ (SemSymbolKindMismatch)]#
    discard

replaceName(nskForVar):
  method meth() = #[tt.Error
        ^ (SemSymbolKindMismatch)]#
    discard

replaceName(nskTemplate):
  macro m() = #[tt.Error
       ^ (SemSymbolKindMismatch)]#
    discard

replaceName(nskMacro):
  template t() = #[tt.Error
          ^ (SemSymbolKindMismatch)]#
    discard

macro t1(): untyped =
  # test wrong symbol kind passed in non-routine context
  # correctly report error
  let b = genSym(nskLet, ident = "a")#[tt.Error
               ^ (SemSymbolKindMismatch)]#
  result = newVarStmt(b, newLit 1)

t1()
