## Implements the main interface of the C code generator. Provides the
## routines for translating MIR bodies to CIR.

import
  compiler/backend/[
    cgendata,
    cir
  ],
  compiler/mir/[
    mirbodies,
    mirtrees
  ]

proc genDecl*(g: var CodeGenEnv, id: ProcedureId): CAst =
  ## Generates the declaration for the given procedure.

proc genDecl*(g: var CodeGenEnv, id: GlobalId): CAst =
  ## Generates the declaration for the given global binding.

proc genDecl*(g: var CodeGenEnv, id: ConstId): CAst =
  ## Generates the declaration for the given constant.

proc genProc*(g: var CodeGenEnv, id: ProcedureId, body: sink MirBody): CAst =
  ## Generates the full C definition for the given procedure, with body `body`.

proc genGlobal*(g: var CodeGenEnv, id: GlobalId): CAst =
  ## Generates the definitions for the given global.

proc genConst*(g: var CodeGenEnv, id: ConstId, body: MirTree): CAst =
  ## Generates the definition for the given constant, with body `body`.
