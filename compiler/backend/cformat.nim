## Implements the formatter that turns the CIR (`cir <#cir>`_) into textual C
## code.

import
  std/[
    tables
  ],
  compiler/backend/[
    # cgendata,
    cir
  ],
  compiler/mir/[
    mirenv,
    mirtrees
  ],
  compiler/utils/[
    containers,
    idioms
  ]

# TODO: move these formatting procedures here (or somewhere else) -- they don't
#       have anything to do with options
from compiler/front/options import toCChar, makeCString

proc format(g: CodeGenEnv, ast: CombinedCAst,
            result: var string, i: var int) =
  # efficiency matters! This procedure potentially processes enormous amounts
  # of data, and should thus be as fast as possible
  # TODO: look into structuring `format` such that the C compiler can use tail
  #       calls
  # TODO: indentation is not handled
  template recurse() =
    format(g, ast, result, i)

  template foreach(n: CNode, body: untyped) =
    for i in 0..<n.len:
      body

  # TODO: parenthesis are currently used to prevent precedence issues. Look into
  #       some way to efficiently detect where the parenthesis can be omitted
  let n = ast[i]
  inc i
  case n.kind
  of cnkIdent:
    result.add g.getStr(n.ident)
  of cnkProcSym:
    result.add g.getStr(g.procs[n.prc])
  of cnkGlobalSym:
    result.add g.getStr(g.globals[n.global])
  of cnkConstSym:
    result.add g.getStr(g.consts[n.cnst])
  of cnkType, cnkWeakType:
    result.add g.getStr(g.types[n.typ].name)
  of cnkCharLit:
    # TODO: too inefficient
    result.add '\''
    toCChar(n.charVal, result)
    result.add '\''
  of cnkIntLit:
    result.addInt g.env.getInt(n.number)
  of cnkUIntLit:
    result.addInt g.env.getUInt(n.number)
  of cnkFloatLit:
    result.addFloat g.env.getFloat(n.number)
    result.add "f"
  of cnkDoubleLit:
    result.addFloat g.env.getFloat(n.number)
  of cnkVerbatim:
    result.add g.env[n.strId]
  of cnkStrLit:
    # TODO: improve; don't allocate a separate string
    result.add makeCString(g.env[n.strId])
  of cnkExpr:
    # temporarily move the cursor
    var i = ord(n.node)
    recurse()

  # expressions
  of cnkAddrOf:
    result.add "(&"
    recurse()
    result.add ")"
  of cnkDeref:
    result.add "(*"
    recurse()
    result.add ")"
  of cnkMember:
    recurse()
    result.add "."
    recurse()
  of cnkPtrMember:
    recurse()
    result.add "->"
    recurse()
  of cnkArrMember:
    recurse()
    result.add "["
    recurse()
    result.add "]"
  of cnkCast:
    result.add "("
    recurse()
    result.add ")("
    recurse()
    result.add ")"
  of cnkInfix:
    let sym = ast[i].ident
    inc i
    recurse()
    result.add " "
    result.add g.getStr(sym)
    result.add " "
    recurse()
  of cnkPrefix:
    recurse()
    result.add "("
    recurse()
    result.add ")"
  of cnkPostfix:
    let sym = ast[i].ident
    inc i
    recurse()
    result.add g.getStr(sym)
  of cnkAsgn:
    recurse()
    result.add " = "
    recurse()
  of cnkCall:
    recurse()
    result.add "("
    for j in 1..<n.len:
      if j > 1:
        result.add ", "
      recurse()
    result.add ")"
  of cnkTernary:
    result.add "("
    recurse()
    result.add " ? "
    recurse()
    result.add " : "
    recurse()
    result.add ")"
  of cnkBraced:
    result.add "{"
    for j in 0..<n.len:
      if j > 0:
        result.add ", "
      recurse()
    result.add "}"

  # statements:
  of cnkStmt:
    recurse()
    result.add ";\n"
  of cnkGoto:
    result.add "goto "
    recurse()
    result.add ";\n"
  of cnkLabel:
    recurse()
    result.add ":;\n"
  of cnkBlock:
    result.add "{\n"
    foreach(n):
      recurse()
    result.add "}\n"
  of cnkWhile:
    result.add "while ("
    recurse()
    result.add ") "
    recurse()
  of cnkReturn:
    if n.len == 0:
      result.add "return;\n"
    else:
      result.add "return "
      recurse()
      result.add ";\n"
  of cnkIf:
    result.add "if ("
    recurse()
    result.add ") "
    recurse()
  of cnkSwitch:
    result.add "switch ("
    recurse()
    result.add ") {\n"
    for _ in 1..<n.len:
      recurse()
    result.add "}\n"
  of cnkCase:
    result.add "case "
    recurse()
    result.add ": "
  of cnkDefault:
    result.add "default: "

  # declaration grammar:
  of cnkDeclaration:
    recurse() # specifiers/qualifiers
    recurse() # declarator
    if n.len == 3:
      # optional initializer
      result.add " = "
      recurse()
    result.add ";\n"
  of cnkParamDecl:
    recurse() # specifiers/qualifiers
    recurse() # name
  of cnkDefinition:
    recurse() # specifiers/qualifiers
    recurse() # function declarator
    result.add " "
    recurse() # body
  of cnkSpecList:
    foreach(n):
      recurse()
      result.add "\n"
  of cnkFuncDecl:
    recurse() # name
    result.add "("
    for j in 1..<n.len:
      if j > 1:
        result.add ", "
      recurse()
    result.add ")"
  of cnkPtrDecl:
    result.add "*"
    recurse()
  of cnkArrayDecl:
    recurse()
    result.add "["
    if n.len == 2:
      recurse()
    result.add "]"
  of cnkDeclList:
    result.add "{\n"
    recurse()
    result.add "}\n"
  of cnkStructSpec:
    result.add "struct "
    foreach(n):
      recurse()
  of cnkUnionSpec:
    result.add "union "
    foreach(n):
      recurse()

  # directives:
  of cnkEmit:
    # just format whatever is provided as the arguments
    foreach(n):
      recurse()

proc format*(g: CodeGenEnv, ast: CombinedCAst, i: CNodeIndex,
             result: var string) =
  ## Formats `ast` starting at `i` into as textual C code, appending the
  ## result to `result`.
  var i = ord(i)
  format(g, ast, result, i)
