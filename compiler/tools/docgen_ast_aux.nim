import
  ast/[
    ast,
    renderer
  ],
  utils/[
    astrepr
  ]

proc failNode*(node: PNode, reason: string = "") {.
    # deprecated: "Temporary hack to speed up development",
    noreturn
  .} =

  var conf = implicitTReprConf
  conf.maxPath = 3
  echo "failed node >>>> ", reason
  echo treeRepr(nil, node, conf)
  echo "failed node <<<<"
  assert false

func getSName*(p: PNode): string =
  ## Get string value from `PNode`
  case p.kind:
    of nkIdent:         result = p.ident.s
    of nkSym:           result = p.sym.name.s
    of nkStrKinds:      result = p.strVal
    of nkOpenSymChoice: result = p[0].sym.name.s
    of nkAccQuoted:
      for sub in p:
        result.add getSName(sub)

    else:
      assert false, "Unexpected kind for 'getSName' - " & $p.kind

proc getSName*(p: PSym): string = p.name.s


