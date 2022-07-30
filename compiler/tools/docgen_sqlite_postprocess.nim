import
  compiler/tools/[
    docgen3,
    docgen_types,
    docgen_sqlite,
    docgen_text
  ],
  compiler/front/[
    options,
    cli_reporter,
    msgs
  ],
  compiler/utils/[
    pathutils
  ],
  std/[
    os
  ]

let outSql = AbsoluteFile(commandLineParams()[0])

var newConf = newConfigRef(cli_reporter.reportHook)

newConf.writeHook =
  proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
    msgs.msgWrite(conf, msg, flags)

newConf.writelnHook =
  proc(conf: ConfigRef, msg: string, flags: MsgFlags) =
    conf.writeHook(conf, msg & "\n", flags)

var newDb = DocDb()
readSqlite(newConf, newDb, outSql)
echo "read sqlite from ", outSql.string

let docs = AbsoluteDir(currentSourcePath().parentDir().parentDir().parentDir() / "doc")
echo docs
unparseExtraDocs(newDb, newConf, docs)
unparseComments(newDb, newConf)

let outSql2 = outSql.changeFileExt("sqlite2")
newConf.writeSqlite(newDb, outSql2)
echo "wrote sqlite database back ", outSql2.string
