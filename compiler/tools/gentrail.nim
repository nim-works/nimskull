import
  ./docgen_sourcetrail,
  ./docgen_types,
  ./docgen_sqlite,
  utils/[
    pathutils
  ],
  std/[
    os
  ],
  front/[
    options,
  ]

proc main(): void =
  let path = paramStr(1)
  echo path

  var conf = ConfigRef()
  var db = DocDb()
  readSqlite(conf, db, AbsoluteFile(path))
  writeSourcetrail(conf, db, AbsoluteFile(path))
  echo "WROTE SOURCETRAIL DATABASE TO ", path

main()
