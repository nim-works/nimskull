import std/[os, pathutils]
const
  explicitSourcePath {.strdefine.} = getCurrentCompilerExe().parentDir.parentDir
  (dir, name, ext) = splitFile(currentSourcePath)

type
  TOutlineKind = enum
    kind1,
    kind2
  TOutline = object
    kind: TOutlineKind


# TODO: tester handle backtick proc `$`(k: TOutlineKind): string = discard 

iterator xrange(fromm, to: int, step = 1): int = discard

template tmpa() = discard
macro tmpb() = discard
converter tmpc() = discard

discard """
$nimsuggest --tester $file
>outline $path/toutline.nim
outline;;skConst;;toutline.explicitSourcePath;;;;$file;;3;;2;;"";;100
outline;;skConst;;toutline.(dir,name,ext,);;;;$file;;4;;2;;"";;100
outline;;skType;;toutline.TOutlineKind;;;;$file;;7;;2;;"";;100
outline;;skType;;toutline.TOutline;;;;$file;;10;;2;;"";;100

outline;;skIterator;;toutline.xrange;;;;$file;;16;;9;;"";;100
outline;;skTemplate;;toutline.tmpa;;;;$file;;18;;9;;"";;100
outline;;skMacro;;toutline.tmpb;;;;$file;;19;;6;;"";;100
outline;;skConverter;;toutline.tmpc;;;;$file;;20;;10;;"";;100
"""