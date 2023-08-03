# test we get some suggestion at the end of the file







type


template foo() =

proc main =

#[!]#
discard """
$nimsuggest --tester $file
>chk $1
chk;;skUnknown;;;;Error;;$file;;12;;0;;"identifier expected, but found \'keyword template\'";;0
chk;;skUnknown;;;;Error;;$file;;14;;0;;"nestable statement requires indentation";;0
chk;;skUnknown;;;;Error;;$file;;12;;0;;"implementation of \'foo\' expected";;0
chk;;skUnknown;;;;Error;;$file;;17;;0;;"invalid indentation";;0
chk;;skUnknown;;;;Hint;;$file;;20;;95;;"Hint: line too long [LineTooLong]";;0
chk;;skUnknown;;;;Hint;;$file;;21;;83;;"Hint: line too long [LineTooLong]";;0
chk;;skUnknown;;;;Hint;;$file;;28;;97;;"Hint: line too long [LineTooLong]";;0
chk;;skUnknown;;;;Hint;;$file;;29;;98;;"Hint: line too long [LineTooLong]";;0
chk;;skUnknown;;;;Hint;;$file;;12;;9;;"\'foo\' is declared but not used [XDeclaredButNotUsed]";;0
chk;;skUnknown;;;;Hint;;$file;;14;;5;;"\'main\' is declared but not used [XDeclaredButNotUsed]";;0
"""
