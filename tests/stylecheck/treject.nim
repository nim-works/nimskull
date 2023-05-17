discard """
  cmd: "nim check $options --stylechecks --stylecheck:error $file"
  action: reject
"""

# RE: stylechecks and stylecheck
# the fact that there are two options that are sorta the same but different
# is really dumb, these need to be consolidated

block types_must_start_with_a_capital_letter:
  type
    user = object      #[tt.Error
    ^ 'user' should be: 'User' [Name]]#
      id: int
  
  let u = user()
  discard u

when false: # knownIssue: type symbols don't get the style check option set so
            #             `linter` never checks them.
  block multi_letter_non_types_or_consts_must_be_lower_case:
    proc Lame() = discard  #[disabled tt.Error
        ^ 'Lame' should be: 'lame']#
    Lame()

block match_casing:
  type
    User = object
      id: int

  template hello =
    var iD = "string"
    var user: User
    echo user.iD       #[tt.Error
              ^ 'iD' should be: 'id' ]#
    echo iD

  hello()