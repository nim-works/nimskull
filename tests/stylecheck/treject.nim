discard """
  cmd: "nim check $options --stylecheck:error $file"
  action: reject
"""

block types_must_start_with_a_capital_letter:
  type
    user = object      #[tt.Error
    ^ 'user' should be: 'User' [Name]]#
      id: int
  
  let u = user()
  discard u

block multi_letter_non_types_or_consts_must_be_lower_case:
  proc Lame() = discard  #[tt.Error
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