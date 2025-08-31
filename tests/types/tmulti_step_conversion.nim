discard """
  description: '''
    Ensure immediately converting a to-distinct-base conversion to the
    distinct type works.
  '''
"""

type
  Base     = ref object of RootObj
  Derived  = ref object of Base
  Distinct = distinct Base

discard Distinct(Base(Derived()))
