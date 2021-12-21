type
  NilTransition* = enum
    ## transition kind: what was the reason for changing the nilability of
    ##   an expression useful for error messages and showing why an
    ##   expression is being detected as nil / maybe nil
    TArg
    TAssign
    TType
    TNil
    TVarArg
    TResult
    TSafe
    TPotentialAlias
    TDependant

  Nilability* = enum
    ## Nilability : if a value is nilable. we have maybe nil and nil, so we
    ## can differentiate between cases where we know for sure a value is
    ## nil and not otherwise we can have Safe, MaybeNil Parent: is because
    ## we just use a sequence with the same length instead of a table, and
    ## we need to check if something was initialized at all: if Parent is
    ## set, then you need to check the parent nilability if the parent is
    ## nil, then for now we return MaybeNil unreachable is the result of
    ## add(Safe, Nil) and others it is a result of no states left, so it's
    ## usually e.g. in unreachable else branches?
    Parent
    Safe
    MaybeNil
    Nil
    Unreachable
