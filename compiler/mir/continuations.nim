## Implements the lowering of `fork`/`land` pairs. For every static `fork`, the
## continuation of the fork (i.e., the code following the land) is reified
## into a standalone procedure -- the context (i.e., all active locals) are
## saved into a separate context object, which must be passed to the reified
## procedure.
##
## A rough summary of the reification process is that the procedure body is
## duplicated and all basic-blocks not reachable from the resumption point
## are removed.
##
## Forks in loops make this a bit trickier in practice. Consider:
##
##   loop:
##     def _1 = ...
##     if cond:
##       goto L2
##     fork a, b, L1
##     ...
##     land L1:
##     ...
##   L2:
##   ...
##
## Here, the loop must also be part of the reified procedure, but a naive
## removal of all unused basic blocks would leave it at the start, which is
## wrong.
##
## The solution is to split the continuation into multiple subroutines, one
## for each such problematic join point.
##
## There are two possible strategies to implement subroutines:
## 1. via separate procedure that tail call each other, passing along extra
##    locals crossing the border as parameter
## 2. or, use a case statement dispatcher in a loop, with each target
##    corresponding to a subroutine. Invoking a subroutine means changing the
##    selector value and jumping back to the loop start -- local variables
##    living across subroutine boundaries are lifted to the top-level scope
##
## Number 2 is chosen because it's slightly simpler to implement and doesn't
## put pressure on the available tailcall argument sizes (and thus the size
## of the context object).
##
## A reified continuation is created for each fork/land pair.
