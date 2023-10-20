import
  compiler / ast / [ lineinfos, ast, astalgo ]
  compiler / front / options,
  compiler / modules / modulegraphs,
  compiler / sem / passes

import dust/spec
import dust/mutate
import dust/hashing

var remains*: Remains

proc opener(graph: ModuleGraph; module: PSym): PPassContext {.nosinks.} =
  ## the opener learns when we're compiling the test file
  result = DustContext(mainIndex: graph.config.projectMainIdx)

when false:
  proc closer(graph: ModuleGraph; context: PPassContext, n: PNode): PNode =
    ## the closer don't do shit
    discard

proc rewriter(context: PPassContext, n: PNode): PNode {.nosinks.} =
  template c: DustContext = DustContext(context)
  # if this isn't the main project,
  if n.info.fileIndex != c.mainIndex:
    # yield the original node
    result = n
  else:
    if c.ignore:
      # we already yielded the program, so just return nil
      result = nil
    else:
      # yield the next available permutation (once)
      result = pop(remains)
      c.ignore = true

const
  dustPass* = makePass(opener, rewriter, nil)