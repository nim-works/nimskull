##[
Utilities related to import and symbol resolution.

Experimental API, subject to change.
]##

#[
Possible future APIs:
* module symbols (https://github.com/nim-lang/Nim/pull/9560)
* whichModule (subsumes canImport / moduleExists) (https://github.com/timotheecour/Nim/issues/376)
* getCurrentPkgDir (https://github.com/nim-lang/Nim/pull/10530)
* import from a computed string + related APIs (https://github.com/nim-lang/Nim/pull/10527)
]#

import system/magics
export magics.privateAccess
