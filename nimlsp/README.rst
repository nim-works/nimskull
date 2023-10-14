==========
Nimskull Language Server Protocol
==========

This is a `Language Server Protocol
<https://microsoft.github.io/language-server-protocol/>`_ implementation in
Nimskull, for Nimskull.
It is based on nimsuggest, which means that every editor that
supports LSP will now have the same quality of suggestions that has previously
only been available in supported editors.

Supported Protocol features
=======

======  ================================
Status  LSP Command
======  ================================
☑ DONE  textDocument/didChange
☑ DONE  textDocument/didClose
☑ DONE  textDocument/didOpen
☑ DONE  textDocument/didSave
☐ TODO  textDocument/codeAction
☑ DONE  textDocument/completion
☑ DONE  textDocument/definition
☐ TODO  textDocument/documentHighlight
☑ DONE  textDocument/documentSymbol
☐ TODO  textDocument/executeCommand
☐ TODO  textDocument/format
☑ DONE  textDocument/hover
☑ DONE  textDocument/rename
☑ DONE  textDocument/references
☑ DONE  textDocument/signatureHelp
☑ DONE  textDocument/publishDiagnostics
☐ TODO  workspace/symbol
☐ TODO  `$/progress<https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress>`
☐ TODO  `textDocument/codeAction<https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction>`
======  ================================
