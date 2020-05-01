" ---------------------------------------------------------------------------
"    Language client servers
" ---------------------------------------------------------------------------
"let g:deoplete#enable_at_startup = 1 " autocompleetion

let g:LanguageClient_serverCommands = { 'haskell': ['hie-wrapper', '--lsp'] }

let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']

"\ 'haskell': ['hie-wrapper', '-r', '$YOURROOTHERE'],

" ---------------------------------------------------------------------------
"    bindings
" ---------------------------------------------------------------------------

" sometimes in haskell we just want the single quote
inoremap ' '


