" ---------------------------------------------------------------------------
"    Language client servers
" ---------------------------------------------------------------------------
"let g:deoplete#enable_at_startup = 1 " autocompleetion

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper'],
    \ }

let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']

"\ 'haskell': ['hie-wrapper', '-r', '$YOURROOTHERE'],

"let g:LanguageClient_serverCommands = { \ 'haskell': ['hie-wrapper', '-r', '~/Programs/cpsc'], \ }

" ---------------------------------------------------------------------------
"    bindings
" ---------------------------------------------------------------------------

" sometimes in haskell we just want the single quote
inoremap ' '


