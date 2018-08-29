" ---------------------------------------------------------------------------
"    Language client servers
" ---------------------------------------------------------------------------
"let g:deoplete#enable_at_startup = 1 " autocompleetion

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper'],
    \ }

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper', '-r', '~/jaredponn.github.io/'],
    \ }
"\ 'haskell': ['hie-wrapper', '-r', '$YOURROOTHERE'],

" ---------------------------------------------------------------------------
"    bindings
" ---------------------------------------------------------------------------
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <Leader>g :call LanguageClient_contextMenu()<CR>

" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>

" type
nnoremap <Leader>t :call LanguageClient#textDocument_definition() <CR>

nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>



" making the autocmpletion a bit more pleaseant
autocmd CompleteDone * silent! pclose!
autocmd InsertLeave * silent! pclose!
" tab ieration
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <expr><s-tab> pumvisible() ? "\<c-p>" : "\<tab>"

" sometimes in haskell we just want the single quote
inoremap ' '

" fixes the weird multiple pressing enter for an enter key to work
inoremap <expr> <CR> pumvisible() ? '<c-e><cr>' : '<cr>'

