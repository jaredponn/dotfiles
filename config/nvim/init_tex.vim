
" ---------------------------------------------------------------------------
"    bindings
" ---------------------------------------------------------------------------

" spel checking
setlocal spell

" autocompile when saving
autocmd BufWritePost * :!pdflatex % 

nnoremap <Leader>pdf :!zathura --fork %<.pdf<cr>

" remove; and s the fancy horizontsal line
set nocursorline


