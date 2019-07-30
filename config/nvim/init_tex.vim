
" ---------------------------------------------------------------------------
"    bindings
" ---------------------------------------------------------------------------

" spel checking
setlocal spell

" autocompile when saving
autocmd BufWritePost * :!make 

"nnoremap <Leader>pdf :!zathura --fork %<.pdf<cr>
"nnoremap <Leader>pdf :!zathura --fork %<.pdf<cr>

" remove; and s the fancy horizontsal line
set nocursorline

"set noai nocin nosi inde=

" Removing the strange auto indent when typing {
set inde=
