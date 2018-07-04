call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'dracula/vim'
Plug 'neomake/neomake' 
Plug 'jiangmiao/auto-pairs'

"Plug 'Shougo/deoplete.nvim'
Plug 'Valloric/YouCompleteMe'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'

" Haskell vim stuff
Plug 'neovimhaskell/haskell-vim'
Plug 'parsonsmatt/intero-neovim'

" C++ sutff
Plug 'rhysd/vim-clang-format'

Plug 'scrooloose/nerdcommenter'

call plug#end()

" I will nevre get FZF to work with neovim
set rtp+=~/.fzf
set rtp+=/usr/local/opt/fzf

set expandtab

" YCMD
let g:ycm_semantic_triggers =  {
  \   'c' : ['->', '.', '_'],
  \   'cpp,cuda,objcpp' : ['->', '.', '::'],
  \   'haskell' : ['.']
  \ }
"let g:ycm_semantic_triggers = {'haskell' : ['re!.']}
"let g:ycm_semantic_triggers.haskell = ['re![^ ]+']
" removes the annoying autocompeltiong menu
let g:ycm_autoclose_preview_window_after_completion = 1


set incsearch "show charcters as typing"
set hlsearch
set ignorecase
set number

set showmatch " this highlights the other bracket
set conceallevel=0
set encoding=utf-8
"set formatoptions-=cro " removes the shityy auto commenter
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

syntax on
filetype plugin on

autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.tex setlocal spell
"set spell spelllang=en_us

" copy and paste
vmap <C-S-c> "+yi
vmap <C-x> "+c
vmap <C-S-v> c<ESC>"+p
imap <C-S-v> <ESC>"+pa
set clipboard=unnamedplus
filetype plugin indent on    

" Setting the color scheme
set background=dark
colorscheme dracula
hi! Normal ctermbg=NONE guibg=NONE
set cursorline

map <SPACE> <leader> 
map <SPACE> \
imap jk <esc>
imap kj <esc>
map <S-k> <Nop>
map <C-g> <esc>
imap <C-g> <esc>
nnoremap j gj
nnoremap k gk
nnoremap Y y$
"nnoremap V v$

" Getting to the real boy escape mode
tnoremap <C-g> <C-\><C-n>

" changing windwos
tnoremap <A-h> <C-\><C-N><C-w>h
tnoremap <A-j> <C-\><C-N><C-w>j
tnoremap <A-k> <C-\><C-N><C-w>k
tnoremap <A-l> <C-\><C-N><C-w>l
inoremap <A-h> <C-\><C-N><C-w>h
inoremap <A-j> <C-\><C-N><C-w>j
inoremap <A-k> <C-\><C-N><C-w>k
inoremap <A-l> <C-\><C-N><C-w>l
nnoremap <A-h> <C-w>h
nnoremap <A-j> <C-w>j
nnoremap <A-k> <C-w>k
nnoremap <A-l> <C-w>l


" LEADER REMAPS
nnoremap <Leader>ev :vsplit $MYVIMRC<cr>
nnoremap <Leader>sv :source ~/.config/nvim/init.vim <cr>
nnoremap <Leader>noh :noh <cr>

nnoremap <Leader>hex :Hex <cr> :Files <cr>
nnoremap <Leader>vex :Vex <cr> :Files <cr>
nnoremap <Leader>ex :Ex <cr> :Files <cr>

augroup interoMaps
  au!
  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.

  " Background process and window management
  au FileType haskell nnoremap <silent> <leader>is :InteroStart<CR>
  au FileType haskell nnoremap <silent> <leader>ir :InteroRestart<CR>
  au FileType haskell nnoremap <silent> <leader>ik :InteroKill<CR>

  " Open intero/GHCi split horizontally
  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  " Open intero/GHCi split vertically
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>

  " Reloading (pick one)
  " Automatically reload on save
  au BufWritePost *.hs InteroReload
  " Manually save and reload
"  au FileType haskell nnoremap <silent> <leader>wr :w \| :InteroReload<CR>
  au FileType haskell nnoremap <silent> <leader>r :InteroReload<CR>

  " Load individual modules
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  " Type-related information
  " Heads up! These next two differ from the rest.
  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
  au FileType haskell map <silent> <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  " Navigation
  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>

  " Managing targets
  " Prompts you to enter targets (no silent):
  au FileType haskell nnoremap <leader>ist :InteroSetTargets<SPACE>
augroup END

" Intero starts automatically. Set this if you'd like to prevent that.
"let g:intero_start_immediately = 1

" Enable type information on hover (when holding cursor at point for ~1 second).
let g:intero_type_on_hover = 1
" OPTIONAL: Make the update time shorter, so the type info will trigger faster.
"set updatetime=250

" NECO GHC
let g:necoghc_use_stack = 1
let g:necoghc_enable_detailed_browse = 1

" SYNTAX HIGHLITING
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords

" neomake things
" normal mode (after 1s; no delay when writing).
"call neomake#configure#automake('w')

" NERD COMMENTER SETTINGS
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Set a language to use its alternate delimiters by default
let g:NERDAltDelims_java = 1
" Add your own custom formats or override the defaults
let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1
" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1


" Clang format stuff:
let g:clang_format#auto_format = 1
let g:clang_format#auto_format_on_insert_leave = 0

let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}

nnoremap <Leader>cd i// -----------------------------------------<CR>//    Sectional Dividers Like This<CR>// -----------------------------------------<CR><esc>kkwv$h
