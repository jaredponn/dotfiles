filetype plugin indent on    

call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/fzf.vim' "fzf (fuzzy searching)
Plug 'dracula/vim'  " color scheme
Plug 'neomake/neomake'  "linting
"Plug 'cohama/lexima.vim' " brackets

" autocompletion
Plug 'Valloric/YouCompleteMe', { 'for': 'c,cpp',
                        \ } "mainly for 
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins'
                \ }
Plug 'autozimu/LanguageClient-neovim', {
                        \ 'branch': 'next',
                        \ 'do': 'bash install.sh',
                        \ }

" C++ sutff
Plug 'rhysd/vim-clang-format'

" commentor
Plug 'scrooloose/nerdcommenter'

call plug#end()

" I will nevre get FZF to work with neovim
set rtp+=~/.fzf
set rtp+=/usr/local/opt/fzf

set expandtab

set incsearch "show charcters as typing"
set hlsearch
set ignorecase
set number

set showmatch " this highlights the other bracket
set conceallevel=0
set encoding=utf-8

syntax on

"set spell spelllang=en_us

" copy and paste
vmap <C-S-c> "+yi
vmap <C-x> "+c
vmap <C-S-v> c<ESC>"+p
imap <C-S-v> <ESC>"+pa
set clipboard=unnamedplus

" Setting the color scheme
set background=dark
colorscheme dracula
hi! Normal ctermbg=NONE guibg=NONE
set cursorline

" ---------------------------------------------------------------------------
"    Vim remaps
" ---------------------------------------------------------------------------
map <SPACE> <leader> 
map <SPACE> \
"imap jk <esc>
"imap kj <esc>
map <S-k> <Nop>
map <C-g> <esc>
imap <C-g> <esc>
nnoremap j gj
nnoremap k gk
nnoremap Y y$
"nnoremap V v$

" Getting to the real boy escape mode in terina mode
tnoremap <C-g> <C-\><C-n>

" changing windwos
"tnoremap <A-h> <C-\><C-N><C-w>h
"tnoremap <A-j> <C-\><C-N><C-w>j
"tnoremap <A-k> <C-\><C-N><C-w>k
"tnoremap <A-l> <C-\><C-N><C-w>l
"inoremap <A-h> <C-\><C-N><C-w>h
"inoremap <A-j> <C-\><C-N><C-w>j
"inoremap <A-k> <C-\><C-N><C-w>k
"inoremap <A-l> <C-\><C-N><C-w>l

"nnoremap <A-h> <C-w>h " moving to windows 
"nnoremap <A-j> <C-w>j " moving to windows
"nnoremap <A-k> <C-w>k " moving to windows
"nnoremap <A-l> <C-w>l " moving to windows

" moving to windows
nnoremap <Leader>h <C-w>h
nnoremap <Leader>j <C-w>j
nnoremap <Leader>l <C-w>l
nnoremap <Leader>k <C-w>k

" shifting windows
nnoremap <Leader>H <C-w>H
nnoremap <Leader>J <C-w>J
nnoremap <Leader>L <C-w>L
nnoremap <Leader>K <C-w>K

" shifting windows in a terminal
tnoremap <Leader>h <C-\><C-N><C-w>h
tnoremap <Leader>j <C-\><C-N><C-w>j
tnoremap <Leader>k <C-\><C-N><C-w>k
tnoremap <Leader>l <C-\><C-N><C-w>l

" editing vimrcs
nnoremap <Leader>ev :vsplit $MYVIMRC<cr>
nnoremap <Leader>evc :vsplit ~/.config/nvim/init_c.vim<cr>
nnoremap <Leader>evi :vsplit ~/.config/nvim/init_vim.vim<cr>
nnoremap <Leader>sv :source ~/.config/nvim/init.vim <cr>

" window managerment rempas
nnoremap <Leader>hex :Hex <cr>/
nnoremap <Leader>vex :Vex <cr>/
nnoremap <Leader>ex :Ex <cr>/
nnoremap - :Ex <cr>/
nnoremap <Leader>he :Hex <cr>/
nnoremap <Leader>ve :Vex <cr>/

" other
nnoremap <Leader>noh :noh <cr> 
nnoremap n nzz

" screeen moement remaps
nnoremap <C-u> <C-u>M
nnoremap <C-d> <C-d>M
nnoremap <C-b> <C-b>M
nnoremap <C-f> <C-f>M

" ---------------------------------------------------------------------------
"    oher bracket completion
" ---------------------------------------------------------------------------
inoremap ( ()<esc>ba
inoremap { {}<esc>ba
inoremap {<cr> {<cr>}<esc>kA<cr><esc>cc
inoremap [ []<esc>ba
"inoremap ' ''<esc>ba
inoremap " ""<esc>ba

" ---------------------------------------------------------------------------
"    nerd commentor
" ---------------------------------------------------------------------------
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

" ---------------------------------------------------------------------------
"    file type specific things
" ---------------------------------------------------------------------------
" idk what this is here for but i've always had it so ....
autocmd BufRead,BufNewFile *.md setlocal spell
autocmd BufRead,BufNewFile *.tex setlocal spell
autocmd FileType markdown,tex,textile source ~/.vim/lang_settings/text.vim
"set formatoptions-=cro " removes the shityy auto commenter
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" NOTE Using BufEnter might be a better idea because there are some cases where BufRead will not tigger

" C
autocmd FileType c source ~/.config/nvim/init_c.vim
autocmd BufRead,BufEnter *.c,*.h source ~/.config/nvim/init_c.vim

" vimrc
autocmd BufRead,BufEnter *.vim source ~/.config/nvim/init_vim.vim

" haskell
autocmd BufRead,BufEnter *.hs source ~/.config/nvim/init_haskell.vim
autocmd BufRead,BufEnter *.hs call deoplete#enable()
