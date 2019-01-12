call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'dracula/vim'  " color scheme

Plug 'Valloric/YouCompleteMe'

Plug 'rhysd/vim-clang-format', { 'for': 'java,c,cpp',} "mainly for 

" Initialize plugin system
call plug#end()

"run before using eclim: /home/jared/builds/eclipse/eclimd

syntax on
set clipboard=unnamedplus
hi! Normal ctermbg=NONE guibg=NONE
colorscheme dracula
set number
set ignorecase
set smartcase
set number
set showmatch " this highlights the other bracket
set conceallevel=0
set encoding=utf-8
set cursorline
syntax on


" editing vimrcs
nnoremap <Leader>ev :tabnew $MYVIMRC<cr>

let g:clang_format#auto_format = 1
let g:clang_format#auto_format_on_insert_leave = 0

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

nnoremap <Leader>noh :noh <cr> 


inoremap ( ()<esc>ha
inoremap { {}<esc>ha
inoremap {<cr> {<cr>}<esc>kA<cr><esc>cc
inoremap [ []<esc>ha
"inoremap ' ''<esc>ba
inoremap " ""<esc>ha

inoremap () ()<esc>ha
inoremap {} {}<esc>ha
inoremap {<cr> {<cr>}<esc>kA<cr><esc>cc
inoremap [] []<esc>ha
"inoremap ' ''<esc>ba
inoremap "" ""<esc>ha


" selects the longest common text of all matches 
set completeopt=longest,menuone

" making the autocmpletion a bit more pleaseant
autocmd CompleteDone * silent! pclose!
autocmd InsertLeave * silent! pclose!

let g:EclimCompletionMethod = 'omnifunc'

nnoremap - :Ex <cr>
nnoremap <Leader>he :Hex <cr>
nnoremap <Leader>ve :Vex <cr>

set noswapfile
