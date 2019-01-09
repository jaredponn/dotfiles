" I will nevre get FZF to work with neovim
set rtp+=~/.fzf
set rtp+=/usr/local/opt/fzf

call plug#begin('~/.local/share/nvim/plugged')
Plug 'junegunn/fzf.vim' "fzf (fuzzy searching)
Plug 'dracula/vim'  " color scheme
Plug 'neomake/neomake'  "linting

" autocompletion
" Plug 'Valloric/YouCompleteMe', { 'for': 'c,cpp,py,python',} "mainly for
" Plug 'Valloric/YouCompleteMe', { 'for': 'c,cpp',} "mainly for

Plug 'vim-scripts/a.vim', {'for' : 'c,cpp'} "switchign between header and c
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'autozimu/LanguageClient-neovim', {
                        \ 'branch': 'next',
                        \ 'do': 'bash install.sh' }

" C++ sutff
Plug 'rhysd/vim-clang-format', { 'for': 'c,cpp',} "mainly for 
Plug 'ludovicchabant/vim-gutentags', { 'for': 'c,cpp',} "mainly for 

" commentor
Plug 'tpope/vim-commentary'

call plug#end()


filetype plugin indent on    

set expandtab
set incsearch "show charcters as typing"
set hlsearch

set ignorecase
set smartcase

set number

set showmatch " this highlights the other bracket
set conceallevel=0
set encoding=utf-8

syntax on

"set spell spelllang=en_us

" copy and paste
set clipboard=unnamedplus

" Setting the color scheme
set background=dark
colorscheme dracula
hi! Normal ctermbg=NONE guibg=NONE
set cursorline
 
" disabling the mouse 
set mouse=

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
nnoremap <Leader>ev :tabnew $MYVIMRC<cr>
nnoremap <Leader>evc :vsplit ~/.config/nvim/init_c.vim<cr>
nnoremap <Leader>evi :vsplit ~/.config/nvim/init_vim.vim<cr>
nnoremap <Leader>sv :source ~/.config/nvim/init.vim <cr>

" window managerment rempas
""nnoremap <Leader>hex :Hex <cr>/
""nnoremap <Leader>vex :Vex <cr>/
""nnoremap <Leader>ex :Ex <cr>/
nnoremap - :Ex <cr>
nnoremap <Leader>he :Hex <cr>
nnoremap <Leader>ve :Vex <cr>

" other
nnoremap <Leader>noh :noh <cr> 
" nnoremap n nzz

" screeen moement remaps
" nnoremap <C-u> <C-u>M
" nnoremap <C-d> <C-d>M
" nnoremap <C-b> <C-b>M
" nnoremap <C-f> <C-f>M

" bindings for easier tab managemnet

" ---------------------------------------------------------------------------
"    oher bracket completion
" ---------------------------------------------------------------------------
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


" ---------------------------------------------------------------------------
"    modifying brackets
" ---------------------------------------------------------------------------
noremap <Leader>( ci(
noremap <Leader>) ci)
noremap <Leader>{ ci{
noremap <Leader>} ci}
noremap <Leader>[ ci[
noremap <Leader>] ci]
noremap <Leader>< ci<
noremap <Leader>> ci>
noremap <Leader>' ci'
noremap <Leader>' ci"


" ---------------------------------------------------------------------------
"    deoplete
" ---------------------------------------------------------------------------
let g:deoplete#enable_at_startup = 1 " autocompleetion
let g:deoplete#auto_complete_delay = 1 " autocompleetion



" making the autocmpletion a bit more pleaseant
autocmd CompleteDone * silent! pclose!
autocmd InsertLeave * silent! pclose!

inoremap <expr><C-n> pumvisible() ? "\<C-n>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ deoplete#mappings#manual_complete()
        function! s:check_back_space() abort "{{{
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~ '\s'
        endfunction"}}}

" tab ieration
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
inoremap <expr><s-tab> pumvisible() ? "\<c-p>" : "\<tab>"
" fixes the weird multiple pressing enter for an enter key to work
inoremap <expr> <CR> pumvisible() ? '<c-e><cr>' : '<cr>'


" ---------------------------------------------------------------------------
"    LSP
" ---------------------------------------------------------------------------
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <Leader>g :call LanguageClient_contextMenu()<CR>

" Or map each action separately
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>

" type
nnoremap <Leader>t :call LanguageClient#textDocument_definition() <CR>

nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

" ---------------------------------------------------------------------------
"    auto commands
" ---------------------------------------------------------------------------
" resize make the windows the same
autocmd VimResized * exe "normal \<c-w>="

"set formatoptions-=cro " removes the shityy auto commenter
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" NOTE Using BufEnter might be a better idea because there are some cases where BufRead will not tigger

" C
autocmd FileType c source ~/.config/nvim/init_c.vim
autocmd BufRead,BufEnter *.c,*.h,*.cpp source ~/.config/nvim/init_c.vim

" vimrc
autocmd BufRead,BufEnter *.vim source ~/.config/nvim/init_vim.vim

" haskell
autocmd BufRead,BufEnter *.hs source ~/.config/nvim/init_haskell.vim

" tex
autocmd BufRead,BufEnter *.tex source ~/.config/nvim/init_tex.vim

" md
autocmd BufRead,BufEnter *.md source ~/.config/nvim/init_md.vim
