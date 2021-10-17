filetype plugin indent on    

set incsearch "show charcters as typing
set hlsearch

" set ignorecase
" set smartcase

set number
syntax on

set showmatch " this highlights the other bracket
set conceallevel=0
set encoding=utf-8

" tab space setting
set tabstop=4 shiftwidth=4 expandtab

"set spell spelllang=en_us

" copy and paste
"set clipboard=unnamedplus

" Setting the color scheme

" editing vimrcs
nnoremap <Leader>ev :tabnew $MYVIMRC<cr>
nnoremap <Leader>sv :source ~/.config/nvim/init.vim <cr>
