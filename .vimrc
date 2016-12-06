"General Settings
set nocompatible              " be iMproved, required
filetype off                  " required
syntax on
let mapleader=","
set laststatus=2
set ruler
set nonumber
set showcmd
set background=dark
set anti enc=utf-8
set linespace=2
set guifont=Source\ Code\ Pro\ Light:h16 " Font family and font size.
set antialias                     " MacVim: smooth fonts.
set encoding=utf-8                " Use UTF-8 everywhere.
set guioptions-=T                 " Hide toolbar.
set lines=80 columns=200          " Window dimensions.
set expandtab
set shiftwidth=4
set softtabstop=4
set textwidth=0 nowrap
set visualbell              " Set visual bell
set foldmethod=indent       " Folding method: indent
set foldlevel=99            " Initial Fold Level
set clipboard=unnamed
set colorcolumn=180

" Moving .swp files away
set backupdir=~/.vim
set directory=~/.vim

autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufRead *.py setlocal colorcolumn=0

let g:markdown_fenced_languages = ['html', 'python', 'bash=sh','php']
let g:markdown_syntax_conceal = 0
set backspace=indent,eol,start

filetype plugin indent on  " required!

" requires xmllint
au FileType xml setlocal equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null


" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'majutsushi/tagbar'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-surround'
Plugin 'altercation/vim-colors-solarized'
Plugin 'beanworks/vim-phpfmt'
Plugin 'valloric/youcompleteme'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'
Plugin 'crusoexia/vim-dracula'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme="dark"

" Statusline Git
" set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set statusline=%{fugitive#statusline()}

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/vendor/*,*/\.git/*
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
map <C-C> :CtrlPBuffer<CR>

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Syntastic
let g:syntastic_php_checkers = ['php','phpcs']
let g:syntastic_php_phpcs_args = "--standard=PSR2 -n --report=csv"
let g:syntastic_php_phpcs_args='--standard=PSR2 -n'
let g:syntastic_python_checkers = ['flake8']
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" NERDTree
nmap <leader>ne :NERDTree<cr>
nmap <leader>nec :NERDTreeClose<cr>
map <silent> <C-n> :NERDTreeFocus<CR>

" Buffer next and previous
nmap <leader>b :bn<CR>
nmap <leader>n :bp<CR>

" vim-phpfmt
let g:phpfmt_standard = 'PSR2'
let g:phpfmt_command = '/usr/local/bin/phpcbf'

" Redraw screen on save
au BufWritePost * :redraw!
colo dracula
