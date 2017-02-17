"General Settings
set nocompatible              " be iMproved, required
set shell=/bin/bash
filetype off                  " required
syntax on
let mapleader=","
set bg=dark
set laststatus=2
set ruler
set nonumber
set showcmd
set background=dark
set anti enc=utf-8
set linespace=2
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
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣
set term=screen-256color


" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-surround'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'airblade/vim-gitgutter'
Plugin 'majutsushi/tagbar'
Plugin 'mileszs/ack.vim'
Plugin 'craigemery/vim-autotag'

" All of your Plugins must be added before the following line

call vundle#end()            " required
filetype plugin indent on    " required

" Put your non-Plugin stuff after this line

" NERDTree
" Toggle nerdtree with F10
map <F10> :NERDTreeToggle<CR>

" Current file in nerdtree
map <F9> :NERDTreeFind<CR>

" Buffer next and previous
nmap <leader>, :bp<CR>
nmap <leader>. :bn<CR>


" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme="light"

" Statusline Git
set statusline=%{fugitive#statusline()}

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/vendor/*,*/\.git/*
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
map <leader>p :CtrlP<cr>
nnoremap <leader>ç :CtrlPTag<cr>

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Ignore line width for syntax checking
let g:syntastic_python_checker_args='--ignore=E501'
