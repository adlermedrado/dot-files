"General Settings
set nocompatible              " be iMproved, required
filetype off                  " required
syntax on
let mapleader=","
set bg=dark
set laststatus=2
set ruler
set nonumber
set showcmd
set background=dark
set linespace=2
set encoding=utf-8                " Use UTF-8 everywhere.
set lines=80 columns=200          " Window dimensions.
set expandtab
set shiftwidth=4
set softtabstop=4
set textwidth=0 nowrap
set visualbell              " Set visual bell
set foldmethod=marker
set foldenable
set foldlevel=5           
set foldnestmax=1
set clipboard=unnamed
set colorcolumn=180
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣
set guifont=Inconsolata\ 16
set backspace=start,eol,indent

" Set autopep8 for python files
au FileType python setlocal formatprg=autopep8\ -

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
Plugin 'craigemery/vim-autotag'
Plugin 'shougo/neocomplete.vim'
Plugin 'tmhedberg/simpylfold'
Plugin 'mileszs/ack.vim'
" All of your Plugins must be added before the following line

call vundle#end()            " required
filetype plugin indent on    " required

" Put your non-Plugin stuff after this line

" NERDTree
" Toggle nerdtree with F7
map <F7> :NERDTreeToggle<CR>

" Current file in nerdtree
map <F6> :NERDTreeFind<CR>

" Buffer next and previous
nmap <leader>, :bp<CR>
nmap <leader>. :bn<CR>


" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_theme="luna"

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
let g:syntastic_python_checkers=['python', 'flake8']
let g:syntastic_python_checker_args='--ignore=E501'


" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#sources#syntax#min_keyword_length = 1

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"

" AutoComplPop like behavior.
let g:neocomplete#enable_auto_select = 1
" Enable omni completion.
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
