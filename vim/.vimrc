" General Settings
set nocompatible              " be iMproved, required

let mapleader = ","
let g:mapleader = ","

set backspace=start,eol,indent
set shell=/bin/bash
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable

" Brackets colors match
hi MatchParen cterm=bold ctermbg=none ctermfg=magenta

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM UX
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable number lines
set number

" Shows the last command in bottom right
set showcmd

" Highlight current line
set cursorline

" Set 7 lines to the cursor - when moving vertically using j/k
set so=0

" Turn on the Wild menu
set wildmenu

" Ignore compiled files
set wildignore+=*.o,*.obj,.git,*.rbc,*.pyc,__pycache__

"Always show current position
set ruler

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" Set line width marker
set colorcolumn=79
highlight ColorColumn ctermbg=238

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Keeps active line vertically centralized
set scrolloff=999


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM Behaviour
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype on
filetype plugin on
filetype indent on

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turn persistent undo on
"    means that you can undo even when you close a buffer/VIM
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try
    set undodir=~/.vimnimum/undodir
    set undofile
catch
endtry

" Set to auto read when a file is changed from the outside
set autoread

" Ignore case in some commands
cab W w| cab Q q| cab Wq wq| cab wQ wq| cab WQ wq

" Using system's clipboard
set clipboard=unnamed,unnamedplus

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

set ai " Auto indent
set si " Smart indent

" Word wrapping, but line breaks only when Enter is pressed
set wrap
set linebreak
set nolist

" No brackts match
let loaded_matchparen = 1 
" Toggle paste mode on/off
set pastetoggle=<F2>

" Set autopep8 for python files
au FileType python setlocal formatprg=autopep8\ -

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

Plug 'tpope/vim-fugitive'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdcommenter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'majutsushi/tagbar'
Plug 'craigemery/vim-autotag'
Plug 'mileszs/ack.vim'
Plug 'easymotion/vim-easymotion'
Plug 'morhetz/gruvbox'
Plug 'ryanoasis/vim-devicons'
Plug 'mhinz/vim-startify'
Plug 'Yggdroot/indentLine'
Plug 'mhinz/vim-signify'
Plug 'nanotech/jellybeans.vim', { 'tag': 'v1.6' }

function! BuildYCM(info)
  if a:info.status == 'installed' || a:info.force
    !./install.sh
  endif
endfunction
Plug 'valloric/youcompleteme'

call plug#end()

filetype plugin indent on    " required

" Put your non-Plugin stuff after this line

" colorscheme 
set t_Co=256   " This is may or may not needed.

set background=dark
colorscheme jellybeans

" NERDTree
" Toggle nerdtree with F7
map <F7> :NERDTreeToggle<CR>

" Current file in nerdtree
map <F6> :NERDTreeFind<CR>

" Buffer next, previous and close
map <C-J> :bprev<CR>
map <C-K> :bnext<CR>
map <C-L> :tabn<CR>
map <C-H> :tabp<CR>
map <F3> :bd<CR> 

" Disable arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
"
" Airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#syntastic#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline_powerline_fonts = 0
let g:airline_theme='papercolor'

" Statusline Git
set statusline=%{fugitive#statusline()}

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/vendor/*,*/\.git/*
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
let g:ctrlp_show_hidden = 1
map <leader>p :CtrlP<cr>
nnoremap <leader>ç :CtrlPTag<cr>

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Ignore line width for syntax checking
let g:syntastic_enable_signs=0
let g:syntastic_auto_loc_list=1
let g:syntastic_enable_highlighting=1
let g:syntastic_loc_list_height=3
let g:syntastic_python_checkers=['flake8']
let g:syntastic_python_flake8_args='--ignore=E501,E128'
let g:airline_enable_syntastic = 1

" IPDB abbreviation
ab IPDB import ipdb; ipdb.set_trace()

" PDB abbreviation
ab PDB import pdb; pdb.set_trace()

" Ack.Vim
cnoreabbrev Ack Ack!
nnoremap <Leader>a :Ack!<Space>

" Integrate ag with Ack.vim
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" Clean search
nmap <esc><esc> :noh<return>

" Easymotion
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap s <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)

" Buffer navigation
nnoremap <silent> <C-b> :silent :bp<CR>
nnoremap <silent> <C-n> :silent :bn<CR>

" NERDCommenter

" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" Enable NERDCommenterToggle to check all selected lines is commented or not 
let g:NERDToggleCheckAllLines = 1
