#!/usr/bin/bash

if [ ! -d "${HOME}/.vim/pack" ] 
then
    echo "Creating ${HOME}/.vim/pack" 
    mkdir -p $HOME/.vim/pack
fi


install_plugin () {
    echo "Installing $2"
    if [ ! -d "~/.vim/pack/${1}/start" ]
    then 
        echo "Creating ${HOME}/.vim/pack/${1}/start" 
        mkdir -p $HOME/.vim/pack/$1/start
        cd $HOME/.vim/pack/$1/start
        git clone $2
    fi
}


declare -A plugins

plugins=(

    ["tpope"]="https://tpope.io/vim/fugitive.git"
    ["tpope"]="https://tpope.io/vim/surround.git"
    ["junegunn"]="https://github.com/junegunn/gv.vim.git"
    ["vim-nerdtree"]="https://github.com/vim-nerdtree/nerdtree.git"
    ["vim-syntastic"]="https://github.com/vim-syntastic/syntastic.git"
    ["preservim"]="https://github.com/preservim/nerdcommenter.git"
    ["ctrlpvim"]="https://github.com/ctrlpvim/ctrlp.vim.git"
    ["vim-airline"]="https://github.com/vim-airline/vim-airline.git"
    ["vim-airline"]="https://github.com/vim-airline/vim-airline-themes.git"
    ["majutsushi"]="https://github.com/majutsushi/tagbar.git"
    ["craigemery"]="https://github.com/craigemery/vim-autotag.git"
    ["easymotion"]="https://github.com/easymotion/vim-easymotion.git"
    ["Yggdroot"]="https://github.com/Yggdroot/indentLine.git"
    ["mhinz"]="https://github.com/mhinz/vim-signify.git"
    ["frazrepo"]="https://github.com/frazrepo/vim-rainbow.git"
    ["tomasr"]="https://github.com/tomasr/molokai.git"

)

for key in "${!plugins[@]}"; do install_plugin $key ${plugins[$key]}; done
