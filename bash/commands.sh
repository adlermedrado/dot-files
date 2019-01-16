c() { 
    cd ~/devel/$* && pyenv activate $*;
}

dp() {
    pyenv deactivate
}

ctp() {
    `~/devel/automate/vim/update-ctags-for-python-project.sh`
}

alias gss="git status --show-stash"
