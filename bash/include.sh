export PATH="/home/adler/.pyenv/bin:$PATH"

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

c() { 
    cd ~/devel/$* && pipenv shell;
}

ctp() {
    `~/devel/automate/vim/update-ctags-for-python-project.sh`
}
