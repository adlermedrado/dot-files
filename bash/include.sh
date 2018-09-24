export PATH="/home/adler/.pyenv/bin:$PATH"

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

c() { 
    cd ~/devel/$* && pipenv shell;
}

alias ct='ctags -R --verbose=yes .'
