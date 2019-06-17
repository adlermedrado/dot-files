# update Path and prompt
PATH="$HOME/.local/bin:$HOME/bin:$PATH"
PATH="$HOME/.pyenv/bin:/home/amedrado/.local/bin:$PATH"
PATH="$HOME/devel/bashblog:$PATH"
export PATH

PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]"
PS1=$PS1"(\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)) \$ "
export PS1

# Pyenv Config
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export EDITOR=vim
