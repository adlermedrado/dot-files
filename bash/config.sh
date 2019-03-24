# update Path and prompt
export PATH="/home/amedrado/.pyenv/bin:/home/amedrado/.local/bin:$PATH"
export PS1=$PS1"(\$(git branch 2>/dev/null | grep '^*' | colrm 1 2)) \$ "

# Pyenv Config
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

