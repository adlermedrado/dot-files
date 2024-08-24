fpath+=~/.zfunc
autoload -Uz compinit && compinit

# starship config
export STARSHIP_CONFIG=~/code/dot-files/starship.toml
eval "$(starship init zsh)"

# Enable colors on ls command
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
alias ls='ls --color=auto'

# load fzf
source <(fzf --zsh)

# aliases
alias brewupd="brew update && brew upgrade && brew cleanup"
alias tn="tmux new-session -s "
alias tat="tmux attach -t "
alias tls="tmux ls"
alias ls="ls -G"
alias vim='nvim'

# Created by `pipx` on 2024-08-07 22:44:29
export PATH="$PATH:/Users/amedrado/.local/bin"
