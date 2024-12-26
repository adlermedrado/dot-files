# Set path 
set -U fish_user_paths ~/.zfunc $fish_user_paths
set -U fish_user_paths /opt/homebrew/bin $fish_user_paths
set -U fish_user_paths /opt/homebrew/sbin $fish_user_paths
set -U fish_user_paths /Users/amedrado/.config/emacs/bin $fish_user_paths
set -U fish_user_paths /Users/amedrado/.local/bin $fish_user_paths
set -U fish_user_paths /usr/local/opt/mysql-client/bin $fish_user_paths

# Enable colors on ls command
set -x CLICOLOR 1
set -x LSCOLORS GxFxCxDxBxegedabagaced

# Check and load fzf if it is available 
if type -q fzf
    fzf --fish | source
end

# Aliases
alias brewupd "brew update; and brew upgrade; and brew cleanup"
alias tn "tmux new-session -s "
alias tat "tmux attach -t "
alias tls "tmux ls"
alias ls "ls -G"
alias vim nvim
alias ll "ls -latr --color=auto"
alias ss "nvim (fzf -m --preview='bat --color=always {}')"
