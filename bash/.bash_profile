export PATH="/$HOME/bin:$HOME/.local/bin:'/Applications/IntelliJ IDEA.app/Contents/MacOS':opt/homebrew/sbin:$HOME/.config/emacs/bin:$PATH"
export EDITOR="lvim"
export GREP_OPTIONS="--color=always"
export CLICOLOR="--color=auto"
export LSCOLORS=GxFxCxDxBxegedabagaced

HISTSIZE=10000
HISTFILESIZE=10000

alias idea="/Applications/Intellij\ IDEA.app/Contents/MacOS/idea"
alias brewupd="brew upgrade && brew cleanup"
alias tn="tmux new-session -s "
alias tat="tmux attach -t "
alias tls="tmux ls"
alias vim="lvim"

eval "$(/opt/homebrew/bin/brew shellenv)"
eval "$(starship init bash)"

if [ -d "$(brew --prefix)/opt/grep/libexec/gnubin" ]; then
    PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

source <(op completion bash)

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
. "$HOME/.cargo/env"
