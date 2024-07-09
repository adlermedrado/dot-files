export PATH="/$HOME/bin:$HOME/.local/bin:'/Applications/IntelliJ IDEA.app/Contents/MacOS':opt/homebrew/sbin:$PATH"
export GREP_OPTIONS="--color=always"
export CLICOLOR="--color=auto"
export LSCOLORS=GxFxCxDxBxegedabagaced
export PYENV_ROOT="$HOME/.pyenv"

HISTSIZE=10000
HISTFILESIZE=10000

alias idea="/Applications/Intellij\ IDEA.app/Contents/MacOS/idea"
alias brewupd="brew upgrade && brew cleanup"
alias tn="tmux new-session -s "
alias tat="tmux attach -t "
alias tls="tmux ls"
alias ls="ls -G"
alias emacs="/opt/homebrew/opt/emacs-plus@29/bin/emacs"
alias emacsclient="/opt/homebrew/opt/emacs-plus@29/bin/emacsclient -c"
alias vim='nvim'

eval "$(/opt/homebrew/bin/brew shellenv)"
eval "$(starship init bash)"

if [ -d "$(brew --prefix)/opt/grep/libexec/gnubin" ]; then
    PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
. "$HOME/.cargo/env"
