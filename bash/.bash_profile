export PATH="/$HOME/bin:$HOME/.local/bin:'/Applications/IntelliJ IDEA.app/Contents/MacOS':opt/homebrew/sbin:$PATH"
export EDITOR="nvim"
alias ls="ls -G"
alias idea="/Applications/Intellij\ IDEA.app/Contents/MacOS/idea"
alias vim=nvim
alias brewupd="brew upgrade && brew cleanup"

eval "$(/opt/homebrew/bin/brew shellenv)"
eval "$(starship init bash)"

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

source <(op completion bash)

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
source /Users/adlermedrado/.config/op/plugins.sh
