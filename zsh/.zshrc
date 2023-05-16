# Exports
export EDITOR=vim
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export PATH=$HOME/bin:$HOME/.local/bin:'/Applications/IntelliJ IDEA.app/Contents/MacOS':$PATH
export TERM="screen-256color"

# History
HISTCONTROL=ignoreboth:erasedups
HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000

# Misc
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history

if [[ ! -d ~/.zplug ]];then
  git clone https://github.com/zplug/zplug ~/.zplug
fi
source ~/.zplug/init.zsh
zplug "zsh-users/zsh-completions"
zplug 'zsh-users/zsh-autosuggestions'
zplug 'plugins/fzf', from:oh-my-zsh
zplug 'plugins/tmux', from:oh-my-zsh

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi
zplug load

alias ls='ls --color=auto'
alias bandit='docker run -it --rm --name bandit -v "$PWD":/workdir adlermedrado/bandit'
alias black='docker run -it --rm --name black -v "$PWD":/workdir adlermedrado/black'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(pyenv init -)"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/amedrado/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)

eval "$(starship init zsh)"

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
