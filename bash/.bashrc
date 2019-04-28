# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
source $HOME/config.sh

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
source $HOME/commands.sh


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/amedrado/.sdkman"
[[ -s "/home/amedrado/.sdkman/bin/sdkman-init.sh" ]] && source "/home/amedrado/.sdkman/bin/sdkman-init.sh"
