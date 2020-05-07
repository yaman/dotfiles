ZSH_THEME="half-life"
HIST_STAMPS="mm/dd/yyyy"

plugins=(
  git
  cabal
  colemak
  docker
  go
  tmux
  tmuxinator
)

source $ZSH/oh-my-zsh.sh

export EDITOR='nvim'

alias vi='nvim'
alias vim='nvim'

alias t=task
alias sudo='sudo '
alias ls="exa -alsnew"
eval $(thefuck --alias)

if [[ -z "$TMUX" ]] ;then
    ID="$( tmux ls | grep -vm1 attached | cut -d: -f1 )" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        tmux new-session
    else
        tmux attach-session -t "$ID" # if available attach to it
    fi
fi


export DISABLE_AUTO_TITLE=true
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#source /usr/share/nvm/init-nvm.sh
source <(kubectl completion zsh) 

eval "$(starship init zsh)"

source /etc/profile

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/canavar/.sdkman"
[[ -s "/home/canavar/.sdkman/bin/sdkman-init.sh" ]] && source "/home/canavar/.sdkman/bin/sdkman-init.sh"
