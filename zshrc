# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jared/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Left view and right view thing
PS1="%F{green}[%n@%M %~]$ %f"
RPS1="%F{blue}%*%f"

# aliases
alias ls='ls --color=auto'
alias n='nvim'

alias zathura='zathura --fork'

alias zshrc='n ~/.zshrc'
alias vimrc='n ~/Jared-s-Config-Files/config/nvim/init.vim'
alias sshuofc="ssh -X jared.pon1@linux.cpsc.ucalgary.ca "

alias realdim='echo 10 | sudo tee  /sys/class/backlight/intel_backlight/brightness'
alias dim='echo 200 | sudo tee  /sys/class/backlight/intel_backlight/brightness'
alias bright='echo 800 | sudo tee  /sys/class/backlight/intel_backlight/brightness'
alias getsong="youtube-dl --extract-audio -o '~/Music/bin/%(title)s.%(ext)s' "

badresponse=("litle did they knoww that mee baddddd" "haiyang god?!?!?!?!" "wtf so badd" "haiyang god me bad carry mee" "badddd" "ME SO BAD WTFF")
alias bad='git pull && git add . && git commit -m "${badresponse[$(($RANDOM % ${#badresponse[@]} + 1 ))]}" && git push'
alias testing='echo "${badresponse[$(($RANDOM % ${#badresponse[@]} + 1 ))]}"'

alias feh='feh --auto-zoom'

alias aoeu='setxkbmap us'
alias asdf='setxkbmap dvorak'

alias fd='cd $(find . -maxdepth 6 -type d | fzf) && ls'
alias ff='n $(fzf --preview="bat --style=numbers --color=always {} | head -100")'

# Path
PATH=$PATH:~/.local/bin/

ls
