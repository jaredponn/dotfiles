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

alias zshrc='n ~/.zshrc'
alias vimrc='n ~/Jared-s-Config-Files/config/nvim/init.vim'

alias realdim='echo 10 | sudo tee  /sys/class/backlight/intel_backlight/brightness'
alias dim='echo 200 | sudo tee  /sys/class/backlight/intel_backlight/brightness'
alias bright='echo 800 | sudo tee  /sys/class/backlight/intel_backlight/brightness'

# Path
PATH=$PATH:~/.local/bin/
