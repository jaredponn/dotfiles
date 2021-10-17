#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias n='nvim'
alias zathura='zathura --fork'
alias pdflatex='pdflatex -interaction=nonstopmode'

alias bashrc='n ~/dotfiles/bashrc'
alias xmonadrc='n ~/dotfiles/xmonad/xmonad.hs'

# alias dim='echo 2000 | sudo tee /sys/class/backlight/intel_backlight/brightness'

# Setting up the wacom!!
# Use this to find the id of what you want to map to the other screen
# xsetwacom list devices 
# Moreover, find the screen name that you want to map to with
# (use HEAD-0 for the first monitor listed, HEAD-1 for the second, and so on...)
# xrandr --listactivemonitors
# Then, with the wacom <ID> (should be a number), and the screen <SCREEN> run
# xsetwacom set <ID> MapToOutput <SCREEN>
alias tabletSetting0='xsetwacom set 12 MapToOutput eDP0'
alias tabletSetting1='xsetwacom set 12 MapToOutput DP-1'


# Pretty showing where you are
PS1="\[\033[38;5;6m\][\@]\[$(tput sgr0)\] \[\033[01;36m\][\u@\h\[\033[01;37m\] \w\[\033[01;36m\]]\$\[\033[00m\] "
# This one just shows the ``head directory" not the entire ``pwd" output...
# PS1="\[\033[38;5;6m\][\@]\[$(tput sgr0)\] \[\033[01;36m\][\u@\h\[\033[01;37m\] \W\[\033[01;36m\]]\$\[\033[00m\] "

# SSH 
# https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
# Start with running
# eval "$(ssh-agent -s)"
# to start the agent -- note we need to use eval so the environment variables change
# the parent process...
# Then, add the required keys by 
# ssh-add ~/.ssh/id_ed234
# if [ -z "$SSH_AUTH_SOCK" ]
# then
#     eval `ssh-agent -s`
#     ssh-add 
# fi

# if [ ! -S ~/.ssh/ssh_auth_sock ]
# then
#   eval `ssh-agent`
#   ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
# fi
# export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock
# ssh-add -l > /dev/null || ssh-add

export VISUAL=nvim
export EDITOR="$VISUAL"

# Used to prevent the computer from sleeping
# while in a zoom meeting (well in general actually..)
# Actually, I'm pretty sure this only needs to be run once, not
# every time I load the bashrc...
xset s off -dpms
