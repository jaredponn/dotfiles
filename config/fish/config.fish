## Start X at login
#if status --is-login
#    if test -z "$DISPLAY" -a $XDG_VTNR = 1
#        exec startx -- -keeptty
#    end
#end
#
## Removes the Fish greeting
set fish_greeting

## fish git prompt
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

## Status Chars
set __fish_git_prompt_char_dirtystate ''
#set __fish_git_prompt_char_dirtystate '⚡' # this doe weird space shifting
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

# git stuff
function fish_prompt
  set last_status $status

  set_color $fish_color_cwd
  printf '%s' (prompt_pwd)
  set_color normal

  printf '%s ' (__fish_git_prompt)

  set_color normal
end

## fkill - kill process
function fkill
 set PROCESS (ps -ef | sed 1d | fzf -m | awk '{print $2}')
 kill $PROCESS
 echo "Killed " $PROCESS
end

## ALIASES
alias v "vim" # v for vim
alias xv "vim-x11" # v for vim
alias pv "vim-huge-python3" # v for vim
alias n "nvim" # n for vim
alias nvimrc "n ~/.config/nvim/init.vim" # n for vim
alias zathura "zathura --fork" # v for vim
alias ghc "stack ghc --"
alias ghci "stack ghci"
alias editFish "n ~/.config/fish/config.fish"
alias fishconf "n ~/.config/fish/config.fish"
alias fishrc "n ~/.config/fish/config.fish"
alias uofc "ssh jared.pon1@linux.cpsc.ucalgary.ca -Y"
alias startwpa "sudo wpa_supplicant -B -Dnl80211 -i wlp3s0 -c/etc/wpa_supplicant/wpa_supplicant-wlp3s0.conf"

## modestly get yt song
alias getsong "youtube-dl --extract-audio -o '~/Music/bin/%(title)s.%(ext)s' "

## lower brightness
alias realdim "echo 10 | sudo tee  /sys/class/backlight/intel_backlight/brightness"
alias dim "echo 200 | sudo tee  /sys/class/backlight/intel_backlight/brightness"
alias bright "echo 800 | sudo tee  /sys/class/backlight/intel_backlight/brightness"
alias discord "flatpak run com.discordapp.Discord/x86_64/stable "

## adding .local/bin to the path. Needed for Haskell's Stack
set -gx PATH /home/jared/.local/bin $PATH
set -gx PATH /home/jared/go/bin $PATH
