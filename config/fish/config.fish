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
set __fish_git_prompt_char_dirtystate ' '
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
alias n "nvim" # n for vim
alias zathura "zathura --fork" # v for vim
alias ghc "stack ghc --"
alias ghci "stack ghci"

## modestly get yt song
alias getsong "youtube-dl --extract-audio -o '~/Music/bin/%(title)s.%(ext)s' "

## adding .local/bin to the path. Needed for Haskell's Stack
set -gx PATH /home/jared/.local/bin $PATH
