#!/bin/sh

# A wrapper for dmenu_run which puts the time as the first result so it 
# is easy to see what time it is...

tmpfile=$(mktemp /tmp/xmonadDmenuListXXXXXXX)
# the format string:
# %H is hour (24 hour time)
# %M is minute
# %S is second
# %a is weekday (Sun)
# %b is month (Jan)
# %e is day of month
echo $(date +"%H:%M:%S (%a %b %e)") >> $tmpfile
dmenu_path >> $tmpfile

cat $tmpfile | dmenu -b -l 1 -fn 'Noto Sans-100' | ${SHELL:-"/bin/sh"} &
# -b                  is display on bottom
# -l 1                is display one vertical line
# -fn 'Noto Sans-100' is use font 

# this is what the original dmenu_run script does...
# dmenu_path | dmenu "$@" | ${SHELL:-"/bin/sh"} &

rm $tmpfile
