#!/bin/sh

# if [ -f $HOME/.Xmodmap ]; then xmodmap $HOME/.Xmodmap; fi
# xset dpms 300 600 900
# xset +dpms

# export DSFNOWRAP=yes
# export DSFEQTEST=yes
# export DSFRAISELOWER=yes

export PATH="$HOME/bin:$PATH:/sbin:/usr/sbin"
export EDITOR="emacs -nw"
export VISUAL="emacs -nw"
export CVS_RSH=ssh

exec x-session-manager
