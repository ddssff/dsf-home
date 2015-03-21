#!/bin/sh

# if [ -f $HOME/.Xmodmap ]; then xmodmap $HOME/.Xmodmap; fi
# xset dpms 300 600 900
# xset +dpms

# export DSFNOWRAP=yes
# export DSFEQTEST=yes
# export DSFRAISELOWER=yes

# export PATH="$HOME/bin:$PATH:/sbin:/usr/sbin"
# export EDITOR="emacs"
# export ALTERNATE_EDITOR=emacs
# export VISUAL="emacsclient"
# export CVS_RSH=ssh

. .ssh/environment

exec x-session-manager
