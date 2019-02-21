# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

echo "Entering .bashrc on $HOSTNAME, PATH=$PATH"

# This causes the nix configuration to happen, so I want it in both login
# and non-login (and even non-interactive?) shells (such as emacs sub-shells.)
. /etc/profile

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# . ~/.ssh/environment

echo "Finishing .bashrc on $HOSTNAME, PATH=$PATH"
