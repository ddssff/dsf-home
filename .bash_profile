# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

echo "Entering .bash_profile on $HOSTNAME, PATH=$PATH"

[ -f ~/.bashrc ] && . ~/.bashrc

[ -f git/dsf-home/bash/history ] && . git/dsf-home/bash/history
[ -f git/dsf-home/bash/winsize ] && . git/dsf-home/bash/winsize
[ -f git/dsf-home/bash/lessopts ] && . git/dsf-home/bash/lessopts
[ -f git/dsf-home/bash/chroot ] && . git/dsf-home/bash/chroot
[ -f git/dsf-home/bash/color ] && . git/dsf-home/bash/color
[ -f git/dsf-home/bash/title ] && . git/dsf-home/bash/title
[ -f git/dsf-home/bash/aliases ] && . git/dsf-home/bash/aliases
[ -f git/dsf-home/bash/completion ] && . git/dsf-home/bash/completion
[ -f git/dsf-home/bash/umask ] && . git/dsf-home/bash/umask

echo "Finishing .bash_profile on $HOSTNAME, PATH=$PATH"
