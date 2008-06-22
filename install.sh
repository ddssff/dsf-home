set -x

for i in .bashrc .xsession elisp .emacs .purple bin .Xdefaults .bash_profile .Xmodmap; do
  [ -e $i ] || ln -s -f darcs/home/$i
  echo "ln -s -f darcs/home/$i -> $?"
done
