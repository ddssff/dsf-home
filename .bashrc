# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

#echo "Entering .bashrc on $HOSTNAME, PATH=$PATH"

# This causes the nix configuration to happen, so I want it in both login
# and non-login (and even non-interactive?) shells (such as emacs sub-shells.)
. /etc/profile

#set -x

# allow nix-daemon to use your ssh-agent
for i in `seq 1 32`; do
    setfacl -m "u:nixbld${i}:r-x" $HOME
    setfacl -m "u:nixbld${i}:r-x" $HOME/nix-seereason
    setfacl -m "u:nixbld${i}:r--" $HOME/nix-seereason/ssh-config
    if [[ ! -z "$SSH_AUTH_SOCK" ]] ; then
      setfacl -m "u:nixbld${i}:rwx" $(dirname ${SSH_AUTH_SOCK})
      setfacl -m "u:nixbld${i}:rwx" ${SSH_AUTH_SOCK}
    fi
done

export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"
#export  NIX_PATH="nixpkgs=$HOME/nix-seereason/nixpkgs-channels:ssh-auth-sock=$SSH_AUTH_SOCK:ssh-config-file=$HOME/nix-seereason/ssh-config"
export NIX_PATH="nixpkgs=$HOME/nix-seereason/nixpkgs-channels:nixpkgs-overlays=$HOME/nix-seereason/seereason-local.nix:ssh-auth-sock=$SSH_AUTH_SOCK:ssh-config-file=$HOME/nix-seereason/ssh-config"

# Make sure the locale is set
export LANG=C

#set +x

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# . ~/.ssh/environment

#echo "Finishing .bashrc on $HOSTNAME, PATH=$PATH"
