# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export LANG=en_US.UTF-8

#echo "Entering .bashrc on $HOSTNAME, PATH=$PATH"

# This causes the nix configuration to happen, so I want it in both login
# and non-login (and even non-interactive?) shells (such as emacs sub-shells.)
. /etc/profile

#set -x

# allow nix-daemon to use your ssh-agent
export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"

# Stable
export NIX_PATH="nixpkgs=$HOME/nix-seereason/nixpkgs-channels"

# Unstable
# export NIX_PATH="nixpkgs=$HOME/nixpkgs-unstable"

# NIX_PATH="$NIX_PATH:nixpkgs-overlays=$HOME/nix-seereason/seereason-local.nix"
NIX_PATH="$NIX_PATH:ssh-auth-sock=$SSH_AUTH_SOCK"
NIX_PATH="$NIX_PATH:ssh-config-file=$HOME/nix-seereason/ssh-config"

# To use ghc HEAD instead of 9.2.2
# nix-shell -I nixpkgs=/home/dsf/nixpkgs/ -p 'haskell.packages.ghcHEAD.ghcWithPackages (pkgs: with pkgs; [ ])'

# Adds nixpkgs-overlays - what is this?
#export NIX_PATH="nixpkgs=$HOME/nix-seereason/nixpkgs-channels:nixpkgs-overlays=$HOME/nix-seereason/seereason-local.nix:ssh-auth-sock=$SSH_AUTH_SOCK:ssh-config-file=$HOME/nix-seereason/ssh-config"

# Do development with ~/nixpkgs
# export NIX_PATH="nixpkgs=$HOME/nixpkgs:ssh-auth-sock=$SSH_AUTH_SOCK:ssh-config-file=$HOME/nix-seereason/ssh-config"

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

export EDITOR=emacs

# . ~/.ssh/environment

#echo "Finishing .bashrc on $HOSTNAME, PATH=$PATH"
