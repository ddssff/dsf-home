(setenv "PAGER" "cat") ;; don't try to use less/more in M-x shell
(setenv "NIX_REMOTE_SYSTEMS" "/etc/nix/machines") ;; will be used when we have distributed builds
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp") ;; so we can find `nix-mode`
(require 'nix-mode) ;; for editting .nix files. Hopefully available even if you have not installed emacs through nix
