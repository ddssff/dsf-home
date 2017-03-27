(setq load-path (cons "~/git/dsf-home/elisp" load-path))
(setq load-path (cons "~/git/dsf-home/elisp/start.d" load-path))

(load-library "50vi")
(load-library "50dired")
(load-library "50haskell")
(load-library "50narrow")
(load-library "vc-git-dired")

;; "Fix" emacs23 window splitting behavior.
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Fix emacs23 line-move behavior
(setq line-move-visual nil)

(setq grep-command "grep -a -n -e ")   ; vs "grep -nH -e "
(setq-default show-trailing-whitespace t)
(setq require-final-newline nil)
(setq isearch-lax-whitespace nil)

;; do not display a splash screen on startup.  If you use emacs as a
;; mime type, the file you click on won't appear without this.  Before
;; emacs 21 it would appear after a short wait.
(setq inhibit-splash-screen t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(define-key esc-map "Z" 'shell)
(define-key esc-map "M" 'manual-entry)
(define-key esc-map "K" 'bury-buffer)
(define-key ctl-x-map "|" 'split-window-horizontally)
(define-key ctl-x-map "\C-l" 'goto-line)
(define-key esc-map "L" 'insert-changelog)
(define-key esc-map "C" 'compile)
(define-key global-map "\C-^" 'next-error)

(setenv "PAGER" "cat") ;; don't try to use less/more in M-x shell
(setenv "NIX_REMOTE_SYSTEMS" "/etc/nix/machines") ;; will be used when we have distributed builds
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp") ;; so we can find `nix-mode`
(require 'nix-mode) ;; for editting .nix files. Hopefully available even if you have not installed emacs through nix

; Do this last so we notice errors
(set-background-color "white")
