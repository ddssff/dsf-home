
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "~/git/dsf-home/elisp" load-path))
(setq load-path (cons "~/git/dsf-home/elisp/start.d" load-path))

(load-library "50dired")
(load-library "50haskell")
(load-library "50narrow")
(load-library "50shell")
(load-library "50vi")
(load-library "my-next-window")

(load-library "vc-git-dired")

;; "Fix" emacs23 window splitting behavior.
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Fix emacs23 line-move behavior
(setq line-move-visual nil)

(setq grep-command "grep -r '--include=*.hs' '--include=*.cabal' -n -e ")
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

(define-key esc-map "M" 'manual-entry)
(define-key esc-map "K" 'bury-buffer)
(define-key ctl-x-map "|" 'split-window-horizontally)
(define-key ctl-x-map "\C-l" 'goto-line)
(define-key esc-map "L" 'insert-changelog)
(define-key esc-map "C" 'compile)
(define-key global-map "\C-^" 'next-error)

(setenv "PAGER" "cat") ;; don't try to use less/more in M-x shell
(setenv "NIX_REMOTE_SYSTEMS" "/etc/nix/machines") ;; will be used when we have distributed builds
;(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp") ;; so we can find `nix-mode`
;(require 'nix-mode) ;; for editting .nix files. Hopefully available even if you have not installed emacs through nix

; Do this last so we notice errors
(set-background-color "white")
(electric-indent-mode -1) ; Restore old behavior
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-log t)
 '(inferior-haskell-find-project-root t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; Use next-buffer instead of other-buffer
(defun read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.
Return the name of the buffer as a string.

This function is intended for the `switch-to-buffer' family of
commands since these need to omit the name of the current buffer
from the list of completions and default values."
  (let ((rbts-completion-table (internal-complete-buffer-except)))
    (minibuffer-with-setup-hook
        (lambda ()
          (setq minibuffer-completion-table rbts-completion-table)
          ;; Since rbts-completion-table is built dynamically, we
          ;; can't just add it to the default value of
          ;; icomplete-with-completion-tables, so we add it
          ;; here manually.
          (if (and (boundp 'icomplete-with-completion-tables)
                   (listp icomplete-with-completion-tables))
              (set (make-local-variable 'icomplete-with-completion-tables)
                   (cons rbts-completion-table
                         icomplete-with-completion-tables))))
      (read-buffer prompt (next-buffer)
                   (confirm-nonexistent-file-or-buffer)))))

;; Set default font
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 140
                    :weight 'normal
                    :width 'normal)
