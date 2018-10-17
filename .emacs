
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "~/git/dsf-home/elisp" load-path))
(setq load-path (cons "~/git/dsf-home/elisp/start.d" load-path))
(load-library "50dired") ; dired-parent, dired-exit, etc
(load-library "50narrow")
(load-library "50shell") ; shell with pop-to-buffer-same-window
(load-library "50vi") ; my-delete-word, my-forward-word
(load-library "vc-git-dired")

;; Fix emacs23 line-move behavior
(setq line-move-visual nil)

;; "Fix" emacs23 window splitting behavior.
(setq split-width-threshold nil)
(setq split-height-threshold nil)

(setq-default show-trailing-whitespace t)
(setq require-final-newline nil)
(setq isearch-lax-whitespace nil)
; Default: "grep --color -nH -e "
;(setq grep-command "grep -r '--include=*.hs' '--include=*.cabal' -n -e ")
(setq grep-command "grep -r -n -e ")

;; do not display a splash screen on startup.  If you use emacs as a
;; mime type, the file you click on won't appear without this.  Before
;; emacs 21 it would appear after a short wait.
(setq inhibit-splash-screen t)

;; My key bindings
(define-key esc-map "M" 'manual-entry)
(define-key esc-map "K" 'bury-buffer)
(define-key ctl-x-map "|" 'split-window-horizontally)
(define-key ctl-x-map "\C-l" 'goto-line)
(define-key esc-map "L" 'insert-changelog)
(define-key esc-map "C" 'compile)
(define-key global-map "\C-^" 'next-error)
(define-key esc-map "Z" 'my-shell)

; this flet doesn't work as hoped
(defun my-shell (&optional buffer)
  ""
  (interactive)
  (letf ((pop-to-buffer (symbol-function 'pop-to-buffer-same-window)))
    (shell buffer)))

;; Make default font larger 
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 140
                    :weight 'normal
                    :width 'normal)

(electric-indent-mode -1) ; Restore old behavior

(load-library "50haskell")
