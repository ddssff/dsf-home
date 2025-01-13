;;;;;;;;;;;;;;;;;;
;; KEY BINDINGs ;;
;;;;;;;;;;;;;;;;;;

(define-key esc-map "M" 'manual-entry)
(define-key esc-map "K" 'bury-buffer)
(define-key esc-map "Z" 'shell)
(define-key ctl-x-map "|" 'split-window-horizontally)
(define-key ctl-x-map "\C-l" 'goto-line)
(define-key ctl-x-map "\C-b" 'buffer-menu-other-window)

;; Set font size according to screen height

(set-face-attribute 'default nil
                    :family "Monospace"
                    :height (cond ((eq (display-pixel-height) 1080) 110) ;; thinkpad g10?
				  ((eq (display-pixel-height) 1200) 120) ;; thinkpad g10?
				  ((eq (display-pixel-height) 1440) 160) ;; thinkpad
				  ((eq (display-pixel-height) 2160) 140) ;; uhd benq monitor
				  (t 160))
                    :weight 'normal
                    :width 'normal)

;; My vc-git package

(add-to-list 'load-path "~/elisp")
(load-library "vc-git-dired")

(set-language-environment "UTF-8")
(setq inhibit-splash-screen t)
(setq-default show-trailing-whitespace t)
(setq require-final-newline nil)
; (setq isearch-lax-whitespace nil)
(setq grep-command "grep -r -n --exclude-dir=dist-newstyle -e ")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq visible-bell t)
; (setq sort-fold-case t)

;;;;;;;;;;;;;;;;;;;;
;; BEHAVIOR FIXES ;;
;;;;;;;;;;;;;;;;;;;;

;; "Fix" emacs23 window splitting behavior.
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Fix emacs23 line-move behavior
(setq line-move-visual nil)

(electric-indent-mode -1) ; Restore old behavior

;;;;;;;;;;;;;;;;;;;;
;; VI WORD MOTION ;;
;;;;;;;;;;;;;;;;;;;;

;; Change M-d and M-f to conform to vi's notion of a word.

(defvar my-white-chars "[ \t\n]")
(defvar my-word-chars "[a-zA-Z_]")
(defvar my-non-word-chars "[^a-zA-Z_ \t\n]")
(defvar my-word-pattern (concat "\\("
				my-white-chars "+\\)\\|\\("
				my-word-chars "+" my-white-chars "*\\)\\|\\("
				my-non-word-chars "+" my-white-chars "*\\)"))

(defun my-forward-word (count)
  "Stop at the beginning of the COUNT'th words from point."
  (interactive "p")
  (if (re-search-forward my-word-pattern nil t count)
      t (ding)))

(defun my-delete-word (count)
  "Delete up to the beginning of the COUNT'th words from point."
  (interactive "p")
  (if (re-search-forward my-word-pattern nil t count)
      (replace-match "" nil nil)
    (ding)))

(defun yank-after-point nil (interactive) (yank '(t)))

(define-key esc-map "f" 'my-forward-word)
(define-key esc-map "d" 'my-delete-word)

;;;;;;;;;;;;;;;;;;
;; HASKELL MODE ;;
;;;;;;;;;;;;;;;;;;

(require 'haskell-mode)

;;;;;;;;;;;;;
;; COMPILE ;;
;;;;;;;;;;;;;

(defun remove-ghc-environment-and-recompile ()
  "Remove any .ghc.environment* file and recompile"
  (interactive)
  (shell-command "rm .ghc.environment*")
  (recompile))

(define-key esc-map "C" 'compile)
(define-key global-map "\C-^" 'next-error)
(define-key compilation-mode-map "G" 'remove-ghc-environment-and-recompile)
