;;;;;;;;;;;;;;;;;;
;; KEY BINDINGs ;;
;;;;;;;;;;;;;;;;;;


(define-key esc-map "M" 'manual-entry)
(define-key esc-map "K" 'bury-buffer)
(define-key esc-map "Z" 'shell)
(define-key ctl-x-map "|" 'split-window-horizontally)
(define-key ctl-x-map "\C-l" 'goto-line)
(define-key ctl-x-map "\C-b" 'buffer-menu-other-window)
(define-key esc-map "C" 'compile)
(define-key global-map "\C-^" 'next-error)

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;

(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/start.d")
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode")
(add-to-list 'load-path "/home/dsf/.nix-profile/share/emacs/site-lisp/elpa/haskell-mode-20181122.23")
;(add-to-list 'load-path "/home/dsf/.nix-profile/share/emacs/site-lisp/elpa/haskell-mode-20190926.313")
;(add-to-list 'load-path "/home/dsf/.nix-profile/share/emacs/site-lisp/elpa/nix-mode-20190119.125")
(add-to-list 'load-path "/home/dsf/.nix-profile/share/emacs/site-lisp/elpa/nix-mode-20181212.1342")
(load-library "vc-git-dired")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEHAVIOR PREFERENCES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "UTF-8")
(setq inhibit-splash-screen t)
(setq-default show-trailing-whitespace t)
(setq require-final-newline nil)
(setq isearch-lax-whitespace nil)
(setq grep-command "grep -r -n --exclude-dir=dist-newstyle -e ")
(put 'downcase-region 'disabled nil)
(setq visible-bell t)
(setq sort-fold-case t)

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

(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/elpa/haskell-mode-20181122.23")
; 16.1-3 includes this
;(require 'inf-haskell)
(require 'haskell-mode)

; ^C^L - go haskell
; ^C^R - reload
(setq haskell-program-name "ghci")

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
; (add-hook 'haskell-mode-hook 'inferior-haskell-mode)
(add-hook 'haskell-mode-after-save-hook t)
(defun haskell-mode-after-save-handler ())
(defun haskell-doc-current-info ())
(defun turn-on-haskell-indent ())
(defun inferior-haskell-find-project-root (buf)
  (let* ((cabal-file (inferior-haskell-cabal-of-buf buf)))
    (and cabal-file (file-name-directory cabal-file))))

(add-hook 'haskell-mode-hook
   (function
    (lambda ()
      (local-set-key "\C-c\C-r"
        '(lambda () (interactive)
           (inferior-haskell-load-file t)))
      (setq comint-prompt-regexp  "^\\(\\*?[A-Z][\\._a-zA-Z0-9]*\\( \\*?[A-Z][\\._a-zA-Z0-9]*\\)*> \\)")
      (setq haskell-program-name "cabal new-repl")
      ;(setq haskell-ghci-program-name "/home/dsf/git/dsf-home/bin/ghci")
    )))

(require 'compile)


(setq multibyte-syntax-as-symbol nil)

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(defun dired-parent ()
  "This function runs dired on the parent of the current file.
If there is no associated filename, it finds the parent of (pwd)."
  (interactive)
  (let ((dirname buffer-file-name)
	(filename buffer-file-name)
	(basename)
	)
    ; Figure out what directory we're looking for
    (if (eq (cdr (assq 'major-mode (buffer-local-variables))) 'dired-mode)
	(setq filename (cdr (assq 'dired-directory
				  (buffer-local-variables)))))
    (if (eq (length dirname) 0)
	(setq dirname (substring (pwd) 10)))

    (if (string-equal (substring dirname 0 2) "~/")
	(progn
	  (setq dirname (and dirname (expand-file-name dirname)))
	  (setq filename (and filename (expand-file-name filename)))))

    (if (not (string-equal dirname "/"))
	(progn
	  (setq dirname (directory-file-name dirname))
	  (while (not (string-equal (substring dirname -1) "/"))
	    (setq basename (concat (substring dirname -1) basename))
	    (setq dirname (substring dirname 0 -1)))))

    ; Get that directory
    (find-file (directory-file-name dirname))

    ; Position the cursor
    ;(message (concat "dirname: " dirname))
    ;(message (concat "filename: " filename))
    ;(message (concat "basename: " basename))
    (let ((name (concat dirname basename)))
      (if (not (eq (length name) 0))
	  (dired-goto-file name)))
    ))

(defun dired-exit ()
  "Quit editing this directory."
  (interactive)
  (dired-do-flagged-delete)
  (kill-buffer (current-buffer)))

(load-library "dired")
(define-key dired-mode-map "q" 'dired-parent)
(define-key dired-mode-map "Q" 'dired-exit)

(defun remove-ghc-environment-and-recompile ()
  "Remove any .ghc.environment* file and recompile"
  (interactive)
  (shell-command "rm .ghc.environment*")
  (recompile))

(define-key compilation-mode-map "G" 'remove-ghc-environment-and-recompile)

;; configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `(;; Open shell in a single window
        (,(rx bos "*shell")
         (display-buffer-same-window)
         (reusable-frames . nil))
        (,(rx bos "*Buffer List*")
         (display-buffer-use-some-window)
         (reusable-frames . nil))
        (,(rx bos "*vc*")
         (display-buffer-use-some-window)
         (reusable-frames . nil))
        (,(rx bos "*grep")
         (display-buffer-use-some-window)
         (reusable-frames . nil))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))
	))

;; Set the default font based on the display height
(message (format "(display-pixel-height) -> %d" (display-pixel-height)))
(message (format "(display-mm-height) -> %d" (display-mm-height)))
;; Benq UHD - height=571mm, pixels=2160
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height (cond ((eq (display-pixel-height) 1080) 110) ;; thinkpad g10?
				  ((eq (display-pixel-height) 1200) 120) ;; thinkpad g10?
				  ((eq (display-pixel-height) 1440) 160) ;; thinkpad
				  ((eq (display-pixel-height) 2160) 230) ;; uhd benq monitor
				  (t 160))
                    :weight 'normal
                    :width 'normal)

;;;;;;;;;
;; NIX ;;
;;;;;;;;;

(setenv "PAGER" "cat") ;; don't try to use less/more in M-x shell. Alternatively you can set `NIX_PAGER` to only affect nix
(setenv "NIX_REMOTE_SYSTEMS" "/etc/nix/machines") ;; will be used when we have distributed builds
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp") ;; so we can find `nix-mode`
(add-to-list 'load-path "~/.nix-profile/share/emacs/site-lisp/elpa/nix-mode-20190119.125")
(require 'nix-mode) ;; might be required to get nix-mode to run automatically for .nix files, not sure.

;;;;;;;;;;;
;; OTHER ;;
;;;;;;;;;;;

(add-to-list 'load-path "~dsf/elisp")
(add-to-list 'load-path "~dsf/elisp/start.d")
(load-library "display-buffer-alist")
(load-library "50narrow")
(load-library "50haskell")
(put 'scroll-left 'disabled nil)

(setq compilation-search-path
      '("."
        "seereason"
        "appraisalscribe-types"
        "happstack-ghcjs-server/tools"
	"../webmodule"
	"../image-cache"
	"../alderon2"
	"../history"
	"../lens-path"
	"../sr-extra"
	"../sr-cache"
	"../chili"
	"../.."
	"../../webmodule"
	"../../image-cache"
	"../../alderon2"
	"../../history"
	"../../lens-path"
	"../../sr-extra"
	"../../sr-cache"
	"../../chili"
	"../.."
	"../../../webmodule"
	"../../../image-cache"
	"../../../alderon2"
	"../../../history"
	"../../../lens-path"
	"../../../sr-extra"
	"../../../sr-cache"
	"../../../chili"
	))

(defun insert-date ()
  "Insert current date in abbreviated format."
  (interactive "*")
  (let* ((now (current-time-string))	;With ugly-printed timestamp
	 (firstchar (string-to-char (substring now 8 9))))
    (if (/= firstchar 32) (insert-char firstchar 1))
    (insert (substring now 9 10) " "	;Insert day of month
	    (substring now 4 7) " "	;Abbreviated name of month
	    (substring now 20 24))))	;Full year number

(define-key esc-map "D" 'insert-date)

(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(undo-limit 800000)
 '(undo-strong-limit 1200000))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
