;;;;;;;;;;;;;;;;;;
;; KEY BINDINGs ;;
;;;;;;;;;;;;;;;;;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(load-library "vc-git-dired")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEHAVIOR PREFERENCES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)
(setq-default show-trailing-whitespace t)
(setq require-final-newline nil)
(setq isearch-lax-whitespace nil)
(setq grep-command "grep -r -n -e ")

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

(require 'inf-haskell)

; ^C^L - go haskell
; ^C^R - reload
(setq haskell-program-name "ghci")

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)

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
;      (setq haskell-program-name "cabal repl")
      ;(setq haskell-ghci-program-name "/home/dsf/git/dsf-home/bin/ghci")
    )))

(require 'compile)

; Try to improve next-error performance on ghc error messages
;(set-default 'compilation-error-regexp-alist '(ghc))
;(add-to-list 'compilation-error-regexp-alist-alist
;  '(ghc "^\\(\\|               defined at \\|               imported from `[^']*\' at \\|               \(and originally defined at \\)\\([^ 	\n]+\\):\\([0-9]+\\):\\([0-9]+\\):?$" 2 3 4))

;(add-hook 'darcs-dired-mode-hook
;       (lambda () (setq compile-command "runhaskell Setup build")))

(defconst haskell-compilation-error-regexp-alist
  `((,(concat
       "^ *\\(?1:[^\t\r\n]+?\\):"
       "\\(?:"
       "\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?" ;; "121:1" & "12:3-5"
       "\\|"
       "(\\(?2:[0-9]+\\),\\(?4:[0-9]+\\))-(\\(?3:[0-9]+\\),\\(?5:[0-9]+\\))" ;; "(289,5)-(291,36)"
       "\\)"
       ":\\(?6: Warning:\\)?")
     1 (2 . 3) (4 . 5) (6 . nil)) ;; error/warning locus

    ;; multiple declarations
    ("^    \\(?:Declared at:\\|            \\) \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)$"
     1 2 4 0) ;; info locus

    ;; this is the weakest pattern as it's subject to line wrapping et al.
    (" at \\(?1:[^ \t\r\n]+\\):\\(?2:[0-9]+\\):\\(?4:[0-9]+\\)\\(?:-\\(?5:[0-9]+\\)\\)?[)]?$"
     1 2 (4 . 5) 0)) ;; info locus
  "Regexps used for matching GHC compile messages.
See `compilation-error-regexp-alist' for semantics.")

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


;; Configure `display-buffer' behaviour for some special buffers
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

;; Set 14pt default font
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `(;; Open shell in a single window
        (,(rx bos "*shell")
         (display-buffer-same-window)
         (reusable-frames . nil))
        (,(rx bos "*grep")
         (display-buffer-next-window)
         (reusable-frames . nil))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))
	))

;;;;;;;;;;;
;; OTHER ;;
;;;;;;;;;;;

(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/start.d")
(load-library "my-next-window")
(load-library "50narrow")
