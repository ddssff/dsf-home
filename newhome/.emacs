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

;; Set font size according to screen height

(set-face-attribute 'default nil
                    :family "Monospace"
                    :height (cond ((eq (display-pixel-height) 1080) 110) ;; thinkpad g10?
				  ((eq (display-pixel-height) 1200) 150) ;; thinkpad g10? <--
				  ((eq (display-pixel-height) 1440) 160) ;; thinkpad
				  ((eq (display-pixel-height) 2160) 200) ;; uhd benq monitor
				  (t 160))
                    :weight 'normal
                    :width 'normal)

;; My vc-git package

(add-to-list 'load-path "~dsf/elisp")
(load-library "vc-git-dired")
(load-library "50narrow")
(load-library "50display-time")

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
(setq undo-outer-limit 100000000)

(setq-default frame-title-format (file-name-nondirectory (directory-file-name default-directory)))

;;;;;;;;;;;;;;;;;;;;
;; BEHAVIOR FIXES ;;
;;;;;;;;;;;;;;;;;;;;

;; "Fix" emacs23 window splitting behavior.
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Fix emacs23 line-move behavior
(setq line-move-visual nil)

(electric-indent-mode -1) ; Restore old behavior

;; Never switch windows for shell (C-Z)
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

;; Never switch windows for C-x b
(defun switch-to-buffer-same-window (buffer-or-name &optional norecord)
  "Select the buffer specified by BUFFER-OR-NAME in this window.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

If called interactively, read the buffer name using `read-buffer'.
The variable `confirm-nonexistent-file-or-buffer' determines
whether to request confirmation before creating a new buffer.
See `read-buffer' for features related to input and completion
of buffer names.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second argument NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in same window: ")))
  (switch-to-buffer buffer-or-name nil norecord))

(define-key ctl-x-map "b" 'switch-to-buffer-same-window)
(define-key ctl-x-map "b" 'switch-to-buffer)

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

;;;;;;;;;;;;;
;; COMPILE ;;
;;;;;;;;;;;;;

(require 'compile)

(defun remove-ghc-environment-and-recompile ()
  "Remove any .ghc.environment* file and recompile"
  (interactive)
  (shell-command "rm .ghc.environment*")
  (recompile))

(define-key esc-map "C" 'compile)
(define-key global-map "\C-^" 'next-error)
(define-key compilation-mode-map "G" 'remove-ghc-environment-and-recompile)

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

;;;;;;;;;;;;;
;; HASKELL ;;
;;;;;;;;;;;;;

(cond
 ((file-exists-p "~dsf/.nix-profile/share/emacs/site-lisp/elpa/haskell-mode-20181122.23")
  (add-to-list 'load-path "~dsf/.nix-profile/share/emacs/site-lisp/elpa/haskell-mode-20181122.23")
  (require 'haskell-mode)
  (defun haskell-mode-after-save-handler ())))

(put 'scroll-left 'disabled nil)

;;;;;;;;;
;; GIT ;;
;;;;;;;;;

(defun push-this-buffer (comment)
  (interactive (list (read-string (format "push for [%s]: " default-directory))))
  (save-buffer)
  (shell-command (format "git add -A; git commit -a -m \" %s\"; git push &"
                     comment)))
