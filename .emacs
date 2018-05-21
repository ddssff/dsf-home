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
;(define-key global-map (kbd "C-x C-b") 'buffer-menu-other-window) ; Restore old behavior?

;(defun my-buffer-menu-other-window ()
;  "Pop up the buffer menu in other window"
;  (interactive)
;  (other-window 1)
;  (buffer-menu)
;  (other-window -1))
;
;(define-key global-map (kbd "C-x C-b") 'my-buffer-menu-other-window)

(setq my-switch-to-buffer-alist t)

(defun switch-to-buffer-other-window (buffer-or-name &optional norecord)
  "Select the buffer specified by BUFFER-OR-NAME in another window.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second argument NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other window: ")))
  (message "foo")
  (let ((pop-up-windows t))
    (pop-to-buffer buffer-or-name my-switch-to-buffer-alist norecord)))

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

;(defun read-buffer-to-switch (prompt)
;  "Read the name of a buffer to switch to, prompting with PROMPT.
;Return the name of the buffer as a string.
;
;This function is intended for the `switch-to-buffer' family of
;commands since these need to omit the name of the current buffer
;from the list of completions and default values."
;  (read-buffer prompt))

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
      (read-buffer prompt (other-buffer)
                   (confirm-nonexistent-file-or-buffer)))))

;; Set default font
(set-face-attribute 'default nil
                    :family "Monospace"
                    :height 140
                    :weight 'normal
                    :width 'normal)
