(load-library "cl")		; Includes remove-if-not used below

;; do not display a splash screen on startup.  If you use emacs as a
;; mime type, the file you click on won't appear without this.  Before
;; emacs 21 it would appear after a short wait.
(setq inhibit-splash-screen t)

;; "Fix" emacs23 window splitting behavior.
(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Fix emacs23 line-move behavior
(setq line-move-visual nil)

(apply 'debian-run-directories
 (remove-duplicates
  (remove-if-not
   (lambda (d) (file-exists-p d))
   (mapcar 'expand-file-name '("~/elisp/start.d" "~dsf/elisp/start.d")))
  :test 'string-equal))

;(load-library "xtla-dired")
;(load-library "vc-darcs")
(load-library "vc-darcs-dired")

;; http://www.emacswiki.org/cgi-bin/wiki/CustomizeAquamacs#toc17
(if (not (string< emacs-version "22"))
    (define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word))

;(load-library "vc-svn")
(custom-set-variables
  ;; custom-set-variables was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 '(canlock-password "ec0c9cfaca1de928df7e15c5b644fd9469dfe82e")
 '(display-time-format (concat "dsf" "@" "server" " - %H:%M%p"))
 '(display-time-interval 10)
 '(vc-cvs-diff-switches "-u")
 '(vc-cvs-stay-local t)
 '(vc-diff-switches "-b")
 '(vc-dired-recurse nil)
 '(vc-dired-terse-display (quote vc)))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;(setq comint-scroll-show-maximum-output nil)

(put 'upcase-region 'disabled nil)

(put 'scroll-left 'disabled nil)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
