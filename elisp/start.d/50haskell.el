(require 'haskell)
(require 'haskell-mode)
;(require 'inf-haskell)

; ^C^L - go haskell
; ^C^R - reload
;(setq haskell-program-name "ghci")
;(setq haskell-program-name "ghcjsi")

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;(add-hook 'haskell-mode-hook 'inferior-haskell-mode)

(defun inferior-haskell-find-project-root (buf)
  (let* ((cabal-file (inferior-haskell-cabal-of-buf buf)))
    (and cabal-file (file-name-directory cabal-file))))

(setq haskell-process-type 'cabal-new-repl)

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

(setq multibyte-syntax-as-symbol nil)
