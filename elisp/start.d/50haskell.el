(require 'inf-haskell)

; ^C^L - go haskell
; ^C^R - reload
(setq haskell-program-name "ghci")
;(setq haskell-program-name "ghcjsi")

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook
   (function
    (lambda ()
      (local-set-key "\C-c\C-r"
        '(lambda () (interactive)
           (inferior-haskell-load-file t)))
      (setq comint-prompt-regexp  "^\\(\\*?[A-Z][\\._a-zA-Z0-9]*\\( \\*?[A-Z][\\._a-zA-Z0-9]*\\)*> \\)")
;      (setq haskell-program-name "cabal repl")
      (setq haskell-ghci-program-name "/home/dsf/git/dsf-home/bin/ghci")
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
