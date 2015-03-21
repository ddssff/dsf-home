(remove-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(require 'compile)
;(set-default 'compilation-error-regexp-alist '(ghc))
;(add-to-list 'compilation-error-regexp-alist-alist
;  '(ghc "^\\(\\|               defined at \\|               imported from `[^']*\' at \\|               \(and originally defined at \\)\\([^ 	\n]+\\):\\([0-9]+\\):\\([0-9]+\\):?$" 2 3 4))

(add-hook 'darcs-dired-mode-hook
       (lambda () (setq compile-command "runhaskell Setup build")))
