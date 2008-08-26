(custom-set-variables
 '(javascript-indent-level 2))

(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

;(add-hook 'java-mode-hook 'my-java-mode-hook)
;
;(defun my-java-mode-hook ()
;  (cond (window-system
;	 (require 'andersl-java-font-lock)
;	 (setq tab-width 4)
;	 (setq c-basic-offset 4)
;	 (turn-on-font-lock))))
