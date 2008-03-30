;; FOR TUAREG MODE

(defun console-type () 'x)	; Colorize when running in an Xterm

;(setq load-path (cons (expand-file-name "~dsf/elisp/tuareg-mode") load-path))
;
(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(require 'font-lock)

(add-hook 'tuareg-mode-hook
	  '(lambda ()
	     (setq tuareg-lazy-= t)
					; indent `=' like a standard keyword
	     (setq tuareg-lazy-paren nil)
					; indent [({ like standard keywords
	     (setq tuareg-in-indent 0)
					; no indentation after `in' keywords
	     (setq tuareg-with-indent 0)
	     				; no extra indentation, just the let
             (setq tuareg-font-lock-governing '("brown" "cyan" nil t t t))
					; bold leading keywords in color mode
					; bold-italic in font mode
	     ;;(auto-fill-mode 1)
					; turn on auto-fill minor mode
	     (if (featurep 'sym-lock)   ; Sym-Lock customization only
		 (setq sym-lock-mouse-face-enabled nil))
					; turn off special face under mouse
	     ))

;; FOR OCAML MODE

;(setq load-path
;      (cons (expand-file-name "~dsf/elisp/ocaml-mode-3.05") load-path))
;(setq auto-mode-alist
;      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;(if window-system (require 'caml-font))
