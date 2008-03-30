;; Global settings and key bindings

(auto-compression-mode 1)
(setq c-basic-offset 2)
(put 'downcase-region 'disabled nil)

(define-key esc-map "Z" 'shell)
(defvar explicit-shell-file-name "bash")
(define-key esc-map "M" 'manual-entry)
(define-key esc-map "K" 'bury-buffer)
(define-key esc-map "C" 'compile)
(define-key ctl-x-map "\C-l" 'goto-line)
(define-key ctl-x-map "|" 'split-window-horizontally)
(define-key esc-map "D" 'insert-date)
(define-key esc-map "T" 'insert-date-and-time)
(define-key esc-map "L" 'insert-changelog)
(define-key esc-map "G" 'fill-block)
;(define-key esc-map "." 'set-mark-command)	;; for remote connections
;(global-set-key "\C-u" 'universal-argument)
(define-key global-map "\C-^" 'next-error)
(define-key ctl-x-map "p" 'send-invisible)

(if window-system
    (progn
      (global-set-key [f11] 'toggle-modes)
      (global-set-key [f9] 'backward-narrowed-page)
      (global-set-key [f10] 'forward-narrowed-page))
  (global-unset-key "\e[")
  (define-key esc-map "[20~" 'backward-narrowed-page)	;; L9 via telnet
  (define-key esc-map "[21~" 'forward-narrowed-page))	;; L10 via telnet

;; I suppose these could go in language specific files.

(setq auto-mode-alist
      (append '(("\\.C$"  . c++-mode)
		("\\.cc$" . c++-mode)
		("\\.c$" . c++-mode)
		("\\.java$" . java-mode)
		("\\.i$" . c++-mode)	; Inline function file
		("\\.ff$" . c++-mode)	; Foreign function file
		("\\.cxx$" . c++-mode)
		("\\.c$"  . c-mode)	; to edit C code
		("\\.h$"  . c++-mode)
		("\\.stk$" . scheme-mode)
		("\\.sch$" . scheme-mode)
		("\\.cdecl$" . scheme-mode)
		("\\.stklos$" . scheme-mode)
		("\\.ltx$" . latex-mode)
		("\\.nw$" . latex-mode)	; To edit noweb files 
		("\\.tgz$" . tar-mode)
		)
	      auto-mode-alist))

(setq auto-mode-alist
      (append '(("^/tmp/article" . text-mode)
		("\\.ol$" . outline-mode)
		("\\.du$" . outline-mode)
		("\\TODO" . outline-mode)
		("\\.tabrc$" . scheme-mode)
		("\\.tabconfig$" . scheme-mode))
	      auto-mode-alist))


; Interesting but a little funky.  Use -r flags.
;(setq grep-command "find . -type f | xargs grep -n -e ")
; Process apparantly binary files
(setq grep-command "grep -a -n -e ")
