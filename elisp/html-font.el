;;; html-font.el --- Adds font-lock support to HTML helper mode in Emacs.
;; Copyright (C) 1995 Ulrik Dickow.

;; Author: Ulrik Dickow <dickow@nbi.dk> (http://www.nbi.dk/~dickow)
;; Created: 18-Jan-1995
;; Maintainer: Ulrik Dickow <dickow@nbi.dk> (http://www.nbi.dk/~dickow)
;; Version: @(#) html-font.el version 3.0 28-Jun-1995 U.Dickow
;; Keywords: languages HTML font-lock faces
;; See also: fort-font.el kumac-font.el html-helper-mode.el html-mode.el
;; See also URL: http://www.nbi.dk/~dickow/emacs/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; LCD Archive Entry:
;; html-font|Ulrik Dickow|dickow@nbi.dk|
;; Adds Font Lock support to HTML helper mode in Emacs.|
;; xx-Jul-1995|3.0|~/misc/html-font.el.Z|

;;; Commentary:

;; Sets up HTML helper mode for full support of font-lock-mode in Emacs.
;; Without something like this, font-lock-mode will only fontify strings.
;; Required: html-helper-mode.el (LCD or http://www.santafe.edu/~nelson/tools/)
;; However, this file can easily be adapted to e.g. M.Andreessen's html-mode.
;; Written for Emacs 19.27-29.
;; Doesn't work with XEmacs 19.10/11, but perhaps with 19.12?

;; Installation, currently:
;;   1) Put html-font.el in your load-path and `M-x byte-compile-file' it.
;;   2) Add this to your ~/.emacs (without the ";;" in front):
;;        (add-hook 'html-helper-load-hook '(lambda () (load "html-font")))
;; Customize font-lock as you would do for any other major mode.
;;   For Emacs 19.29, default colours are setup automatically.
;;   For earlier versions, you have to write a lot yourself (or get it from
;;   somewhere else, e.g. Simon Marshall's face-lock.el in the LCD; see also
;;   http://www.nbi.dk/~dickow/emacs/), for example:
;;   3) To have a colourful display with font lock in any supported major mode,
;;      you can add something like this to your ~/.emacs:
;;        (copy-face             'bold 'font-lock-keyword-face) ; or italic/...
;;        (set-face-foreground         'font-lock-keyword-face "ForestGreen")
;;        (setq font-lock-keyword-face 'font-lock-keyword-face)
;;      and similary for `font-lock-string-face' etc. (see font-lock.el).
;;   4) To have font-lock turned on every time html-helper-mode is entered do
;;        (add-hook 'html-helper-mode-hook '(lambda () (font-lock-mode 1)))
;;      If you don't want strings highlighted as such, then insert
;;        (make-local-variable font-lock-no-comments)
;;        (setq font-lock-no-comments t)
;;      in the hook (immediately before `(font-lock-mode 1)').
;; A brave XEmacs 19.12 user that wants to try this file should
;; define font-lock-{bold,italic,underline}-face, for example like this:
;;   (copy-face 'bold   'font-lock-bold-face)
;;   (copy-face 'italic 'font-lock-italic-face)
;;   (set-face-underline-p (make-face 'font-lock-underline-face) t)
;; For faster startup, you may want to use colours instead, like in 3).

;; This file will probably be incorporated into html-helper-mode.el later on.

;; Author's note: It should remain possible for the user to choose whether
;;   to highlight strings or not. The default may be changed, however (cf. the
;;   commented out statements in the hook in the bottom of this file).

;; Bugs: * Nested tags may not always be fontified the way you would expect.
;;   "strings" ("...") outside tags shouldn't inhibit bold/italic
;;   fontification, but it currently does (author doesn't care much).
;;   * It almost works for XEmacs 19.10/11, but with "screwy colours" 8-().

;;; Code:

(require 'html-helper-mode)  ; When we like to modify the syntax table
(require 'font-lock) ; New in 3.0, for better backwards compatibility (<19.29).

;; Code for greater flexibility, especially for XEmacs compatibility.
;; Note that Emacs evaluates the face entries in `font-lock-keywords',
;; while XEmacs doesn't.
(defvar font-lock-bold-face 'bold
  "Face used as bold.  Typically `bold' or `font-lock-bold-face'.")
(defvar font-lock-italic-face 'italic
  "Face used as italic.  Typically `italic' or `font-lock-italic-face'.")
(defvar font-lock-underline-face 'underline
  "Face used as underline.
Typically `underline' or `font-lock-underline-face'.")
;; The following face variable is introduced in Emacs 19.29's font-lock.el.
;; For Emacs <19.29 we fall back to the doc string face (obsoleted by 19.29).
(or (boundp 'font-lock-variable-name-face)
    (setq font-lock-variable-name-face 'font-lock-doc-string-face))

;; Let's reset syntax to make it possible for font-lock to colour strings.
;; They usually appear inside anchors; thus these may become two-coloured.
(modify-syntax-entry ?\" "\"   " html-helper-mode-syntax-table)

(defvar html-font-lock-keywords
  (let ((tword "\\(h1\\|title\\)")          ; Titles, like function defs
	(bword "\\(b\\|h[2-4]\\|strong\\)") ; Names of tags to boldify
	(iword "\\(address\\|cite\\|em\\|i\\|var\\)") ; ... to italify
	;; Regexp to match shortest sequence that surely isn't a bold end.
	;; We simplify a bit by extending "</strong>" to "</str.*".
	;; Do similarly for non-italic and non-title ends.
	(not-bend (concat "\\([^<]\\|<\\([^/]\\|/\\([^bhs]\\|"
			  "b[^>]\\|"
			  "h\\([^2-4]\\|[2-4][^>]\\)\\|"
			  "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
	(not-iend (concat "\\([^<]\\|<\\([^/]\\|/\\([^aceiv]\\|"
			  "a\\([^d]\\|d[^d]\\)\\|"
			  "c\\([^i]\\|i[^t]\\)\\|"
			  "e\\([^m]\\|m[^>]\\)\\|"
			  "i[^>]\\|"
			  "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
	(not-tend (concat "\\([^<]\\|<\\([^/]\\|/\\([^ht]\\|"
			  "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list
     ;; Comments: <!-- ... -->. They traditionally override string colouring.
     ;; It's complicated 'cause we won't allow "-->" inside a comment, and
     ;; font-lock colours the *longest* possible match of the regexp.
     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)" 1 font-lock-comment-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("\\(<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)"
       1 font-lock-comment-face keep)
     ;; Anchors, images & forms (again keep possible string colouring inside).
     '("\\(<a\\b[^>]*>\\)" 1 font-lock-keyword-face keep)
     "</a>"
     '("\\(<\\(form\\|i\\(mg\\|nput\\)\\)\\>[^>]*>\\)"
       1 font-lock-variable-name-face keep)
     ;; HTML special characters
     '("&[^;\n]*;" . font-lock-string-face)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 font-lock-underline-face keep)
     ;; '("<u>\\([^<]*\\)<" 1 ..underl..) '("\\(>[^<]*\\)</u>" 1 ..underl..)
     ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
     (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
	   2 'font-lock-function-name-face 'keep)
     ;; Large-scale structure keywords (like "program" in Fortran).
     ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
     '("</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)>"
       . font-lock-variable-name-face)
     ;; Any tag, general rule, just before bold/italic stuff.
     '("\\(<[^>]*>\\)" 1 font-lock-type-face keep)
     ;; More tag pairs (<b>...</b> etc.).
     ;; Cunning repeated fontification to handle common cases of overlap.
     ;; Bold simple --- first fontify bold regions with no tags inside.
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
	   2 'font-lock-bold-face 'keep)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside.
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
	   2 'font-lock-italic-face 'keep)
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
	   2 'font-lock-bold-face 'keep)))
  "Additional expressions to highlight in HTML helper mode.")

;; It shouldn't matter whether this hook is executed before or after font-lock.
;; Fortunately, font-lock.el is very friendly in this respect.
;; We must be equally friendly and make sure we don't make global defaults.
;; It can be done much more elegantly in Emacs 19.29+, but we'll keep it this
;; way to retain compatibility with older Emacsen.
(add-hook 'html-helper-mode-hook
 '(lambda ()
    (make-local-variable 'font-lock-keywords-case-fold-search)
    (make-local-variable 'font-lock-keywords)
;;  (make-local-variable 'font-lock-no-comments)
    ;; Regard the patterns in html-font-lock-keywords as case-insensitive
    (setq font-lock-keywords-case-fold-search t)
    (setq font-lock-keywords html-font-lock-keywords)))
;;  (setq font-lock-no-comments t)))

(provide 'html-font)

;;; html-font.el ends here
