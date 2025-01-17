;; font-lock-hacks.el

; These are the faces:
;
;	font-lock-comment-face			this
;	font-lock-string-face			(light brown)
;	font-lock-keyword-face			(like defun)
;	font-lock-function-name-face		(like tofl)
;	font-lock-variable-name-face		orange/yellow
;	font-lock-type-face			
;	font-lock-reference-face		(like `foo-mode' below)
;
; Last edited:
; 
; Sat Aug 30 05:52:45 1997 by David Fox (fox at dsf.net)
; 	 Noted bug handling file names with spaces in dired mode.
;

(defun toggle-font-lock ()
  (interactive)
  (font-lock-mode (if (and (boundp 'font-lock-mode) font-lock-mode) 0 1)))

(defvar machine-exts
  (mapconcat
   (lambda (x) x)
   '("save"					;; General
     "a" "o" "so"				;; C, C++
     "cma" "cmi" "cmo" "cmx" "cmxa"		;; Ocaml
     "aux" "log" "lof" "cjk" "dvi" "ps" "toc"	;; TeX
     "bbl" "blg"				;; Bigloo?
     "class")					;; Java
   "\\|"))

(if (eq window-system 'x)
    (progn
      ;; (add-hook 'find-file-hooks 'turn-on-font-lock)
      (add-hook 'scheme-mode-hook 'turn-on-font-lock)
      (add-hook 'c++-mode-hook (lambda () (turn-on-font-lock)))
      (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
      (add-hook 'html-mode-hook 'turn-on-font-lock)
      (setq font-lock-maximum-decoration t)
      (setq font-lock-maximum-size
	    '((outline-mode . 1048576)
	      (latex-mode . 1048576)
	      (tuareg-mode . 4096)
	      (t . 256000)))

      ;; If you add patterns for a new mode, say foo.el's `foo-mode',
      ;; say in which you don't want syntactic fontification to occur,
      ;; you can make Font Lock mode use your regexps when turning on
      ;; Font Lock by adding to `foo-mode-hook':
      ;;
      ;;  (add-hook 'foo-mode-hook
      ;;   '(lambda () (make-local-variable 'font-lock-defaults)
      ;;               (setq font-lock-defaults '(foo-font-lock-keywords t))))
      ;;

      ;; Nasty regexps of the form
      ;; "bar\\(\\|lo\\)\\|f\\(oo\\|u\\(\\|bar\\)\\)\\|lo" are made
      ;; thusly: (make-regexp '("foo" "fu" "fubar" "bar" "barlo" "lo"))
      ;; for efficiency.  See
      ;; /pub/gnu/emacs/elisp-archive/functions/make-regexp.el.Z on
      ;; archive.cis.ohio-state.edu for this and other functions.

      ;; Bug - doesn't understand file names that contain spaces.

      (setq dired-font-lock-keywords
	    `(

	      ;; Lighten non-version-controlled files
	      ("^  .......... \\(precious\\|other\\|junk\\|backup\\)[ ]+[^ ]+[ ]+[^ ]+[ ]+[^ ]+[ ]+\\([^ ].*$\\)"
	       2 font-lock-string-face)

	      ;; Lighten non-version-controlled files
	      ("^  .......... \\(unrecognized\\)[ ]+[^ ]+[ ]+[^ ]+[ ]+[^ ]+[ ]+\\([^ ].*$\\)"
	       2 font-lock-comment-face)

	      ;; Put directory headers in italics.
	      ("^  \\(/.+\\)" 1 font-lock-type-face)

	      ;; Hilight symlinks
	      ("\\([^ ]+\\) -> [^ ]+$" . font-lock-type-face)

	      ;; Put marks in bold.
	      ("^[^ ]" . font-lock-reference-face)

	      ;; Put files that are subdirectories in bold.
	      ("^..d.* \\([^ ]+\\)$" 1 font-lock-keyword-face)

	      ;; Big files in a bright red face.
	      ;; ("^\\(....................................[1-9].*\\)"
	      ;; 1 font-lock-comment-face)

	      ;; Images in reference face (just for fun...)
	      ;; (" \\([^ ]*\\.\\([jm]pg\\|[JM]PG\\|gif\\|GIF\\|p[pbg]m\\)\\)$"
	      ;; 1 font-lock-reference-face)

	      ;; backup and machine generated files in string face
	      (,(concat "^.* \\("
			"\\([^ ]*[~#]\\)"
			"\\|"
			"\\([^ ]*\\.\\("
			machine-exts
			"\\)\\)"
			"\\)$")
			1 font-lock-string-face)
	      ;; (" \\([^ ]*\\.\\(o\\|aux\\|log\\|dvi\\|class\\)\\)$"
;	      (,(concat " \\([^ ]*"
;			 "\\(\\.\\(o\\|aux\\|log\\|dvi\\|class\\)\\)$"
;			 "\\|"
;			 "[~#]$"
;			 "\\)")
;	       1 font-lock-string-face)

	      ;; Executable files in variable name face
	      ("^.....x..[^s].* \\([^ ]*\\)$" 1 font-lock-variable-name-face)
	      ;; SUID and SGID executable files in comment face
	      (,(concat "^\\("
			".....[sS]"	; SUID executable
			"\\|"
			"........[sS]"	; SGID executable
			"\\)"
			".* \\([^ ]*\\)$")
	       2 font-lock-comment-face)
	      ))

      (add-hook 'dired-mode-hook
		'(lambda () (make-local-variable 'font-lock-defaults)
		   (setq font-lock-defaults '(dired-font-lock-keywords t))
		   (turn-on-font-lock)))

  (add-hook 'scheme-mode-hook
	    '(lambda ()
	       (load-library "scheme-font-lock")
	       (make-local-variable 'font-lock-defaults)
	       (turn-on-font-lock)))

;  (add-hook 'font-lock-mode-hook
;	    (function
;	     (lambda ()
;	       (if (eq major-mode 'java-mode)
;		   (progn
;		     (load-library "java-f-lck")
;		     (setq font-lock-keywords java-font-lock-keywords))))))

;  (defvar scheme-font-lock-keywords
;    (eval-when-compile
;      (list
;       ;;
;       ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> for
;       ;; STklos, scoops, meroon and sos.
;       (list 
;	(concat
;	 "^(\\(def\\(macro\\|ine\\("
;			       "\\|-\\(generic\\(\\|-method\\)"
;				   "\\|\\(method"
;				      "\\|generic"
;				      "\\|external"
;				      "\\|c-external"
;				      "\\|macro\\)\\)\\)\\)"
;	   "\\|\\(taboo-\\(method\\|generic\\|macro\\|delegator\\|module\\)\\)"
;	   "\\)"
;	   "\\>[ \t]*(?\\([^ \t\n\)]+\\)?")
;	'(1 font-lock-keyword-face)
;	'(3 font-lock-function-name-face nil t))
;       ;;
;       ;; STklos class specifiers as types.
;       '("<\\(\\sw+\\)>" 1 font-lock-type-face)
;       ;;
;       ;; Scheme `:' keywords as references.
;       '(":\\(\\sw+\\)" 1 font-lock-reference-face)
;       ;; Gambit Scheme `:' keywords as references.
;       '("\\(\\sw+\\):" 1 font-lock-reference-face)
;       ;;
;       ;; Control structures.
;       (cons
;	(concat
;	 "(\\("
;	 "begin\\|c\\(a\\(ll\\(-with-\\(current-continuation\\|"
;	 "input-file\\|output-file\\)\\|/cc\\)\\|se\\)\\|ond\\)\\|"
;	 "d\\(efmacro\\|efine-\\(macro\\|class\\|syntax\\)\\|o\\)\\|"
;	 "\\(taboo-class\\)\\|"
;	 "else\\|for-each\\|if\\|"
;	 "l\\(ambda\\|et\\(-syntax\\|\\*?\\|rec\\(\\|-syntax\\)\\)\\)\\|"
;	 "map\\|syntax\\(\\|-rules\\)"
;	 "\\)\\>") 1)))
;    "Default expressions to highlight in Scheme modes.")

  (defvar outline-font-lock-keywords
    '(;; Highlight headings according to the level.
      ("^\\([*%]+\\)\\([ ]*[0-9]*[ \t]*\\)\\([^\n\r]+\\)?[ \t]*[\n\r]"
       (1 font-lock-string-face)
       (2 font-lock-string-face)
       (3 (let ((len (- (match-end 1) (match-beginning 1))))
	    (or (cdr (assq len '((1 . font-lock-function-name-face)
				 (2 . font-lock-keyword-face)
				 (3 . font-lock-comment-face)
				 (4 . font-lock-variable-name-face)
				 (5 . font-lock-function-name-face)
				 (6 . font-lock-keyword-face)
				 (7 . font-lock-comment-face))))
		font-lock-variable-name-face))
	  nil t))
      ;; Highlight citations of the form [1] and [Mar94].
      ;("\\[\\([A-Z][A-Za-z]+\\)*[0-9]+\\]" . font-lock-type-face)
      ("\\(<<[^\n\r]*>>=\\)[\n\r]" . font-lock-type-face)
      ("\\(<<[^\n\r]*>>\\)" . font-lock-reference-face)
      )
    "Additional expressions to highlight in Outline mode.")

  ; But I haven't created an RPM spec file mode yet...

  (defvar rpm-spec-font-lock-keywords
    '(;; Highlight the RPM spec file tags
       ("^%.*$" . font-lock-type-face)
       ("^Description:" . font-lock-keyword-face)
       ("^Group:" . font-lock-keyword-face)
       ("^Version:" . font-lock-keyword-face)
       ("^Release:" . font-lock-keyword-face)
       ("^Requires:" . font-lock-function-name-face)
       ("^Source" . font-lock-variable-name-face)
       ("^Patch" . font-lock-variable-name-face)
       ("[ \t]*#.*" . font-lock-comment-face))
    "Additional expressions to highlight in rpm spec file mode.")

  ; My Scheme mode definitions.

  (defconst scheme-font-lock-keywords-1
    (eval-when-compile
      (list
       ;;
       ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
       ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
       (list (concat "\\(define"
		      "\\("
		          ;; Function names.
		       "\\(\\|-"
		        "\\(generic\\(\\|-procedure\\)\\|"
		           "method\\|in-line\\|constant\\|external\\|module\\|"
			   "c-external"
		         "\\)"
			"\\)\\|"
		       ;; Macro names, as variable names.  A bit dubious, this.
		        "\\(-syntax\\|-macro\\)\\|"
		     ;; Class names.
		     "-class"
		     "\\)\\)\\>"
		     ;; Any whitespace and declared object.
		     "[ \t]*(?"
		     "\\(\\sw+\\)?")
	     '(1 font-lock-keyword-face)
	     '(7 (cond ((match-beginning 3) font-lock-function-name-face)
		       ((match-beginning 6) font-lock-variable-name-face)
		       (t font-lock-type-face))
		 nil t)
	     )
       ))
    "Subdued expressions to highlight in Scheme modes.")

  (defconst scheme-font-lock-keywords-2
    (append 
     scheme-font-lock-keywords-1
     (eval-when-compile
       (list
	;;
	;; Control structures.
	(cons
	 (concat
	  "(" (regexp-opt
	       '("begin" "call-with-current-continuation" "call/cc"
		 "call-with-input-file" "call-with-output-file" "case" "cond"
		 "do" "else" "for-each" "if" "lambda"
		 "let" "let*" "let-syntax" "letrec" "letrec-syntax"
		 ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
		 "and" "or" "delay"
		 ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
		 ;;"quasiquote" "quote" "unquote" "unquote-splicing"
		 "map" "syntax" "syntax-rules") t)
	  "\\>") 1)
	;;
	;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
	'("\\<<\\sw+>\\>" . font-lock-type-face)
	;;
	;; Scheme `:' keywords as references.
	'("\\<:\\sw+\\>" . font-lock-reference-face)
	)))
  "Gaudy expressions to highlight in Scheme modes.")

  (defvar scheme-font-lock-keywords scheme-font-lock-keywords-2
    "Default expressions to highlight in Scheme modes.")

  (if (string< "19" emacs-version) (global-set-key [f12] 'toggle-font-lock))

  )
  )

(setq font-lock-maximum-decoration t)

;(custom-set-faces)

(if (and (boundp 'window-system) window-system)
    (if (string-match "XEmacs" emacs-version)
	(require 'sym-lock)
      (require 'font-lock)))

