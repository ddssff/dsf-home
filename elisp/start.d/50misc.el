;; misc.el

;;;;;;;;;;;;;; Some Functions ;;;;;;;;;;;;;; 



;;
;; From cmu's comint.el -- This should be in emacs, but it isn't.
;;

;(defun mem (item list &optional elt=)
;  "Test to see if ITEM is equal to an item in LIST.
;Option comparison function ELT= defaults to equal."
;  (let ((elt= (or elt= (function equal)))
;	(done nil))
;    (while (and list (not done))
;      (if (funcall elt= item (car list))
;	  (setq done list)
;	  (setq list (cdr list))))
;    done))

; Switches between certain sets of major modes.

(defun toggle-modes ()
  (interactive)
  (cond ((string-equal mode-name "Outline") (latex-mode))
	((string-equal mode-name "LaTeX") (outline-mode))
	(t (message "No next mode."))))

(defun toggle-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (shrink-window 0))			; Make truncate-lines value take effect

(defun toggle-case-fold-search()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (if case-fold-search
      (message "folding case")
    (message "case sensitive")))

;(setq-default case-fold-search nil)

(defun insert-date ()
  "Insert current date in abbreviated format."
  (interactive "*")
  (let* ((now (current-time-string))	;With ugly-printed timestamp
	 (firstchar (string-to-char (substring now 8 9))))
    (if (/= firstchar 32) (insert-char firstchar 1))
    (insert (substring now 9 10) " "	;Insert day of month
	    (substring now 4 7) " "	;Abbreviated name of month
	    (substring now 20 24))))	;Full year number

(defun insert-changelog ()
  "Insert changelog header."
  (interactive "*")
  (let* ((now (current-time-string))	;With ugly-printed timestamp
	 ;(firstchar (string-to-char (substring now 8 9)))
	 )
    ;(if (/= firstchar 32) (insert-char firstchar 1))
    (insert "* " 
	    (substring now 0 4)		;Insert day of week
	    (substring now 4 11)	;Insert month name and day of month
	    (substring now 20 24) " "	;Insert year
	    (user-full-name) " <"
	    (user-login-name) "@" (system-name) ">\n\n")))

(defun insert-date-and-time ()
  "Insert current date and time in abbreviated format."
  (interactive "*")
  (insert-date)
  (insert (substring (current-time-string) 10 16)))
