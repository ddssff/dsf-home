;; bounce.el

;; Have M-H bounce between related files with the same prefix.

; If the current extension matches the key, look for files with the
; extensions given in the associated list, in the order given.  If
; none are found create the file with the first extension after the key.

(defvar bounce-alist '((".h" ".i" ".C" ".cc" ".c")
		       (".i" ".C" ".cc" ".c" ".h")
		       (".C" ".h" ".i")
		       (".cc" ".h" ".i")
		       (".scm" ".sch")
		       (".sch" ".scm")
		       (".ml" ".mli")
		       (".c" ".h" ".i")))

(defun bounce-between-exts ()
  (interactive)
  (string-match "\\.[^.]*$" "f.oo.h")
  ; Find the location of the file's extension
  (let ((index (string-match "\\.[^.]*$" buffer-file-name)))
    (if index
	; Find the extension and look up the list of next file extensions
	(let* ((base (substring buffer-file-name 0 index))
	       (ext (substring buffer-file-name index))
	       (next (assoc ext bounce-alist)))
	  (if next
	      (find-file-with-extension base (cdr next) (car (cdr next)))
	    (message (concat "File " base (cdr next) (car (cdr next)) " not found"))
	    )))))

(defun find-file-with-extension (base exts dflt)
  (if (null exts)
      (find-file (concat base dflt))
      (if (file-exists-p (concat base (car exts)))
	  (find-file (concat base (car exts)))
	(find-file-with-extension base (cdr exts) dflt))))

(define-key esc-map "H" 'bounce-between-exts)
