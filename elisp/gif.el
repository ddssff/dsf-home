;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gif.el
;;
;; Written by Art Mellor @ Cayman Systems, Inc. 1991
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Pull (possibly) multi-part GIF's out of gnus
;;;;
;; Last Edited:
;; 
;; Tue Aug 13 10:52:41 1996 by David Fox (fox at maki.net)
;; 	 Added code to recognize base64 files to
;; 	 gnus-grab-gif-is-self-contained.  See also uu.el.

;; Assemble all the parts of a gif and decode to file
(defun gnus-grab-gif ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((buffer (generate-new-buffer "*gif parts*"))
	  (id-string (get-message-id-string))
	  (num-parts (get-number-of-parts))
	  (part-num 1)
	  (part-missing nil)
	  (start (point)))


      ;; if you can't parse the subject line, see if it is a one-parter
      (if (or (not id-string) (not num-parts))
	  (if (gnus-grab-gif-is-self-contained)
	      (gnus-grab-gif-one-part)
	    (error "Sorry, can't parse this subject line!"))
	(progn 
	  (setq part-missing (part-missing-p id-string num-parts))
	  (if part-missing
	      (progn
		(kill-buffer buffer)
		(error (concat "Couldn't find part "
			       (int-to-string part-missing)))))

	  (while (<= part-num num-parts)
	    (goto-char (point-min))
	    ;; If the parts are numbered 01/10, then chop off the leading 0
	    ;; when the partnum gets over 9
	    (if (= part-num 10)
		(let ((len (length id-string)))
		  (if (equal (substring id-string (- len 1) nil) "0")
		      (setq id-string
			    (substring id-string 0 (- len 1))))))
	    (if (not (search-forward
		      (concat id-string
			      (int-to-string part-num)) nil t))
		(progn
		  (kill-buffer buffer)
		  (goto-char start)
		  (error "Couldn't find part %d" part-num)))
	    (message "Grabbing part %d of %d" part-num num-parts)
	    (gnus-summary-display-article (gnus-summary-article-number))
	    (save-excursion
	      (set-buffer "*Article*")
	      (append-to-buffer buffer (point-min) (point-max)))
	    (setq part-num (+ part-num 1)))
	  (switch-to-buffer buffer)
	  (uudecode-buffer (= num-parts 1))
	  (kill-buffer buffer))))))

;; Check to see if current article is self-contained
(defun gnus-grab-gif-is-self-contained ()
  (interactive)
  (save-excursion
    (let ((result nil))
      (gnus-summary-display-article (gnus-summary-article-number))
      (other-window 1)
      (beginning-of-buffer)
      (if (re-search-forward "^begin [0-7]*" nil t)
	  (setq result (re-search-forward "^end" nil t))
	(if (re-search-forward "^/9j/")
	    (setq result t)))
      (other-window 1)
      result)))

;; Grab a single part gif with a non-formatted subject header
(defun gnus-grab-gif-one-part ()
  (interactive)
  (save-excursion
    (gnus-summary-display-article (gnus-summary-article-number))
    (other-window 1)
    (call-interactively 'uudecode-buffer-no-merge)
    (other-window 1)))

;; *** how do you pass a universal arg to a function along
;; with call-interactively?
(defun uudecode-buffer-no-merge ()
  (interactive)
  (uudecode-buffer t))

;; Return the region as a string
(defun region-to-string (begin end)
  (buffer-substring (min begin end) (max begin end)))

;; This regular expression controls what types of subject lines can be
;; parsed. Currently handles lines like:
;;	foo [1/3]
;;	foo (1/3)
;;	foo 1/3
;;	foo [1 of 3]
;;	foo (1 of 3)
;;	foo 1 of 3
;;	foo1 of 3
;;
(defvar id-end-regexp "[0-9]+\\(|\\|/\\| [oO][fF] \\)")

;; Check if all the parts are there
(defun part-missing-p (id-string num-parts)
  (save-excursion
    (let ((part-num 1)
	  (cant-find nil))

      (while (and (<= part-num num-parts) (not cant-find))
	(goto-char (point-min))
	;; If the parts are numbered 01/10, then chop off the leading 0
	;; when the partnum gets over 9
	(if (= part-num 10)
	    (let ((len (length id-string)))
	      (if (equal (substring id-string (- len 1) nil) "0")
		  (setq id-string
			(substring id-string 0 (- len 1))))))
	(if (not (search-forward
		  (concat id-string
			  (int-to-string part-num)) nil t))
	    (setq cant-find part-num)
	  (progn
	    (message "Found part %d of %d" part-num num-parts)
	    (setq part-num (+ part-num 1)))))
      cant-find)))

;; Return a string to identify this thread of messages
(defun get-message-id-string ()
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      (re-search-forward "] " nil t)
      (let ((start (point))
	    (result nil))
	(if (re-search-forward id-end-regexp end t)
	    (progn 
	      (re-search-backward "[0-9]+" nil t)
	      (setq result (region-to-string start (point)))))
	result))))
			  

;; How many parts to this picture?
(defun get-number-of-parts ()
  (save-excursion
    (end-of-line)
    (let ((end (point))
	  (result t))
      (beginning-of-line)
      (cond ((not (re-search-forward id-end-regexp end t))
	     (setq result nil))
	    ((not (re-search-forward "[0-9]+" end t))
	     (setq result nil))
	    (t
	     (setq result
		   (string-to-int (region-to-string (match-beginning 0)
						    (match-end 0))))))
      result)))

