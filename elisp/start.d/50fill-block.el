;; fill-block.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;			Fill block with prefix				;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find the prefix yourself, and (2) it doesn't get confused by adjacent
;; non-prefixed lines.  Useful for block comments (like this one) as
;; well as quoted regions in news messages.

(defun un-line-start (string)
  "Remove uparrow from start of regular expression."
  (cond ((not (stringp string)) string)
        ((= (string-to-char string) ?^) (substring string 1))
        (t string)))

(defun fill-block (arg)
  "Fill lines surrounding this one having the same start text.
With argument, only fills paragraph; with no argument, all lines."
  (interactive "*P")
  (save-excursion
    (beginning-of-line)
    (let ((bol (point)))
      (skip-chars-forward "^a-zA-Z0-9\n")
      (if (bolp) (progn (skip-chars-forward "a-zA-Z0-9")
			(skip-chars-forward "> ")))
      (if (bolp) (error "No block marker at start of line"))
      (let* ((fill-prefix (buffer-substring bol (point)))
             (pref-quote (regexp-quote fill-prefix))
             (pref-stop (if arg
                            (concat pref-quote
                                    (un-line-start paragraph-separate))
			  (concat pref-quote
				  (un-line-start paragraph-separate))
                          "X"))
             (pref-start (if arg
                             (concat pref-quote
                                     (un-line-start paragraph-start))
                           "X" )))
        (while (progn (forward-line 1)  ;Skip fwd over whole block
                      (and (looking-at pref-quote)
                           (not (looking-at pref-stop)))))
        (let ((block-end (point)))
          (while (and (not (bobp))      ;Stopping at start of buffer
                      (progn
                        (forward-line -1) ;Backwd over block
                        (if (looking-at pref-quote) ;Check if still matches
                            (not (looking-at pref-start)) ;yes, continue?
                          (forward-line 1) ;no, move past mismatch and stop
                          nil))))
          (fill-region (point) block-end))))))
