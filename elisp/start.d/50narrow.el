(defun forward-narrowed-page (num)
  (interactive "p")
  (beginning-of-buffer)	; Not sure this is necessary.
  (narrow-to-page num)
; (beginning-of-buffer)
  )

; In emacs-19.29 and 30, when we're at the beginning of the buffer
; (that is, (point) equals (point-min)) doing (narrow-to-page -1) will
; take us back two pages, while (narrow-to-page 0) keeps us on this
; page.  To work around this, we first go to the end of the page.
; If the page is empty this still doesn't work.

(defun backward-narrowed-page (num)
  (interactive "p")
  (beginning-of-buffer)
  (let ((start (point)))
    (narrow-to-page (- num))
    (beginning-of-buffer)
    (let ((mid (point)))
      (beginning-of-buffer)
      (if (> start (point)) (narrow-to-page 1))
      (if (= start (point)) (narrow-to-page -1))
      (beginning-of-buffer))))
