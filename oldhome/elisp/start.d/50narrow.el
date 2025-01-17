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

(if window-system
    (progn
      (global-set-key [f11] 'toggle-modes)
      (global-set-key [f9] 'backward-narrowed-page)
      (global-set-key [f10] 'forward-narrowed-page))
  (global-unset-key "\e[")
  (define-key esc-map "[20~" 'backward-narrowed-page)	;; L9 via telnet
  (define-key esc-map "[21~" 'forward-narrowed-page))	;; L10 via telnet
