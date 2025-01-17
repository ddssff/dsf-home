(defun buffer-menu-next-window (&optional arg)
  "Display the Buffer Menu in another window.
See `buffer-menu' for a description of the Buffer Menu.

By default, all buffers are listed except those whose names start
with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files."
  (interactive "P")
  (switch-to-buffer-next-window (list-buffers-noselect arg))
  (message
   "Commands: d, s, x, u; f, o, 1, 2, m, v; ~, %%; q to quit; ? for help."))

(setq my-switch-to-buffer-alist t)

(define-key global-map (kbd "C-x C-b") 'buffer-menu-next-window)

(defun switch-to-buffer-next-window (buffer-or-name &optional norecord)
  "Select the buffer specified by BUFFER-OR-NAME in next-window.
BUFFER-OR-NAME may be a buffer, a string (a buffer name), or
nil.  Return the buffer switched to.

If called interactively, prompt for the buffer name using the
minibuffer.  The variable `confirm-nonexistent-file-or-buffer'
determines whether to request confirmation before creating a new
buffer.

If BUFFER-OR-NAME is a string and does not identify an existing
buffer, create a new buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

Optional second argument NORECORD non-nil means do not put this
buffer at the front of the list of recently selected ones.

This uses the function `display-buffer' as a subroutine; see its
documentation for additional customization information."
  (interactive
   (list (my-read-buffer-to-switch "Switch to buffer in other window: ")))
  (let ((pop-up-windows t))
    (pop-to-buffer buffer-or-name my-switch-to-buffer-alist norecord)))

(defun display-buffer-next-window (buffer alist)
  "display-buffer-same-window with a next-window added"
  (unless (or (cdr (assq 'inhibit-same-window alist))
	      (window-minibuffer-p)
	      (window-dedicated-p))
    (message "this-window: %s" (selected-window))
    (message "next-window: %s" (next-window (selected-window)))
    (window--display-buffer buffer (next-window (selected-window)) 'reuse alist)))

;(defun display-buffer-next-window (buffer alist)
;  "Display BUFFER in the window returned by next-window.
;Search for a usable window, set that window to the buffer, and
;return the window.  If no suitable window is found, return nil.
;
;If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
;event that a window in another frame is chosen, avoid raising
;that frame."
;  (let* ((window (next-window))
;	 (quit-restore (and (window-live-p window)
;			    (window-parameter window 'quit-restore)))
;	 (quad (nth 1 quit-restore)))
;    (when (window-live-p window)
;      ;; If the window was used by `display-buffer' before, try to
;      ;; resize it to its old height but don't signal an error.
;      (when (and (listp quad)
;		 (integerp (nth 3 quad))
;		 (> (nth 3 quad) (window-total-height window)))
;	(condition-case nil
;	    (window-resize window (- (nth 3 quad) (window-total-height window)))
;	  (error nil)))
;
;      (prog1
;	  (window--display-buffer buffer window 'reuse alist)
;	(window--even-window-sizes window)
;	(unless (cdr (assq 'inhibit-switch-frame alist))
;	  (window--maybe-raise-frame (window-frame window)))))))

; Haven't tried this yet
;(defun my-list-buffers (&optional arg)
;  "Display a list of existing buffers.
;The list is displayed in a buffer named \"*Buffer List*\".
;See `buffer-menu' for a description of the Buffer Menu.
;
;By default, all buffers are listed except those whose names start
;with a space (which are for internal use).  With prefix argument
;ARG, show only buffers that are visiting files."
;  (interactive "P")
;  (display-buffer-in-previous-window (list-buffers-noselect arg)))

;(define-key global-map (kbd "C-x C-b") 'buffer-menu-other-window)

;; Configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `(;; Open shell in a single window
        (,(rx bos "*shell")
         (display-buffer-same-window)
         (reusable-frames . nil))
        (,(rx bos "*grep")
         (display-buffer-next-window)
         (reusable-frames . nil))
        (,(rx bos "*vc")
         (display-buffer-next-window)
         (reusable-frames . nil))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))
	))
