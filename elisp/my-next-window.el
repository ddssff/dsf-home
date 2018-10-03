(defun buffer-menu-next-window (&optional arg)
  "Display the Buffer Menu in another window.
See `buffer-menu' for a description of the Buffer Menu.

By default, all buffers are listed except those whose names start
with a space (which are for internal use).  With prefix argument
ARG, show only buffers that are visiting files."
  (interactive "P")
  (switch-to-buffer-other-window (list-buffers-noselect arg))
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
