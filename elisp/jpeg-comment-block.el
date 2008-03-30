; Edit jpeg file comment block.  Adapted from history.el.

(defvar jpeg-buffer "*JPEG Comment block*"
  "*The buffer name to edito the comment block text in.")

(defvar jpeg-file-name ""
  "*The name of the jpeg file being edited.")

(defvar jpeg-mode-map nil)

(if jpeg-mode-map
    nil
  (setq jpeg-mode-map (make-sparse-keymap))
  ;(define-key jpeg-mode-map "\C-c?"    'jpeg-help)
  ;(define-key jpeg-mode-map "\C-c\C-a" 'jpeg-abort)
  (define-key jpeg-mode-map "\C-c\C-c" 'jpeg-exit)
  (define-key jpeg-mode-map "\C-x\C-s" 'jpeg-exit)
)

(defun edit-jpeg-comment (file-name)
  (interactive)
  (jpeg-update-last-edit file-name)
;  (jpeg-update-last-change)
;  (jpeg-update-last-update)
  nil)

(defun jpeg-update-last-edit (file-name)
  (save-window-excursion
   (pop-to-buffer jpeg-buffer t)
   (erase-buffer)
   (setq jpeg-file-name file-name)
    (shell-command (concat "rdjpgcom " file-name) t)
    (set-buffer-modified-p nil)
    (use-local-map jpeg-mode-map)
    (message "Edit comment block.  Type C-c C-c when done.")
    (recursive-edit)
    (kill-buffer jpeg-buffer)))

(defun jpeg-exit (&optional arg)
  "Leave the recursive edit of an history log message."
  (interactive "P")
  (if (buffer-modified-p)
      (progn
	(shell-command (concat "rm -f " jpeg-file-name "~"))
	(shell-command (concat "mv " jpeg-file-name " " jpeg-file-name "~"))
	(shell-command
	 (concat "wrjpgcom -replace -comment \""
		 (buffer-substring (point-min) (point-max))
		 "\" < " jpeg-file-name "~" " > " jpeg-file-name))))
  (exit-recursive-edit))
