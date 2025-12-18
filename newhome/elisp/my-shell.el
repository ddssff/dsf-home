(defun my-shell ()
  "Switch to an existing *shell* buffer with matching default directory,
or create a new one if none found."
  (interactive)
  (let* ((current-dir (expand-file-name default-directory))
         (shell-buffer (best-shell current-dir)))
    (if shell-buffer
        ;; If a matching shell buffer exists, switch to it
        (switch-to-buffer shell-buffer)
      ;; Otherwise, create a new shell buffer in the current directory
      (let ((default-directory current-dir))
        (shell)))))

(defun best-shell (child-path)
  "Return the buffer that is the best shell for a child path"
  (let* ((buffers (buffer-list))
	 ; Zip each buffers with its working directory
         (pairs (zip buffers (mapcar #'buffer-directory buffers)))
	 ; Filter out non-shells
	 (shells (seq-filter #'is-shell-buffer pairs))
	 ; Filter out non-parents
	 (parents (seq-filter (lambda (x) (is-prefix-of (cdr x) child-path)) shells))
	 ; The best shell is the parent with the longest path
	 (best (max-by (lambda (pair) (length (cdr pair))) parents)))
    (princ (format "here: %s, parents: %s" child-path parents))
    (car best)))

(defun is-prefix-of (prefix string)
  "Return t if PREFIX is a prefix of STRING, nil otherwise."
  (and (<= (length prefix) (length string))
       (string= prefix (substring string 0 (length prefix)))))

(defun max-by (key-function list)
  "Return the element from LIST for which KEY-FUNCTION returns the maximum value.
Comparison is done using `max` on the results of KEY-FUNCTION."
  (if (null list)
      nil ; Return nil for an empty list
    (let* ((mapped-values (mapcar key-function list))
           (max-value (apply 'max mapped-values))
           (max-index (cl-position max-value mapped-values))) ; Use `cl-position` from `cl-lib`
      (nth max-index list))))

;(defun overcdr (f pair)
;  (cons (car pair) (f (cdr pair)))

(defun zip (xs ys)
  (cond
   ((or (null xs) (null ys)) ())
   (t (cons (cons (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))

(defun buffer-directory (buffer)
  "Return the working directory of a buffer"
  (with-current-buffer buffer
     (expand-file-name default-directory)))

(defun is-shell-buffer (pair)
  "Return the working directory of a buffer"
  (with-current-buffer (car pair)
     (derived-mode-p 'shell-mode)))

(define-key esc-map "Z" 'my-shell)
