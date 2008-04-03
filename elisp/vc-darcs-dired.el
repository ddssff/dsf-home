(require 'vc-darcs)
(require 'dired)
(require 'dired-aux)

(defun drop-prefix (prefix string)
  (cond ((> (length prefix) (length string)) string)
	((string= prefix (substring string 0 (length prefix)))
	 (substring string (length prefix)))
	(t string)))

(defun vc-darcs-dired-whatsnew ()
  "Run \"darcs whatsnew -u\"."
  (interactive)
  (vc-darcs-command "whatsnew" nil 1 (dired-current-directory) "-u")
  (with-current-buffer "*vc*" (goto-char 0))
  (display-buffer "*vc*"))

(defun vc-darcs-dired-print-log (arg)
  "Run \"darcs changes\"."
  (interactive "P")
  (if arg
      (vc-darcs-command "changes" nil 0 nil "--verbose")
    (vc-darcs-command "changes" nil 0 nil))
  (with-current-buffer "*vc*" (goto-char 0))
  (display-buffer "*vc*"))

(defun vc-darcs-dired-add-file ()
  "Run \"darcs add\" on current file."
  (interactive)
  (vc-darcs-command "add" nil 0 (dired-get-filename))
  (dired-relist-entry (dired-get-filename)))

(defun vc-darcs-dired-remove-file ()
  "Run \"darcs remove\" on current file."
  (interactive)
  (vc-darcs-command "remove" nil 0 (dired-get-filename))
  (dired-relist-entry (dired-get-filename)))

(defvar darcs--dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "tl" 'vc-darcs-dired-print-log)
    (define-key map "ta" 'vc-darcs-dired-add-file)
    (define-key map "td" 'vc-darcs-dired-remove-file)
    (define-key map "t=" 'vc-darcs-dired-whatsnew)
    map))

(defvar darcs-dired-do-changes nil
  "If this is true, run 'tla changes' and integrate the result
into the dired output.  This may take some time.")

(defvar darcs-dired-changes-list nil
  "List of changes for the directory.  This is made buffer local
before it is modified.")

(defun darcs--set-mode-line ()
  (setq mode-name "DARCS"))

(defun darcs--make-tag (file elem changed)
  ;(message (format "elem: %S" elem))
  (let* ((typ (cdr elem)))
    (cond ((string= file "_darcs") "control")
	  ((equal typ '("a")) "precious")
	  ((equal typ '("A")) "added")
	  ((equal typ '("R")) "removed")
	  ((equal typ '("M")) "modified")
	  ((null typ) "")
	  (t (concat "unknown: " (prin1-to-string typ))))))

(defun darcs--inventory (directory)
  (let* ((buffer (get-buffer-create "*vc*"))
	 (manifest
	  (progn
	    (with-current-buffer buffer (erase-buffer))
	    (vc-darcs-command "query" nil 0 nil "manifest")
	    (with-current-buffer buffer (remove-if (lambda (x) (or (string= x "") (string= x "No changes!")))
						   (split-string (buffer-string) "\n")))))
	 (whatsnew
	  (progn
	    (with-current-buffer buffer (erase-buffer))
	    (vc-darcs-command "whatsnew" nil 1 nil "-l" "-s" "--boring")
	    (with-current-buffer buffer
	      (mapcar (lambda (s)
			(let* ((name (substring s 2))
			       (end (string-match "[0-9+-][ 0-9+-]+$" name))
			       (name2 (if (null end) name (substring name 0 (- end 1))))
			       (end2 (string-match "/$" name2))
			       (name3 (if (null end2) name2 (substring name2 0 end2))))
			  (list name3 (substring s 0 1)))) (remove-if (lambda (x) (or (string= x "") (string= x "No changes!")))
								      (split-string (buffer-string) "\n")))))))
    ;; Merge the manifest and whatsnew to get an inventory
    (merge-inventory whatsnew manifest)
    ))

(defun merge-inventory (whatsnew manifest)
  (cond ((null manifest) whatsnew)
	((null (assoc (car manifest) whatsnew))
	 (merge-inventory (cons (list (car manifest)) whatsnew) (cdr manifest)))
	(t (merge-inventory whatsnew (cdr manifest)))))	 

;(file-name-directory (directory-file-name (vc-darcs-_darcs-dir (dired-current-directory))))

(defun darcs--dired-hook ()
  "This hook is called every time a file or directory is read, with
the buffer narrowed to the affected listings.  The function reformats
the listings to reflect arch version control"
  (message "Getting directory DARCS info ... ")
  (darcs--set-mode-line)
  (use-local-map darcs--dired-mode-map)
  (let* ((directory (dired-current-directory))
;	 (inventory-alist
;	  (mapcar 
;	   (lambda (elem) (cons (nth 2 elem) elem))
;	   (darcs--inventory directory)))
	 (_darcs (vc-darcs-_darcs-dir directory))
	 (top (file-name-directory (directory-file-name _darcs)))
	 (subdir (concat "./" (drop-prefix top directory)))
	 (inventory-alist (darcs--inventory directory))
	 (changes (if darcs-dired-do-changes (darcs--changes) darcs-dired-changes-list)))
    (if darcs-dired-do-changes
	(set (make-local-variable 'darcs-dired-changes-list) changes))
    (toggle-read-only -1)
    (goto-char 0)
    (dired-goto-next-file)
    (while (dired-move-to-filename)
      (darcs--edit-dired-line top subdir inventory-alist changes)
      (dired-next-line 1))
    (toggle-read-only 1)
    (message "Getting directory DARCS info ... done")
    ))

(defun darcs--edit-dired-line (top subdir inventory-alist changes)
  (let* ((file (dired-get-filename 'no-dir))
	 (elem (assoc (concat subdir file) inventory-alist))
	 (changed (assoc file changes))
	 (mark (darcs--make-tag file elem changed)))
    (beginning-of-line)
    (forward-char 12)
    (let* ((beg (point))
	   (end (- (re-search-forward "[ ]+[^ ]+[ ]+[^ ]+[ ]+[^ ]+[ ]") 1))
	   (fmt (format "%%-%ds" (- end beg))))
      (delete-region beg (- (point) 1))
      (insert (format fmt (or mark ""))))))

(define-derived-mode darcs-dired-mode
  dired-mode "DARCS"
  "Major mode derived from dired-mode for managing DARCS project
directories.  Several normal bindings are overridden, and other
bindings starting with 't' are added (see below.)

\\{darcs--dired-mode-map}"
  ;(message "entering darcs-dired-mode")
  (set-keymap-parent darcs--dired-mode-map dired-mode-map)
  (make-local-hook 'dired-after-readin-hook)
  (add-hook 'dired-after-readin-hook 'darcs--dired-hook nil t)
  (setq darcs-dired-mode t)
  ;(message "finished darcs-dired-mode")
  )

(defun darcs--dired-before-readin-hook ()
  "If this is a darcs directory prepare to go into DARCS mode."
  (cond ((and (vc-darcs-_darcs-dir default-directory)
	      (not (string= major-mode 'darcs-dired-mode)))
	 (setq dired-listing-switches "-l")
	 (darcs-dired-mode))))

(defun darcs--look-for-buffer (path)
  "Avoid creating new buffers for a directory when one already exits."
  (dired-find-buffer-nocreate (file-name-as-directory path) 'darcs-dired-mode))

(if (not (memq 'darcs--look-for-buffer find-directory-functions))
    (setq find-directory-functions
	  (cons 'darcs--look-for-buffer find-directory-functions)))

(add-hook 'dired-before-readin-hook 'darcs--dired-before-readin-hook nil nil)
