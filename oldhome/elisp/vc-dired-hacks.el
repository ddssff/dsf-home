;; Modify dired-noselect so we run vc-directory instead of dired when
;; we see a CVS subdirectory.

(load-library "dired")

;; Modified to run vc-directory when CVS subdirectory exists.

(defun dired-noselect (dir-or-list &optional switches)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dir-or-list (setq dir-or-list default-directory))
  ;; This loses the distinction between "/foo/*/" and "/foo/*" that
  ;; some shells make:
  (let (dirname initially-was-dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq initially-was-dirname
	  (string= (file-name-as-directory dirname) dirname))
    (setq dirname (abbreviate-file-name
		   (expand-file-name (directory-file-name dirname))))
    (if find-file-visit-truename
	(setq dirname (file-truename dirname)))
    ;; If the argument was syntactically  a directory name not a file name,
    ;; or if it happens to name a file that is a directory,
    ;; convert it syntactically to a directory name.
    ;; The reason for checking initially-was-dirname
    ;; and not just file-directory-p
    ;; is that file-directory-p is slow over ftp.
    (if (or initially-was-dirname (file-directory-p dirname))
	(setq dirname  (file-name-as-directory dirname)))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list)))
      (setq dir-or-list dirname))
    (if (file-directory-p (concat dirname "CVS"))
	(vc-directory dirname switches)
      (dired-internal-noselect dir-or-list switches))))

;; Modified to ignore mode argument.  VC buffers will be mode
;; vc-dired-mode, rather than just dired-mode.

(defun dired-find-buffer-nocreate (dirname &optional mode)
  ;; This differs from dired-buffers-for-dir in that it does not consider
  ;; subdirs of default-directory and searches for the first match only.
  ;; Also, the major mode must be MODE.
  (let (found (blist dired-buffers))    ; was (buffer-list)
    (while blist
      (if (null (buffer-name (cdr (car blist))))
	  (setq blist (cdr blist))
	(save-excursion
	  (set-buffer (cdr (car blist)))
	  (if (or (and (consp dired-directory)
		       (equal (car dired-directory) dirname))
		  (equal dired-directory (file-name-as-directory dirname)))
	      (setq found (cdr (car blist))
		    blist nil)
	    (setq blist (cdr blist))))))
    found))

;; Modified to do nothing if switches argument matches current
;; switches.

(defun dired-sort-other (switches &optional no-revert)
  ;; Specify new ls SWITCHES for current dired buffer.  Values matching
  ;; `dired-sort-by-date-regexp' or `dired-sort-by-name-regexp' set the
  ;; minor mode accordingly, others appear literally in the mode line.
  ;; With optional second arg NO-REVERT, don't refresh the listing afterwards.
  (if (not (equal switches dired-actual-switches))
      (progn
	(setq dired-actual-switches switches)
	(if (eq major-mode 'dired-mode) (dired-sort-set-modeline))
	(or no-revert (revert-buffer)))))

(custom-set-variables
 '(vc-dired-recurse nil)
 '(vc-dired-terse-display nil))

(message "vc-hacks loaded")
