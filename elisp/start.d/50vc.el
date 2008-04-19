(custom-set-variables
 '(vc-dired-recurse nil)
 '(vc-diff-switches "-b")
 '(vc-cvs-stay-local t)
 '(vc-dired-terse-display 'vc)
 '(vc-cvs-diff-switches "-u"))

(defun vc-dired-recurse-toggle ()
  (interactive)
  (setq vc-dired-recurse (not vc-dired-recurse))
  (vc-directory dired-directory nil))

(global-set-key [f8] 'vc-dired-recurse-toggle)
;(add-to-list 'vc-handled-backends 'SVN)
(require 'psvn)

(defun svn-dired-hook ()
  (if (file-exists-p (concat default-directory "/.svn"))
      (svn-status default-directory)))

(add-hook 'dired-before-readin-hook 'svn-dired-hook)
