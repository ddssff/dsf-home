(setq load-path (cons (concat (getenv "HOME") "/elisp") load-path))

(defvar flavor 
  (make-symbol (concat "emacs" (int-to-string emacs-major-version)))
  "Apparantly it is debian emacs policy that this variable contain
the a symbol whose name is the name of the numbered emacs directory
in /usr/share.  Whatever.")
