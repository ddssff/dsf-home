;; DIRED HACKS:
;;   - make "q" find parent directory
;;   - make "." set a new tags file
;;   - Some key mappings

(defun dired-parent ()
  "This function runs dired on the parent of the current file.
If there is no associated filename, it finds the parent of (pwd)."
  (interactive)
  (let ((dirname buffer-file-name)
	(filename buffer-file-name)
	(basename)
	)
    ; Figure out what directory we're looking for
    (if (eq (cdr (assq 'major-mode (buffer-local-variables))) 'dired-mode)
	(setq filename (cdr (assq 'dired-directory
				  (buffer-local-variables)))))
    (if (eq (length dirname) 0)
	(setq dirname (substring (pwd) 10)))
    
    (if (string-equal (substring dirname 0 2) "~/")
	(progn
	  (setq dirname (and dirname (expand-file-name dirname)))
	  (setq filename (and filename (expand-file-name filename)))))

    (if (not (string-equal dirname "/"))
	(progn
	  (setq dirname (directory-file-name dirname))
	  (while (not (string-equal (substring dirname -1) "/"))
	    (setq basename (concat (substring dirname -1) basename))
	    (setq dirname (substring dirname 0 -1)))))

    ; Get that directory
    (find-file (directory-file-name dirname))

    ; Position the cursor
    ;(message (concat "dirname: " dirname))
    ;(message (concat "filename: " filename))
    ;(message (concat "basename: " basename))
    (let ((name (concat dirname basename)))
      (if (not (eq (length name) 0))
	  (dired-goto-file name)))
    ))

(defun dired-exit ()
  "Quit editing this directory."
  (interactive)
  (dired-do-flagged-delete)
  (kill-buffer (current-buffer)))

(defun dired-set-tags-file-name ()
  (interactive)
  (setq tags-file-name (dired-get-filename))
  (message "New tags file: %s" tags-file-name))

(defun dired-set-mark (char)
  "Set the mark of the current file to CHAR."
  (interactive)
  (cond ((eq char ?*)
	 (dired-mark 1))
	(t				; Assumes CHAR isn't ?T
	 (dired-change-marks ?* ?T)
	 (dired-mark 1)
	 (dired-change-marks ?* char)
	 (dired-change-marks ?T ?*))))

(defun concat-sep (sep lst)
  (if (null lst)
      ""
    (if (null (cdr lst))
	(car lst)
      (concat (car lst) sep (concat-sep sep (cdr lst))))))

(defun dired-hide-junk ()
  (interactive)
  (dired-mark-files-regexp
   (concat "\\([~#]\\|\."
	   (concat-sep 
	    "\\|"
	    '("[ao]" "aux" "cm[aiox]" "dvi" "eps" "hide" "log" "hi"
	      "old" "toc"))
	   "\\)$"))
   (dired-do-kill-lines))

(defun dired-vm-visit-file ()
  (interactive)
  (vm-visit-folder (dired-get-filename)))

;(defvar sc-font "-misc-fixed-medium-r-semicondensed--13-100-100-100-c-60-*-8")
(defvar sc-font "-adobe-courier-medium-r-normal--10-100-75-75-m-60-iso8859-1")
;-adobe-courier-medium-r-normal--8-80-75-75-m-50-iso8859-1")

(defun dired-my-view-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (cond ((string-match "\.ps$" file)
	   (dired-show-file "ghostview -magstep 2 '" file "' &"))
	  ((string-match "\.dvi$" file)
	   (dired-show-file "xdvi -s 3 '" file "' &"))
	  ((string-match "\.sc$" file)
	   (dired-show-file
	    (concat "xterm -xrm \"xterm.vt100.geometry:180x68\" "
		    "-xrm \"xterm.vt100.Font:" sc-font "\" -e sc '")
	    file "' &"))
	  ((or (string-match "\.jpg$" file)
	       (string-match "\.jpeg$" file)
	       (string-match "\.JPG$" file)
	       (string-match "\.gif$" file)
	       (string-match "\.GIF$" file)
	       (string-match "\.pgm$" file)
	       (string-match "\.pnm$" file)
	       (string-match "\.ppm$" file)
	       (string-match "\.pbm$" file)
	       (string-match "\.tif$" file))
	   (dired-show-file "xv '" file "' &"))
	  (t (dired-view-file)))))

(defun dired-edit-file ()
  (interactive)
  (let ((file (dired-get-filename)))
    (cond ((string-match "\.jpg$" file)
	   (edit-jpeg-comment file))
	  (t (dired-find-file)))))

;(defun dired-execute-file-app ()
;  (interactive)
;  (let ((file (dired-get-filename)))
;    (cond ((string-match "\.ps$" file)
;	   (shell-command (concat "ghostview -magstep 2 '" file "'")))
;	  ((string-match "\.dvi$" file)
;	   (shell-command (concat "xdvi -s 3 " file)))
;	  ((string-match "\.sc$" file)
;	   (shell-command (concat "xterm -font " sc-font
;				  " -geometry 210x76 -e sc " file)))
;;	  ((string-match "\.jpg$" file)
;;	   (shell-command (concat "djpeg < " file " | xv -")))
;	  ((or (string-match "\.jpg$" file)
;	       (string-match "\.JPG$" file)
;	       (string-match "\.gif$" file)
;	       (string-match "\.GIF$" file)
;	       (string-match "\.pgm$" file)
;	       (string-match "\.ppm$" file)
;	       (string-match "\.pbm$" file)
;	       (string-match "\.tif$" file))
;	   (dired-show-file))
;	  (t (dired-find-file))
;	  )))

(defun shell-escapes (s)
  (cond ((= (length s) 0)
	 "")
	((string-equal (substring s 0 1) "&")
	 (concat "\&" (shell-escapes (substring s 1))))
	(t
	 (concat (substring s 0 1) (shell-escapes (substring s 1))))))

(defun dired-show-file (pre path post)
  (save-excursion
    (let* ((f (dired-get-filename))
	   (d (file-name-directory f))
	   (n (file-name-nondirectory f))
	   (p (get-buffer-process "*shell*")))
      (set-buffer "*shell*")
      (comint-send-string p (concat "cd " d))
      (comint-send-input)
      ;(comint-send-string p (concat "xli '" f "' &"))
      (comint-send-string p (concat pre (shell-escapes path) post))
      (comint-send-input)))
  (dired-set-mark ?V))

(autoload 'vm-visit-folder "vm" "vm mail reader" t)

(defun dired-setup ()
  (define-key dired-mode-map "q" 'dired-parent)
  (define-key dired-mode-map "." 'dired-set-tags-file-name)
  (define-key dired-mode-map "Q" 'dired-exit)
  (define-key dired-mode-map "F" 'find-file-literally)
  (define-key dired-mode-map "V" 'dired-vm-visit-file)
  (define-key dired-mode-map "\C-k" 'dired-hide-junk)
  (define-key dired-mode-map "v" 'dired-my-view-file)
  (define-key dired-mode-map "e" 'dired-edit-file)
  ;(setq dired-listing-switches "-lG")	;; This messes up ange-ftp
  (setq dired-listing-switches "-la")
  )

(if (string< "19" emacs-version)
    (add-hook 'dired-mode-hook 'dired-setup)
  (load-library "dired")
  (dired-setup))
