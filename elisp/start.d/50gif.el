;; gif-hacks.el

;;
;; Grabgif
;;

(autoload 'gnus-grab-gif "gif" "" t)
(autoload 'uuencode-file "uu" "" t)
(autoload 'uuencode-buffer "uu" "" t)
(autoload 'uuencode-region "uu" "" t)
(autoload 'uudecode-buffer "uu" "" t)
(autoload 'uudecode-region "uu" "" t)
(autoload 'uudecode-region-replace "uu" "" t)
(autoload 'uumerge-buffer "uu" "" t)
(autoload 'mark-next-uuencode "uu" "" t)
    
;; Uncomment this line if you are using the uucat program provided at
;; the end of uu.el
(setq uumerge-in-unix t)
    
;; This specifies the default directory to put uudecodes into
(setq uudecode-directory (expand-file-name "~/xfer/"))

;; Commands for viewing images from dired mode.

(defvar graphics-display
  (if (getenv "COLORDISP")
      (getenv "COLORDISP")
    (concat "-display " (getenv "DISPLAY")))
  "string to pass to -display argument of X commands.")

(defun image-viewing-hacks ()
  (define-key dired-mode-map "1" 'dired-view-image-file-1)
  (define-key dired-mode-map "2" 'dired-view-image-file-2)
  (define-key dired-mode-map "3" 'dired-view-image-file-3)
  (define-key dired-mode-map "4" 'dired-he-file-1)
  (define-key dired-mode-map "5" 'dired-he-file-2)
  (define-key dired-mode-map "6" 'dired-he-file-3))

(defun dired-view-image-file-1 () (interactive) (dired-view-image-file 1))
(defun dired-view-image-file-2 () (interactive) (dired-view-image-file 2))
(defun dired-view-image-file-3 () (interactive) (dired-view-image-file 3))

;(defun dired-view-command (file scale)
;  (cond ((or (string-match ".jpg$" (dired-get-filename))
;	     (string-match ".JPG$" (dired-get-filename)))
;	 (concat "djpeg "
;		 (if (= scale 9) "-gif ")
;		 " < "
;		 (dired-get-filename)
;		 (if (< scale 9)
;		     (concat
;		      (if (= scale 1)
;			  ""
;			(concat " | pnmscale " (int-to-string scale)))
;		      " | ppmdither ")
;		     )
;		 " | xloadimage " graphics-display
;		 " -title " (dired-get-filename)
;		 " stdin &"))
;	((or (string-match ".gif$" (dired-get-filename))
;	     (string-match ".GIF$" (dired-get-filename)))
;	 (concat "xloadimage " graphics-display
;		 " -zoom " scale "00 " 
;		 (dired-get-filename) "&"))
;	((string-match ".pf.Z$" (dired-get-filename))
;	 (concat "zcat " (dired-get-filename) " | suntopic | "
;		 (if (> scale 1)
;		     (concat " scale "
;			     (int-to-string scale) " " 
;			     (int-to-string scale) " IN OUT | "))
;		 "unmap IN OUT | dither 224 IN OUT | "
;		 "iview " graphics-display 
;		 " -name " (dired-get-filename) " IN &"))))

(defun dired-view-command (file scale)
  (concat "xloadimage "
	  graphics-display " "
	  (if (> scale 1)
	      (concat "-zoom " scale "00 "))
	  (dired-get-filename)
	  "&"))

(defun dired-view-image-file (scale)
; new code
  (let ((buf (buffer-name))
	(pt (point))
	(cmd (dired-view-command (dired-get-filename) scale)))
    (set-buffer "*shell*")
    (insert cmd)
    (comint-send-input)
    (set-buffer buf)
    (dired-next-line 1)
    )
; end of new code
; old code
;  (save-excursion
;    (let ((cmd (dired-view-command (dired-get-filename) scale)))
;      (set-buffer "*shell*")
;      (end-of-buffer)
;      (insert cmd)
;      (comint-send-input)))
;  (dired-next-line 1)
; end of old code
  )

(add-hook 'dired-load-hook 'image-viewing-hacks)

;(defun get-gif ()
;  (grab-gif)

;(load-library "ange-ftp-hacks")
