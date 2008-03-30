;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uu.el
;;
;; Written by Art Mellor @ Cayman Systems, Inc. 1991
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; M-x uuencode-file
;;;;
;;;; M-x uuencode-region
;;;;
;;;; M-x uuencode-buffer
;;;;
;;;; M-x uudecode-buffer
;;;;
;;;; M-x uudecode-region
;;;;
;;;; M-x mark-next-uuencode
;;;;
;;;; M-x uumerge-buffer
;;;;
;; Last Edited:
;; 
;; Mon Nov  3 11:12:47 1997 by David Fox (fox at dsf.net)
;; 	 Switched from base64 to munpack.
;; 
;; Wed Mar 19 11:16:46 1997 by David Fox (fox at dsf.net)
;; 	 Only write jpeg block for jpeg files.
;; 
;; Fri Jan 31 06:08:44 1997 by David Fox (fox at dsf.net)
;; 	 Convert spaces in filenames for uudecoding to underscores.
;; 
;; Mon Jan 13 12:31:00 1997 by David Fox (fox at maki.net)
;; 	 Insert message header into jpeg comment field for base64 images.
;; 
;; Mon Jan 13 12:03:00 1997 by David Fox (fox at dsf.net)
;; 	 Construct filename using a checksum on the buffer to be
;; 	 uudecoded rather than a sequence number.
;; 
;; Tue Aug 13 10:51:50 1996 by David Fox (fox at maki.net)
;; 	 Added code to recognize and perform base64 decoding.
;; 	 Specifically, a base64 encoded jpeg file begins with "/9j/",
;; 	 so we look for files that contain this string at the
;; 	 beginning of a line.  Filenames are constructed using the
;; 	 prefix "base64-", a sequence number, and the suffix ".jpg".
;; 	 See also gif.el.

;; default dir for uudecode-buffer output
(defvar uudecode-directory "")

;; whether to use UNIX based uumerge, or emacs based
(defvar uumerge-in-unix nil)
(defvar unix-uumerge-command "uucat")

;; Insert the uuencoded version of a specified file
(defun uuencode-file (file)
  (interactive "fInsert uuencoded file: ")
  (let ((buffer (generate-new-buffer "*uuencode*"))
	(filename))
    ;; what file to enclose
    (setq file (expand-file-name file))
    ;; break it apart
    (setq filename (file-name-nondirectory file))

    (save-excursion
      ;; go to temp buffer and clean it out
      (set-buffer buffer)
      (delete-region (point-min) (point-max))

      ;; slap file in buffer
      (insert-file file)

      ;; uuencode it
      (message "uuencoding...")
      (uuencode-region filename (point-min) (point-max))
      (message "uuencoding...done"))

    ;; stuff it in the original buffer and delete temporary
    (insert-buffer buffer)
    (kill-buffer buffer)))

;; Run uuencode over a region and use 'filename' as file's output name
(defun uuencode-region (filename start end)
  (interactive "sNew filename: \nr")
  (shell-command-on-region
   start end (concat "uuencode " filename) t))

;; Run uuencode on a buffer
(defun uuencode-buffer (filename)
  (interactive "sFilename for uuencode: \nP")
  (uuencode-region filename (point-min) (point-max)))

;; Convert spaces to underscores
(defun repair-file-name (filename &optional index)
  (if (not index) (setq index 0))
  (cond ((= index (length filename)) filename)
	((string= (substring filename index (+ index 1)) " ")
	 (concat (substring filename 0 index) "_"
		 (repair-file-name (substring filename (+ index 1)))))
	(t (repair-file-name filename (+ index 1)))))

;; Run uudecode over a region with output to 'filename'
(defun uudecode-region (filename start end &optional skip-uumerge)
  (interactive "FUudecode to file: \nr\nP")
  (let ((buffer (generate-new-buffer "*uudecode*")) ; Temp buffer
	(temp))
    (save-excursion
      ; pop contents of this buffer in temp buffer
      (append-to-buffer buffer start end)
      (set-buffer buffer)

      ; Merge multiple parts
      (if (not skip-uumerge)
	  (if uumerge-in-unix
	      (if (not (unix-uumerge-buffer))
		  (progn
		    (kill-buffer buffer)
		    (error (concat unix-uumerge-command
				   ": Command not found."))))
	    (uumerge-buffer)))

      ; find the uuencode file name and put it in 'file'
      (goto-char (point-min))
      (cond ((re-search-forward "^begin [0-7]* " end t)
	     (setq temp (point))
	     (end-of-line)
	     (delete-region temp (point))
	     (insert-string (expand-file-name (repair-file-name filename)))

	     ; send the buffer through uuencode and axe the temp buffer
	     (message "uudecoding...")
	     (shell-command-on-region (point-min) (point-max) "uudecode" nil)
	     (message "uudecoding...done.")
	     (kill-buffer buffer))
	    (t
	     (message "mime decoding...")
	     (shell-command-on-region (point-min) (point-max)
				      (concat "cd " uudecode-directory
					      "; munpack > /dev/null") nil)
	     (message "mime decoding...done")
	     (kill-buffer buffer))
;	    (t
;	     (kill-buffer buffer)
;	     (error "Can't find beginning of uuencoded data"))
	    ))))
      

;; Replace the region with the uudecoded version
(defun uudecode-region-replace (start end &optional skip-uumerge)
  (interactive "r")
  (let ((temp (concat "/tmp/uudecode" (user-uid))))
    (uudecode-region temp start end skip-uumerge)
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert-file temp))
    (delete-file temp)))

;; Run uudecode over a buffer with output to a user specified file
(defun uudecode-buffer (&optional skip-uumerge)
  (interactive "P")
  (let ((filename)
	(file)
	(temp)
	(headers nil))
    ;; find the uuencode file name and put it in 'file'
    (goto-char (point-min))
    (message "still uudecoding.")
    (cond ((re-search-forward "^begin [0-7]* " nil t)
	   (setq temp (point))
	   (setq headers (point))
	   (end-of-line)
	   (setq filename (repair-file-name (buffer-substring temp (point)))))
	  (t
	   (push-mark (point) t)
	   (end-of-buffer)))
      
    ;; ask user if this is where it should go and what format
    (if (equal "" uudecode-directory)
	(setq uudecode-directory default-directory))

    (setq file
;	  (expand-file-name
;	   (read-file-name
;	    (concat "Uudecode to file: (default " filename ") ")
;	    uudecode-directory
	    (concat uudecode-directory filename)
;	    nil))
	  )

    ;; make sure it's not a directory
    (if (file-directory-p file)
	(setq file (concat (directory-file-name file) "/" filename)))

    (setq uudecode-directory (file-name-directory file))
    (if (not uudecode-directory) (setq uudecode-directory default-directory))

    (uudecode-region file (point-min) (point-max) skip-uumerge)

    ; If its a jpeg file put the article headers into the comment block.
    (let ((ext (substring filename -4)))
      (if (and headers (or (string= ext ".jpg") (string= ext ".JPG")))
	  (shell-command-on-region
	   1 headers (concat "wrjpgcom " file " > " file "2 && "
			     "/bin/mv " file "2 " file))))))

;; Put a region around the next uuencode begin/end pair
;; Leave point at end so that you can do this repeatedly
(defun mark-next-uuencode ()
  (interactive)
  (cond ((re-search-forward "^begin [0-7]* " nil t)
	 (beginning-of-line)
	 (push-mark (point) t)
	 (if (not (re-search-forward "^end" nil t))
	     (error "Can't find end of uuencoded data"))
	 (next-line 1))
	((re-search-forward "^/9j/" nil t)
	 (beginning-of-line)
	 (push-mark (point) t)
	 (end-of-buffer))
	(t
	 (error "Can't find next uuencode"))))

;; Delete all non-uu type lines from a buffer. This is useful if you have
;; a multi-part uu file and want to strip out garbage (e.g. mail headers)
;; from between the parts.
;; I got the heuristics from a perl script called uumerge

;; return nil if the command was not found
;; UNIX command 'uucat' source provided at end of this file
(defun unix-uumerge-region (start end)
  (interactive "r")
  (message "uumerging (via UNIX)...")
  (save-excursion
    (shell-command-on-region start end unix-uumerge-command t)
    (goto-char start)
    (if (search-forward
	 (concat unix-uumerge-command ": Command not found.") (point-max) t)
	nil
      (message "uumerging (via UNIX)...done."))))

(defun unix-uumerge-buffer ()
  (interactive)
  (unix-uumerge-region (point-min) (point-max)))

(defun uumerge-buffer ()
  (interactive)

  (message "uumerging...")
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
    
      ;; Check for valid begin and end
      (cond ((re-search-forward "^begin [0-7]* " nil t)
	     (next-line 1)
	     (beginning-of-line)
	     (save-excursion
	       (if (not (re-search-forward "^end" nil t))
		   (error "Can't find end of uuencoded data")))

	     ;; Delete lines that aren't valid UU lines in between begin and end
	     (while (and (not (is-uu-end)) (not (eobp)))
	       (if (line-contains-regexp "[a-z]+$")
		   (delete-region (match-beginning 0) (match-end 0)))
	       (beginning-of-line)
	       (cond ((line-contains-regexp "[a-z]")
		      (kill-line))
		     ((is-uu-line)
		      (next-line 1))
		     (t (kill-line)))))
	    (t 
	     (error "Can't find beginning of uuencoded data")))))
  (message "uumerging...done."))

(defun is-uu-end ()
  (if (not (or (looking-at "^ $") (looking-at "^`")))
      nil
    (save-excursion
      (next-line 1)
      (line-contains-regexp "^end"))))

(defun is-uu-line ()
  (save-excursion
    (let ((declared-len) (actual-len (line-length)))
      (beginning-of-line)
      (setq declared-len (- (char-after (point)) 32))
      (if (> declared-len 0)
	  (= (/ (+ 2 declared-len) 3)
	     (/ actual-len 4))
	nil))))
  
(defun line-length ()
  (save-excursion
    (beginning-of-line)
    (let ((begin (point)))
      (end-of-line)
      (+ 1 (- (point) begin)))))

(defun replace-regexp-in-line (regexp string)
  (save-excursion
    (beginning-of-line)
    (let ((begin (point)) (value))
      (end-of-line)
      (narrow-to-region begin (point))
      (beginning-of-line)
      (setq value (replace-regexp regexp string))
      (widen)
      value)))


(defun line-contains-regexp (regexp)
  (save-excursion
    (end-of-line)
    (let ((end (point)) (value))
      (beginning-of-line)
      (setq value (re-search-forward regexp end t))
      (widen)
      value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uumerge, perl script
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #!/usr/bin/perl
;; while (<>) {
;;         $sawbegin++, last if ($mode,$file) = /^begin\s*(\d*)\s*(\S*)/;
;; }
;; die "missing begin" unless $sawbegin;
;; open(OUT,"> $file") if $file ne "";
;; while (<>) {
;;         $sawend++, last if /^end/;
;;         s/[a-z]+$//; # handle stupid trailing lowercase letters
;;         next if /[a-z]/;
;;         next unless int((((ord() - 32) & 077) + 2) / 3) == int(length() / 4);
;;         print OUT unpack("u", $_);
;; }
;; die "missing end" unless $sawend;
;; chmod oct($mode), $file;
;; exit 0;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source for a program to merge uu files via unix
;; You will find the emacs command delete-rectangle useful for removing ;'s
;; I got this from a friend, hope that its not illegal to be redistributing it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /* ==================== uucat.c ==================== */
;; /*
;; 
;;    uucat file1 file2 ... filen | uudecode
;;    or
;;    cat file1 file2 ... filen | uucat | uudecode
;;    or, if you already have concatenated them into one file
;;    uucat file | uudecode
;; 
;; */
;; 
;; /* uucat.c */
;; #include <stdio.h>
;; #include <string.h>
;; 
;; #define TRUE	1
;; #define FALSE	0
;; 
;; #define LENGTH	150
;; 
;; extern void uuread();
;; 
;; 
;; void main(argc, argv)
;; int argc;
;; char *argv[];
;; {
;;   int error, argno;
;;   FILE *infile;
;; 
;;   if (argc < 2)
;;     uuread(stdin);
;;   else
;;   {
;;     error = FALSE;
;;     for (argno = 1; !error && argno < argc; argno++)
;;     {
;;       if ((infile = fopen(argv[argno], "r")) == NULL)
;;       {
;; 	error = TRUE;
;; 	fprintf(stderr, "uucat: Can't open '%s' for input.\n", argv[argno]);
;;       }
;;       else
;;       {
;; 	uuread(infile);
;; 	fclose(infile);
;;       }
;;     }
;;   }
;; }
;; 
;; 
;; void uuread(infile)
;; FILE *infile;
;; {
;;   char *s2, *s1, *s0, *tmp_s;
;;   int length;
;;   static char s[3 * (LENGTH + 1)];
;;   static int echo_on = FALSE, started = FALSE, just_finished = FALSE;
;;   static int line_length = 0, lines_to_go = 0;
;; 
;;   s0 = s;
;;   s1 = s0 + (LENGTH + 1);
;;   s2 = s1 + (LENGTH + 1);
;; 
;;   s0[0] = s1[0] = s2[0] = '\0';  /* Clear strings */
;; 
;;   while (fgets(s0, LENGTH, infile) != NULL)
;;   {
;;     s0[LENGTH] = '\0';  /* Make sure string is terminated */
;; 
;;     if (just_finished)
;;     {
;;       if (strncmp(s0, "size ", 5) == 0)
;;       {
;; 	fputs(s0, stdout);
;; 	s0[0] = '\0';
;;       }
;;       just_finished = FALSE;
;;     }
;; 
;;     if (!started)
;;     {
;;       if (strncmp(s0, "begin ", 6) == 0)
;;       {
;; 	started = echo_on = TRUE;
;; 	line_length = 0;
;; 	lines_to_go = 0;
;;       }
;;     }
;;     else  /* started */
;;     {
;;       length = strlen(s0);
;;       if (line_length == 0)
;; 	line_length = length;
;; 
;;       if (echo_on)
;;       {
;; 	lines_to_go = 0;
;; 	if (s0[0] != 'M' || length != line_length)
;; 	{
;; 	  echo_on = FALSE;
;; 	  lines_to_go = 2;  /* Lines to go before 'end' is expected */
;; 	}
;;       }
;;       else  /* !echo_on */
;;       {
;; 	if (s0[0] == 'M' && length == line_length)
;; 	  echo_on = TRUE;
;; 	else if (lines_to_go > 0)
;; 	{
;; 	  if (lines_to_go == 2)
;; 	  {
;; 	    if (s0[0] == ' ' || s0[0] == '`')
;; 	      lines_to_go = 1;
;; 	    else
;; 	      lines_to_go = 0;  /* Unexpected line, so break off */
;; 	  }
;; 	  else if (lines_to_go == 1)
;; 	  {
;; 	    if (strcmp(s0, "end\n") == 0)
;; 	    {
;; 	      fputs(s2, stdout);
;; 	      fputs(s1, stdout);
;; 	      fputs(s0, stdout);
;; 	      lines_to_go = 0;  /* Done. Break off */
;; 	      just_finished = TRUE;
;; 	      started = FALSE;
;; 	    }
;; 	    else
;; 	      lines_to_go = 0;  /* Unexpected line, so break off */
;; 	  }
;; 	}
;;       }
;;     }
;; 
;;     if (echo_on)
;;     {
;;       fputs(s0, stdout);
;;       s0[0] = '\0';
;;     }
;; 
;;     tmp_s = s2;
;;     s2 = s1;
;;     s1 = s0;
;;     s0 = tmp_s;
;;   }
;; }
