;;; history.el
;;;
;;; Last update: Thu Nov  1 07:36:52 2001
;;;
;;; These functions implement a flexible history mechanism for text
;;; files.  By default, there are three strings which trigger timestamp,
;;; attribution, and/or a log entry.  These three strings are:
;;;
;;;   "Last update"	Timestamp
;;;   "Last change"	Timestamp and attribution (by so-and-so at host)
;;;   "Last edited"	timestamp, attribution, and log entry
;;; 
;;; In order to be recognized by the history function, the strings must
;;; be immediately followed by a colon ":".  See the variables,
;;; last-update-regexp, last-change-regexp, and last-edited-regexp.
;;;
;;; Edit History:
;;; 
;;; Sun Sep  8 16:00:30 1996 by David Fox (fox at maki.net)
;;; 	 Allow ``Edit History'' as well as ``Last Edited''
;;; 
;;; Fri Nov 18 12:57:06 1994 by David Fox (fox at maki.net)
;;; 	 To do: Widen (again?) before inserting the text, in case the
;;; 	 user has narrowed the buffer while editing the history entry.
;;; 	 Also, pop buffer with point at same position as when the save
;;; 	 was first attempted, you are more likely to see the relevant
;;; 	 edited text that way, rather than the top of the file.
;;; 
;;; Mon Nov 14 09:05:41 1994 by David Fox (fox at fairview.net)
;;; 	 Added code to make the buffer being saved visible while
;;; 	 editing its history entry.
;;; 
;;; Sun Oct 30 14:26:16 1994 by David Fox (fox at first.cs.nyu.edu)
;;; 	 Added variable "history-require-entry", when set to nil you
;;; 	 can exit without typing a message and no entry will be logged.
;;; 
;;; Fri Sep  9 11:17:34 1994 by Alan K. Stebbens (aks at dokoka.ucsb.edu)
;;; 	 Old timestamps without the year were not being updated
;;; 	 correctly.  Created history-timestamp-regexp to make it easier
;;; 	 to tailor the kinds of timestamps recognized by the function
;;; 	 `history-update-last-update'.  Also, take care of blank lines at
;;;      the end of the logged text.
;;; 
;;; Fri Sep  9 10:24:09 1994 by Alan K. Stebbens (aks at dokoka.ucsb.edu)
;;; 	 Fixed update regexp pattern.
;;; 
;;; Thu Mar 17 22:01:40 1994 by Alan K. Stebbens (aks at dokoka.ucsb.edu)
;;; 	 Rewritten to allow timestamp-only, timestamp and attribution,
;;; 	 or all the above with log entry; general cleanup.
;;; 
;;; Fri May 18 08:42:51 1990 by Alan Stebbens (aks at somewhere.ucsb.edu)
;;;      Added option to allow minimal log entries: C-u C-c C-c.
;;; 	 Added fully qualified domain name lookup.
;;; 
;;; Fri Aug 18 18:44:37 1989 by Alan Stebbens (aks at ccse)
;;; 	 Fixed fill-prefix initialization.
;;; 
;;; Fri Aug 18 18:36:16 1989 by Alan Stebbens (aks at ccse)
;;;	 Replaced (backward-word 2) subsequent ref to (point) with a more
;;;	 simple, correct, and general (match-beginning 0).  This avoids
;;;	 the assumption that the last-edited-regexp is always two words
;;;	 (whatever a "word" is, since this depends upon the syntax table
;;;	 of the current buffer).
;;;
;;; Tue Apr  4 17:37:06 1989 by David C. Howland (dch at ccird3)
;;; 	 Added defvar for history-search-limit. Up'd the limit from 3000
;;; 	 characters to 5000


(defvar history-min-comment-length 10
  "*The minimum length that a \"change-log-stamp\" comment must be.
If set to 0 there is no minimum.")

(defvar history-require-entry t
  "*If t don't allow exiting without an entry of sufficient length.")

(defvar history-buffer "*History*"
  "*The buffer name to get history information from.")

(defvar history-search-limit 5000
  "*The marker string \"Last edited:\" or \"Last update:\" must occur in 
 the first 5000 characters")

(defvar history-mode-map nil)

(if history-mode-map
    nil
  (setq history-mode-map (make-sparse-keymap))
  (define-key history-mode-map "\C-c?"    'history-help)
  (define-key history-mode-map "\C-c\C-a" 'history-abort)
  (define-key history-mode-map "\C-c\C-c" 'history-exit)
  (define-key history-mode-map "\C-x\C-s" 'history-exit)
)

(defvar last-edited-regexp
  "\\(Last [Ee]dit\\(ed\\)?\\|Edit [Hh]istory\\|[Cc]hange [Ll]og\\):"
  "*The regexp after which the time, date, and log entry are written by 
the function \[[change-log-stamp].  See also: last-update-regexp,
last-change-regexp.")

(defvar last-update-regexp
  "Last [Uu]pdated?:"
  "*The regexp after which the time and date stamp is written by the
function change-log-stamp.  See also: last-edited-regexp and
last-change-regexp.")

(defvar last-change-regexp
  "Last [Cc]hanged?:"
  "*The regexp matching a string after which the time and date stamp,
and an attribution is written by the function change-log-stamp.  See
also: last-edited-regexp and last-update-regexp.")

(defconst history-timestamp-regexp
  "\\w+ \\w+ +[0-9]+ [0-9:]+\\( [0-9]+\\)?"
  "A regexp which matches the output of (current-time-string).  
Currently, the time-stamp looks like: \"Fri Sep  9 10:51:14 1994\".")

(defvar history-full-hostname nil
  "The fully-qualified domain name for this host.  If set to null,
the function of the same name will compute it dynamically.")

(defun history-full-hostname ()
  "Return this system's fully-qualified domain name.  If the 
variable of the same name is set, it returns its value; otherwise
it is computed from system-name and possibly using the file
\"/etc/resolv.conf\"."
  (if history-full-hostname
      history-full-hostname
    (setq history-full-hostname
	  (if (string-match "\\." (system-name))
	      (system-name)
	    (let ((file "/etc/resolv.conf")
		  (name (system-name))
		  (dom (getenv "DOMAINNAME")))
	      (if (and (null dom)
		       (file-readable-p file))
		  (save-window-excursion
		    (save-excursion
		      (let ((buf (get-buffer-create "*domain temp*")))
			(set-buffer buf)
			(insert-file file)
			(goto-char (point-min))
			(if (re-search-forward "^domain[ \t]+\\([^ \t\n]+\\)" nil t)
			    (setq dom (buffer-substring (match-beginning 1) (match-end 1))))
			(kill-buffer buf)))))
	      (if dom
		  (setq name (concat name "." dom)))
	      name)))))

;; Define the "signature" by which we attribute changes

(defvar history-change-attribution
  (concat " by " (user-full-name) " (" (user-login-name)
	  (if (boundp 'nick-name)
	      (concat " - " nick-name))
	  " at " (history-full-hostname) ")")
  "*A string used to identify changes by \\[change-log-stamp].  By default,
it is the following string: \"by First M. Lastname (login at host.domain)\"
If the user has defined the variable \"nick-name\", then it will be included
after the login.")


;; change-log-stamp

;; This is the principal command which should be attached via "write-file-hooks"

(defun change-log-stamp ()
  "Update the \"Last edited:\" and \"Last update:\" fields in a buffer.
Can be used on \"write-file-hooks\" for automatic updating.  The string
matched by \"last-edited-regexp\" triggers a log entry, while the string
matched by \"last-update-regexp\" causes only the time and date to be
recorded.  The log entry includes the date and time, the string kept in
the variable \"history-change-attribution\", and a comment which is
solicited from the user.
   The \"Last Edited:\" marker string must occur in the first 
\"history-search-limit\" characters of the buffer.  When creating a log
entry, any text which precedes on the same line the \"Last edited\"
string is used as a prefix for each comment line."
  (interactive)
  (history-update-last-edit)
  (history-update-last-change)
  (history-update-last-update)
  nil)					;return nil

(defvar history-orig-buffer)		;keep track of the original buffer

(defvar history-comments)
(defvar history-insert-start)

(defun history-update-last-edit ()
  "Update the log entry in the current buffer.   Search for the first
occurance of the string matched by \"last-edited-regexp\", and, if
found, create a new log entry."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward last-edited-regexp history-search-limit t)
        (let ((line-length (or (and (boundp 'fill-column) fill-column) 72))
	      prefix-line here)
	  (save-excursion
	    (beginning-of-line)
	    (setq here (point))
	    (re-search-forward last-edited-regexp)
	    (setq history-comments (buffer-substring here (match-beginning 0)))
	    )
	  (setq prefix-line (concat history-comments "	 "))
	  (setq history-orig-buffer (buffer-name))
	  (save-window-excursion
	    (setq history-insert-start (point)) ; save place where text is to go.
	    (switch-to-buffer history-orig-buffer) ; Make sure mod buff visible
	    (pop-to-buffer history-buffer t)
	    (erase-buffer)
	    (set-buffer-modified-p nil)
	    (or (eq major-mode 'indented-text-mode)
		(indented-text-mode))
	    (setq fill-column line-length)
	    (insert prefix-line)
	    (set-fill-prefix)
	    (auto-fill-mode 1)
	    (use-local-map history-mode-map)
	    (message 
	     "Enter log message. Type C-c C-c when done, C-c ? for help.")
	    (recursive-edit)
	    (kill-buffer history-buffer))
	  )
      )
    )
  )

(defun history-update-last-update ()
  "Update the \"Last update:\" timestamp in the current buffer.  Search
for the first occurance of the string matched by \"last-update-regexp\",
and, if found, replace the previous timestamp with a new one."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward last-update-regexp history-search-limit t)
	(let ((limit (1+ (save-excursion (end-of-line) (point)))))
	  (if (re-search-forward (concat "\\s *" history-timestamp-regexp) limit t)
	      (replace-match (concat " " (current-time-string)) nil t))))))

(defun history-update-last-change ()
  "Update the \"Last update:\" timestamp and attribution in the current
buffer.  Search for the first occurance of the string matched by
\"last-change-regexp\", and, if found, replace the previous timestamp
with a new one."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward last-change-regexp history-search-limit t)
	(let ((start (point)))
	  (end-of-line)
	  (delete-region start (point))
	  (goto-char start)
	  (insert " " (current-time-string))
	  (insert history-change-attribution)))))
  

(defun history-exit (&optional arg)
  "Leave the recursive edit of an history log message."
  (interactive "P")
  (let ((buffsize (buffer-size)))
    (cond ((or arg (> buffsize history-min-comment-length))
	   (switch-to-buffer history-orig-buffer)
	   (goto-char history-insert-start) ; move to where text is to go
	   (insert "\n" history-comments "\n")
	   (insert history-comments (current-time-string))
	   (insert history-change-attribution)
	   (insert "\n")
	   (insert-buffer history-buffer)
	   (forward-char buffsize)
	   (insert "\n")
	   (delete-blank-lines)
	   (delete-blank-lines)
	   (exit-recursive-edit))
	  (history-require-entry
	   (error
	    "Log must be greater than %d characters!"
	    history-min-comment-length))
	  (t (history-abort))
	  )
    )
  )

(defun history-abort ()
  "Abort the recursive edit of an history log message."
  (interactive)
  (exit-recursive-edit)
  )

(defun history-help()
  "Describes \"change-log-stamp\"  key bindings. See \"change-log-stamp\" function.
Related variables: history-min-comment-length, history-buffer and 
                   history-search-limit.

The following commands are available:
\\{history-mode-map}
"
  
  (interactive)
  (describe-function 'history-help)
)
