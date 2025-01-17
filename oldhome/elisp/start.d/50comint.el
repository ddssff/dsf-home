(require 'comint)

(defun comint-send-string (process string)
  "Like `process-send-string', but also does extra bookkeeping for Comint mode."
  (if process
      (with-current-buffer (if (processp process)
			       (process-buffer process)
			     (get-buffer process))
	(comint-snapshot-last-prompt))
    (comint-snapshot-last-prompt))
  (my-process-send-string process string))

;; Break up the string so that we don't get EOT characters in our input stream.
(defun my-process-send-string (process string)
  (if (> (length string) 200)
      (progn (process-send-string process (substring string 0 200)) (my-process-send-string process (substring string 200)))
    (process-send-string process string)))	     
