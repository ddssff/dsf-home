(custom-set-variables
 '(display-time-interval 10))

(let ((host (shell-command-to-string "echo -n $HOSTNAME"))
      (user (getenv "USER")))
  (if (not (string= host "dsf"))
      (custom-set-variables
       `(display-time-format 
	 (concat ,user "@" ,host " - %H:%M%p")))))

(display-time)
