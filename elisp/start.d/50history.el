;; CHANGE LOG, EDIT HISTORY

;(setq write-file-hooks (cons 'change-log-stamp write-file-hooks))
(autoload 'change-log-stamp "history" "Automatic edit history logger." t)
(setq history-require-entry nil)
