;; vi-word-motion.el

;; Change M-d and M-f to conform to vi's notion of a word.

(defvar my-white-chars "[ \t\n]")
(defvar my-word-chars "[a-zA-Z_]")
(defvar my-non-word-chars "[^a-zA-Z_ \t\n]")
(defvar my-word-pattern (concat "\\("
				my-white-chars "+\\)\\|\\("
				my-word-chars "+" my-white-chars "*\\)\\|\\("
				my-non-word-chars "+" my-white-chars "*\\)"))

(defun my-forward-word (count)
  "Stop at the beginning of the COUNT'th words from point."
  (interactive "p")
  (if (re-search-forward my-word-pattern nil t count)
      t (ding)))

(defun my-delete-word (count)
  "Delete up to the beginning of the COUNT'th words from point."
  (interactive "p")
  (if (re-search-forward my-word-pattern nil t count)
      (replace-match "" nil nil)
    (ding)))

(defun yank-after-point nil (interactive) (yank '(t)))

(define-key esc-map "f" 'my-forward-word)
(define-key esc-map "d" 'my-delete-word)
