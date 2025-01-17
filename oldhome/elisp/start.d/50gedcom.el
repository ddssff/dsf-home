(autoload 'gedcom-mode "gedcom" "GEDCOM Mode" t)
(setq auto-mode-alist
  (cons '("\\.ged$" . gedcom-mode) auto-mode-alist))
