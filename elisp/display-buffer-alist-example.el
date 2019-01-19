;; I don't use this file, its an example I pulled from somewhere. -dsf

;; Configure `display-buffer' behaviour for some special buffers
(setq display-buffer-alist
      `(;; Messages, errors, processes, Calendar in the bottom side window
        (,(rx bos (or "*Apropos"                   ; Apropos buffers
                      "*Man"                       ; Man buffers
                      "*Help"                      ; Help buffers
                      "*Warnings*"                 ; Emacs warnings
                      "*Process List*"             ; Processes
                      "*Proced"                    ; Proced processes list
                      "*Compile-Log*"              ; Emacs byte compiler log
                      "*compilation"               ; Compilation buffers
                      "*Flycheck errors*"          ; Flycheck error list
                      "*Calendar"                  ; Calendar window
                      "*env-info"                  ; Environment information
                      "*Cargo"                     ; Cargo process buffers
                      "*Word"                      ; WordNut buffers
                      (and (1+ nonl) " output*"))) ; AUCTeX command output
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.45))
        ;; REPLs on the bottom half
        (,(rx bos (or "*cider-repl"     ; CIDER REPL
                      "*intero"         ; Intero REPL
                      "*idris-repl"     ; Idris REPL
                      "*ielm"           ; IELM REPL
                      "*SQL"))          ; SQL REPL
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (reusable-frames . visible)
         (window-height . 0.50))
        ;; Open shell in a single window
        (,(rx bos "*shell")
         (display-buffer-same-window)
         (reusable-frames . nil))
        ;; Open PDFs in the right side window
        (,(rx bos "*pdf")
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (reusable-frames . visible)
         (window-width . 0.5))
        ;; Let `display-buffer' reuse visible frames for all buffers. This must
        ;; be the last entry in `display-buffer-alist', because it overrides any
        ;; previous entry with more specific actions.
        ("." nil (reusable-frames . visible))))
