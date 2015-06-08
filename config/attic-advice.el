(defadvice kmacro-start-macro (before kmacro-start-macro activate)
  (setq macro-active t))

(defadvice kmacro-end-or-call-macro-repeat (after kmacro-end-or-call-macro-repeat activate)
  (setq macro-active nil))

(defadvice digit-argument (before digit-argument activate)
  (set-mark-command nil)
  (deactivate-mark))

(defadvice forward-list (before forward-list activate)
  (set-mark-command nil)
  (deactivate-mark))

(defadvice backward-list (before backward-list activate)
  (set-mark-command nil)
  (deactivate-mark))

(defadvice delete-other-windows (after delete-other-windows activate)
  (if sauron-active
      (progn
        (attic-sauron-toggle)
        (switch-to-buffer-other-window "*Sauron*")
        (split-window-right)
        (other-window 1)
        (switch-to-buffer ":home")
        (beginning-of-buffer)
        (sticky-window-keep-window-visible)
        (other-window 1))))

(provide 'attic-advice)
