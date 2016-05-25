;; For some reason on a remote connection the buffer doesn't get
;; removed, so remove it manually first
(defadvice async-shell-command (before attic-ad/async-shell-command activate)
  (when (get-buffer "*Async Shell Command*")
    (kill-buffer "*Async Shell Command*")))

(defadvice digit-argument (before attic-ad/digit-argument-before activate)
  (set-mark-command nil)
  (deactivate-mark))

(defadvice forward-list (before attic-ad/forward-list-before activate)
  (set-mark-command nil)
  (deactivate-mark))

(defadvice backward-list (before attic-ad/backward-list-before activate)
  (set-mark-command nil)
  (deactivate-mark))

(provide 'attic-advice)
