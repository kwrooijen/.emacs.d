;; For some reason on a remote connection the buffer doesn't get
;; removed, so remove it manually first
(defadvice async-shell-command (before attic-ad/async-shell-command activate)
  (when (get-buffer "*Async Shell Command*")
    (kill-buffer "*Async Shell Command*")))

(provide 'attic-advice)
