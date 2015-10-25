(defadvice attic-semi-colon/body (after attic-ad/attic-semi-colon/body-after activate)
  (unless powerline-fixed
    (powerline-reset)
    (setq powerline-fixed t)))

(defadvice kmacro-start-macro (before attic-ad/kmacro-start-before-macro activate)
  (setq macro-active t))

(defadvice kmacro-end-or-call-macro-repeat (after attic-ad/kmacro-end-or-call-macro-repeat-after activate)
  (setq macro-active nil))

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
