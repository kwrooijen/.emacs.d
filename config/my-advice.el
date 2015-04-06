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

(defadvice gnus (after gnus activate)
  (gnus-demon-init))

(defadvice dired-find-file (after dired-find-file activate)
  (set-neo-root-project))

(defadvice select-window-1 (after select-window-1 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-2 (after select-window-2 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-3 (after select-window-3 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-4 (after select-window-4 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice select-window-5 (after select-window-5 activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice neotree-enter (after neotree-enter activate)
  (if neotree-active
      (set-neo-root-project)))

(defadvice delete-other-windows (after delete-other-windows activate)
  (if sauron-active (sauron-toggle-hide-show)))

(provide 'my-advice)
