;;; custom/custom-keys.el -*- lexical-binding: t; -*-

(bind-key* "M-+" 'align-regexp)
(bind-key* "M-C" 'capitalize-previous-word)
(bind-key* "C-S-V" 'x-clipboard-yank)
(bind-key* "C-S-C" 'clipboard-kill-ring-save)

(general-define-key
 :states '(normal visual)
 :keymaps '(override)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "" nil

 "b" '(:ignore t :which-key "Buffers")
 "bb" 'helm-mini

 "f" '(:ignore t :which-key "Files")
 "ff" 'helm-find-files
 "fs" 'save-buffer

 "g" '(:ignore t :which-key "Git")
 "gg" 'magit-status
 "gu" 'smerge-keep-upper
 "gl" 'smerge-keep-lower
 "ga" 'smerge-keep-all

 "p" '(:ignore t :which-key "Project")
 "pp" 'helm-projectile-switch-project
 "pf" 'helm-projectile-find-file
 "pi" 'projectile-invalidate-cache
 "pr" 'projectile-replace

 "s" '(:ignore t :which-key "Search")
 "ss" 'helm-swoop-without-pre-input
 "sp" 'helm-projectile-ag

 "w" '(:ignore t :which-key "Window")
 "ws" 'evil-window-split
 "wu" 'winner-undo
 "wU" 'winner-redo
 "wq" 'kill-this-buffer

 "wh" 'evil-window-left
 "wl" 'evil-window-right
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wL" 'split-window-right
 "wJ" 'split-window-below

 "r" '(:ignore t :which-key "Resume")
 "rr" 'helm-resume
 "ry" 'helm-show-kill-ring
 "rb" 'select-minibuffer)

(provide 'custom-keys)
