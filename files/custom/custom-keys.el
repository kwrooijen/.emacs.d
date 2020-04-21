;;; custom/custom-keys.el -*- lexical-binding: t; -*-

(bind-key* "M-+" 'align-regexp)
(bind-key* "M-C" 'capitalize-previous-word)

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

 "p" '(:ignore t :which-key "Project")
 "pp" 'helm-projectile-switch-project
 "pf" 'helm-projectile-find-file

 "s" '(:ignore t :which-key "Search")
 "ss" 'helm-swoop-without-pre-input
 "sp" 'helm-projectile-ag

 "w" '(:ignore t :which-key "Window")
 "ws" 'evil-window-split
 "wu" 'winner-undo
 "wu" 'winner-redo

 "r" '(:ignore t :which-key "Resume")
 "rr" 'helm-resume
 "ry" 'helm-show-kill-ring
 "rb" 'select-minibuffer)

(provide 'custom-keys)
