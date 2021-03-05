(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :config
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

(defun elpy-shell-goto-buffer ()
  (interactive)
  (popwin:pop-to-buffer "*Python*"))

(use-package python-mode
  :straight t
  :config
  (mode-leader-def
    'normal python-mode-map
    "e" '(:ignore t :which-key "Evaluate")
    "bb" 'elpy-shell-goto-buffer
    "ee" 'elpy-shell-send-statement
    "eb" 'elpy-shell-send-buffer))

(provide 'lang-python)
