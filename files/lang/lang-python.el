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
  (add-hook* 'python-mode-hook (electric-pair-mode 1))
  (mode-leader-def
    'normal python-mode-map
    "t" '(:ignore t :which-key "Test")
    "tt" 'elpy-test-pytest-runner
    "e" '(:ignore t :which-key "Evaluate")
    "bb" 'elpy-shell-goto-buffer
    "ee" 'elpy-shell-send-statement
    "eb" 'elpy-shell-send-buffer))

(use-package python-pytest
  :straight t)

(provide 'lang-python)
