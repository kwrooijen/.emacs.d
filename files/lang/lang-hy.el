(use-package hy-mode
  :straight t
  :config
  (define-key hy-mode-map (kbd "C-x C-e") 'hy-shell-eval-last-sexp-stay)
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'flycheck-mode)
  (mode-leader-def
    'normal hy-mode-map
    "eb" 'hy-shell-eval-buffer
    "'" 'run-hy))

(defun hy-shell-eval-last-sexp-stay ()
  "Eval last Hy expression but don't jump to buffer"
  (interactive)
  (save-window-excursion
    (hy-shell-eval-last-sexp)))

(provide 'lang-hy)
