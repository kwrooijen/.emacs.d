(use-package hy-mode
  :straight t
  :config
  (add-hook 'hy-mode-hook #'lispy-mode)
  (add-hook 'hy-mode-hook #'flycheck-mode)
  (mode-leader-def
    'normal hy-mode-map

    "" nil))

(provide 'lang-hy)
