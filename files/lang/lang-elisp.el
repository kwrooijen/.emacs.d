(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

(provide 'lang-elisp)
