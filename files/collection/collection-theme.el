;;; collection/collection-theme.el -*- lexical-binding: t; -*-

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))

(use-package vi-tilde-fringe
  :straight t
  :config
  (add-hook 'text-mode-hook #'vi-tilde-fringe-mode)
  (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package solaire-mode
  :straight t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(provide 'collection-theme)
