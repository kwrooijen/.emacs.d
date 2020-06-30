;;; collection/collection-theme.el -*- lexical-binding: t; -*-

(use-package solaire-mode
  :straight t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  ;; Solaire assumes real buffers are darkened, and brightens up other buffers.
  ;; We can use (solaire-mode-swap-bg) to make real buffers bright, but the title
  ;; bar wil stay dark. So instead we reverse the `solaire-mode-real-buffer-fn`
  ;; function. (solaire-mode-reset-fringe-face nil) needs to be run

  ;; (setq solaire-mode-real-buffer-fn (lambda () (not (buffer-file-name))))
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package doom-themes
  :straight t
  :after solaire-mode
  :config
  (setq dark-theme? t)
  (defun toggle-theme ()
    (interactive)
    (mapc 'disable-theme custom-enabled-themes)
    (if dark-theme?
        (progn
          (load-theme 'doom-moonlight t)
          (set-face-background 'region "#603693"))
      (load-theme 'doom-one-light t))
    (setq dark-theme? (not dark-theme?)))
  (load-theme 'doom-moonlight t)
  (set-face-background 'region "#603693")
  (solaire-mode-swap-bg))

(use-package vi-tilde-fringe
  :straight t
  :config

  (add-hook 'text-mode-hook #'vi-tilde-fringe-mode)
  (add-hook 'prog-mode-hook #'vi-tilde-fringe-mode)
  (add-hook* 'vi-tilde-fringe-mode-hook
             (setq left-fringe-width 8
                   right-fringe-width 8)))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(provide 'collection-theme)
