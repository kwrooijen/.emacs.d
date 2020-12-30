(use-package toml-mode
  :straight t)

(use-package rust-mode
  :straight t
  :config
  (define-key rust-mode-map (kbd "M-i") 'iedit-mode))

(use-package cargo
  :straight t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package flycheck-rust
  :straight t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'lang-rust)
