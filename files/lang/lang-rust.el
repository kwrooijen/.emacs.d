(use-package toml-mode
  :straight t)

(use-package rust-mode
  :straight t
  :hook (rust-mode . lsp))

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :straight t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'lang-rust)
