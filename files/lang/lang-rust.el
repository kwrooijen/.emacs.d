(use-package toml-mode
  :straight t)

(use-package racer
  :straight t
  :hook ((rust-mode . racer-mode)
         (rust-mode . eldoc-mode)))

(use-package rust-mode
  :straight t
  :init
  (setq rust-format-on-save t)
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key rust-mode-map (kbd "M-i") 'iedit-mode)
  (mode-leader-def
    'normal rust-mode-map
    "'" 'cargo-process-repeat
    "cB" 'cargo-process-bench
    "cb" 'cargo-process-build
    "cc" 'cargo-process-clean
    "cd" 'cargo-process-doc
    "cD" 'cargo-process-doc-open
    "cn" 'cargo-process-new
    "ci" 'cargo-process-init
    "cr" 'cargo-process-run
    "ce" 'cargo-process-run-example
    "cR" 'cargo-process-run-bin
    "cs" 'cargo-process-search
    "ct" 'cargo-process-test
    "cu" 'cargo-process-update
    "cT" 'cargo-process-current-test
    "cf" 'cargo-process-current-file-tests
    "cC" 'cargo-process-check
    "cs" 'cargo-process-clippy
    "ca" 'cargo-process-add
    ;; "cr" 'cargo-process-rm
    "cU" 'cargo-process-upgrade
    "co" 'cargo-process-outdated
    "ca" 'cargo-process-audit))

(use-package cargo
  :straight t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package flycheck-rust
  :straight t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'lang-rust)
