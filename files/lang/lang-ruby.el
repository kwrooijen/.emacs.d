(use-package ruby-tools
  :straight t)

(use-package ruby-hash-syntax
  :straight t
  :general
  (:states 'visual
   "C-m"   'ruby-hash-syntax-toggle))

(use-package ruby-electric
  :straight t
  :config
  (eval-after-load "ruby-mode"
    '(add-hook 'ruby-mode-hook 'ruby-electric-mode)))

(use-package chruby
  :straight t
  :config   (add-hook 'ruby-mode-hook 'chruby-use-corresponding))

(use-package minitest
  :straight t
  :general
  (:keymaps 'ruby-mode-map
   :states  'normal
   ", t t"  'minitest-verify
   ", t a"  'minitest-verify-all)
  :config
  (add-hook 'minitest-compilation-mode-hook
    (lambda ()
      (local-set-key (kbd "g g") 'evil-goto-first-line))))

(provide 'lang-ruby)
