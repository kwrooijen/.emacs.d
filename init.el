(setq gc-cons-threshold 100000000)

(let ((file-name-handler-alist nil))
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/tempo")
(require 'attic-functions)
(require 'attic-packages)
(require 'attic-advice)
(require 'attic-keys)
(require 'attic-hydra)
(require 'attic-paredit)
(require 'attic-options)
(require 'attic-hooks)
(require 'attic-scripts)
(require 'attic-colors)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "0f6e58d1814b4138c5a88241f96547d35883cbb3df6cf9ec8ef44856ece04c13" default)))
 '(mode-line-format attic-mode-line-format))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ))
