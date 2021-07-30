;;; files/bootstrap.el -*- lexical-binding: t; -*-

;; Install straight.el package manager
(setq gc-cons-threshold most-positive-fixnum)
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(add-to-list 'load-path (expand-file-name "files/collection" "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "files/lang" "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "files/custom" "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "files/deps" "~/.emacs.d"))
(add-to-list 'exec-path "/home/linuxbrew/.linuxbrew/bin")
(add-to-list 'exec-path "usr/local/bin")

(use-package path-helper
  :straight t
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (path-helper-setenv-all))

(use-package system-packages
  :straight t
  :init
  :config
  (setq system-packages-use-sudo nil)
  (setq system-packages-package-manager 'brew))

;; Setup mode leader key. This will be used by any lang/ directory
(use-package general :straight t :config (general-override-mode))

(defconst mode-leader "'")

(general-create-definer mode-leader-def
  :keymaps '(normal visual)
  :prefix "'")

;; For some reason Evil Collection breaks at initial boot unless this is set here
(setq evil-want-keybinding nil)

(provide 'bootstrap)
