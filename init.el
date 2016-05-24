(setq gc-cons-threshold 100000000)

;; My vars
;; Macro is not active at boot, setting variable
(defvar macro-active nil)

;; (defvar key-setup 'hybrid)
(defvar key-setup 'off)
(defvar mode-lock 'evil)

(let ((file-name-handler-alist nil))
  (unless (file-exists-p "~/.emacs.d/multiple-cursors.el/multiple-cursors.el")
    (shell-command "cd ~/.emacs.d && git submodule init && git submodule update"))
  (add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
  (add-to-list 'load-path "~/.emacs.d/config")
  (add-to-list 'load-path "~/.emacs.d/plugins")
  (add-to-list 'load-path "~/.emacs.d/tempo")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'multiple-cursors)
  (require 'attic-options)
  (require 'attic-functions)
  (require 'attic-packages)
  (require 'attic-advice)
  (require 'attic-keys)
  (require 'attic-hooks)
  (require 'attic-colors)
  (when (file-exists-p "~/.system.el")
    (load-file "~/.system.el"))

  ;; temp hack
  (bind-key "<return>" 'newline mc/keymap)
  (bind-key "M-P" 'mc/mark-previous-like-this attic-mode-map)
  (bind-key "M-N" 'mc/mark-next-like-this attic-mode-map)
  (multiple-cursors-mode t))

(setq gc-cons-threshold 800000)
