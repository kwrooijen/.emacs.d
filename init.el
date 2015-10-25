(setq gc-cons-threshold 100000000)

;; My vars
;; TODO find a way to fix powerline without this hack
(defvar powerline-fixed nil)
;; Macro is not active at boot, setting variable
(defvar macro-active nil)
;; Total of unread messages from mu4e
(defvar mu4e-total-unread 0)
;; Is helm swoop active?
(defvar helm-swoop-active nil)
;; Is helm register active?
(defvar helm-register-active nil)

(let ((file-name-handler-alist nil))
  (add-to-list 'load-path "~/.emacs.d/config")
  (add-to-list 'load-path "~/.emacs.d/plugins")
  (add-to-list 'load-path "~/.emacs.d/tempo")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'attic-options)
  (require 'attic-functions)
  (require 'attic-packages)
  (require 'attic-advice)
  (require 'attic-keys)
  (require 'attic-hooks)
  (require 'attic-colors)
  (when (file-exists-p "~/.system.el")
    (load-file "~/.system.el")))

(setq gc-cons-threshold 800000)
