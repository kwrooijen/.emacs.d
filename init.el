(setq comp-deferred-compilation t)
(add-to-list 'load-path (expand-file-name "files" "~/.emacs.d"))
(require 'bootstrap)

(require 'custom-functions)

(require 'collection-evil)
(require 'collection-helm)
(require 'collection-productivity)
(require 'collection-theme)
(require 'collection-editing)
(require 'collection-lisp)

(require 'lang-c)
(require 'lang-clojure)
(require 'lang-hy)
(require 'lang-elisp)
(require 'lang-rust)
(require 'lang-ruby)
(require 'lang-python)

(require 'custom-options)
(require 'custom-keys)
(require 'undo-tree)

(setq gc-cons-threshold  (* 1024 1024 10))
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lispy-clojure-middleware-tests nil)
 '(safe-local-variable-values (quote ((cider-shadow-cljs-default-options . "app")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-margin-change ((t (:foreground "#fcb75d" :inherit nil))))
 '(diff-hl-margin-delete ((t (:foreground "#e85555" :inherit nil))))
 '(diff-hl-margin-insert ((t (:foreground "#5be56b" :inherit nil))))
 '(flycheck-error ((t (:underline "#ff6c6b"))))
 '(flycheck-info ((t (:underline "#98be65"))))
 '(flycheck-warning ((t (:underline "#ECBE7B")))))

(defun my-first-helm-command ()
  (interactive)
  (helm :sources (helm-build-sync-source "test??"
                   :candidates '(("Hoi" . 123) ))
        :buffer "*helm my command*"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
