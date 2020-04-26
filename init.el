(add-to-list 'load-path (expand-file-name "files" "~/.emacs.d"))
(require 'bootstrap)

(require 'custom-functions)

(require 'collection-evil)
(require 'collection-helm)
(require 'collection-productivity)
(require 'collection-theme)
(require 'collection-editing)
(require 'collection-lisp)

(require 'lang-clojure)
(require 'lang-elisp)

(require 'custom-options)
(require 'custom-keys)

;; Reset gc-cons-threshold back
(setq gc-cons-threshold 800000)
