(add-to-list 'load-path (expand-file-name "files" "~/.emacs.d"))
(require 'bootstrap)

(require 'custom-functions)

(require 'collection-evil)
(require 'collection-helm)
(require 'collection-productivity)
(require 'collection-theme)
(require 'collection-lisp)
(require 'collection-editing)

(require 'lang-clojure)

(require 'custom-options)
(require 'custom-keys)

;; Reset gc-cons-threshold back
(setq gc-cons-threshold 800000)
