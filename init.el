(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/tempo")
(add-to-list 'load-path "~/.emacs.d/plugins/distel/elisp")

(require 'my-packages)
(require 'my-powerline)
(require 'my-colors)
(require 'my-functions)
(require 'my-keys)
(require 'my-options)
(require 'my-hooks)
(require 'my-scripts)

(require 'tempo-erlang)

;; (setq my-extra-music t)
;; (setq my-extra-mail t)
;; (setq my-extra-chat t)

(require 'my-extras)
