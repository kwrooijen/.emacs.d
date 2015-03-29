(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/config/modes")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/tempo")
(add-to-list 'load-path "~/.emacs.d/plugins/magit-gh-pulls")

(require 'my-packages)
(require 'my-powerline)
(require 'my-colors)
(require 'my-functions)
(require 'my-keys)
(require 'my-hydra)
(require 'my-options)
(require 'my-hooks)
(require 'my-scripts)

;; My modes
(require 'my-erlang)
(require 'my-haskell)
(require 'my-elixir)
(require 'my-rust)
(require 'my-eshell)
(require 'my-helm)

;; (setq my-extra-music t)
;; (setq my-extra-mail t)
;; (setq my-extra-chat t)
;; (setq my-extra-web t)
;; (setq my-extra-doc t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "0f6e58d1814b4138c5a88241f96547d35883cbb3df6cf9ec8ef44856ece04c13" default)))
 '(mode-line-format
   (quote
    (" "
     (:eval
      (concat "["
              (number-to-string
               (escreen-get-current-screen-number))
              "]"))
     " "
     (:eval
      (if
          (not
           (equal
            (gnus-mst-notify-modeline-form)
            ""))
          "[M] "))
     (:eval erc-modified-channels-object)
     "%*" "_" mode-line-remote " "
     (:eval
      (modeline-region-counter))
     "%3lL:%2cC "
     (:eval
      (format-time-string "%-I:%M%p"))
     " | " mode-line-buffer-identification " | " mode-name " |"
     (vc-mode vc-mode)
     " " battery-mode-line-string))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
