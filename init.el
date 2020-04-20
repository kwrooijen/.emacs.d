;;
;; Init
;;
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

;;
;; Packages
;;

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))

(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (evil-define-key 'normal global-mode-map (kbd "<SPC>") 'hydra-leader))

(use-package clojure-mode
  :straight t)

(use-package diff-hl
  :straight t)

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package hydra
  :straight t
  :ensure t)

;;
;; Configuration
;;

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; Smooth Scrolling
(setq scroll-margin 1
      scroll-conservatively 10000
      scroll-step 1
      auto-window-vscroll nil)


(set-frame-font "-*-Fira Mono-*-*-*-*-10-*-*-*-*-*-*-*" nil t)

;;
;; Keybindings
;;


(defhydra hydra-leader ()
  "Leader"
  ("f" find-file "Find File"))

 (define-key evil-normal-state-map (kbd "SPC") 'hydra-leader/body)

