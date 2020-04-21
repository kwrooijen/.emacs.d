;;
;; Init
;;
;; (defvar bootstrap-version)

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
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-leader
  :straight t
  :ensure t
  :config
  (global-evil-leader-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :straight t
  :config
  (evil-collection-init))

(use-package vi-tilde-fringe
  :ensure t
  :straight t
  :config
  (global-vi-tilde-fringe-mode t))

(use-package which-key
  :straight t
  ;; :init
  ;; (setq which-key-show-early-on-C-h t)
  ;; (setq which-key-idle-delay 10000)
  ;; (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(use-package clojure-mode
  :straight t)

(use-package diff-hl
  :straight t
  :init
  (setq diff-hl-margin-symbols-alist
   '((insert . "▐") (delete . "▐") (change . "▐") (unknown . "▐") (ignored . "▐")))

  :custom-face
  (diff-hl-margin-insert ((t (:foreground "#5be56b" :inherit nil))) )
  (diff-hl-margin-delete ((t (:foreground "#e85555" :inherit nil))) )
  (diff-hl-margin-change ((t (:foreground "#fcb75d" :inherit nil))) )
  :config
  (global-diff-hl-mode 1)
  (diff-hl-margin-mode 1))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package helm
  :straight t
  :ensure t
  :config
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action))

(use-package helm-swoop
  :straight t
  :ensure t)

(use-package helm-ag
  :straight t
  :ensure t)

(use-package helm-projectile
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

(evil-leader/set-key "e" 'find-file)

(evil-leader/set-key

 ;; Buffers
 "b b" 'helm-buffers-list

 ;; Files
 "f f" 'helm-find-files

 ;; Git
 "g g" 'magit-status

 ;; Project
 "p p" 'helm-projectile-switch-project
 "p f" 'helm-projectile-find-file

 ;; Search
 "s s" 'helm-swoop
 "s p" 'helm-projectile-ag

 ;; Window
 "w s" 'evil-window-split)

(evil-leader/set-leader "SPC")


;; TODO check these
;; (anzu-mode
;; +org-pretty-mode
;; git-gutter-mode
;; global-whitespace-newline-mode
;; global-whitespace-mode
;; whitespace-newline-mode
;; whitespace-mode
;; highlight-parentheses-mode
;; highlight-quoted-mode
;; rainbow-delimiters-mode
;; vi-tilde-fringe-mode
;; goto-address-prog-mode
;; goto-address-mode
;; highlight-numbers-mode
;; smartparens-mode
;; ws-butler-mode
;; projectile-mode
;; gcmh-mode
;; better-jumper-local-mode
;; company-search-mode
;; company-mode
;; helm-mode
;; helm-ff--delete-async-modeline-mode
;; evil-goggles-mode
;; evil-escape-mode
;; org-capture-mode
;; pcre-mode
;; hl-todo-mode
;; org-cdlatex-mode
;; org-src-mode
;; org-list-checkbox-radio-mode
;; orgtbl-mode
;; org-table-follow-field-mode
;; org-table-header-line-mode
;; magit-blame-mode
;; magit-file-mode
;; magit-wip-initial-backup-mode
;; magit-wip-before-change-mode
;; magit-wip-after-apply-mode
;; magit-wip-after-save-local-mode
;; magit-wip-mode
;; smerge-mode
;; diff-minor-mode
;; git-commit-mode
;; mml-mode
;; with-editor-mode
;; which-key-mode
;; buffer-face-mode
;; text-scale-mode
;; global-auto-revert-mode
;; auto-revert-tail-mode
;; auto-revert-mode
;; solaire-mode
;; evil-snipe-local-mode
;; flycheck-mode
;; clj-refactor-mode
;; superword-mode
;; subword-mode
;; sgml-electric-tag-pair-mode
;; paredit-mode
;; yas-minor-mode
;; cider--debug-mode
;; cider-mode
;; cider-auto-test-mode
;; cider-popup-buffer-mode
;; cider-enlighten-mode
;; vc-parent-buffer
;; lispyville-mode
;; lispy-other-mode
;; lispy-goto-mode
;; lispy-mode
;; outline-minor-mode
;; helm-migemo-mode
;; counsel-mode
;; compilation-minor-mode
;; compilation-shell-minor-mode
;; compilation-in-progress
;; ivy-mode
;; xref-etags-mode
;; edebug-mode
;; override-global-mode
;; mc-hide-unmatched-lines-mode
;; rectangular-region-mode
;; multiple-cursors-mode
;; +web-wordpress-mode
;; +web-jekyll-mode
;; +emacs-lisp-ert-mode
;; +data-vagrant-mode
;; undo-tree-visualizer-selection-mode
;; undo-tree-mode
;; reveal-mode
;; flyspell-mode
;; ispell-minor-mode
;; ns-auto-titlebar-mode
;; general-override-local-mode
;; general-override-mode
;; eldoc-mode
;; visible-mode
;; visual-line-mode
;; next-error-follow-minor-mode
;; abbrev-mode
;; overwrite-mode
;; auto-fill-function
;; defining-kbd-macro
;; isearch-mode)
