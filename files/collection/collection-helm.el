;;; collection/collection-helm.el -*- lexical-binding: t; -*-

(use-package helm
  :straight t
  :init
  (setq
   ;; truncate long lines in helm completion
   helm-truncate-lines t
   ;; may be overridden if 'ggrep' is in path (see below)
   helm-grep-default-command "grep -a -d skip %e -n%cH -e %p %f"
   helm-grep-default-recurse-command "grep --exclude-dir=\"dist\" -a -d recurse %e -n%cH -e %p %f"
   ;; do not display invisible candidates
   helm-quick-update t
   ;; be idle for this many seconds, before updating in delayed sources.
   helm-idle-delay 0.01
   ;; be idle for this many seconds, before updating candidate buffer
   helm-input-idle-delay 0.01
   ;; open helm buffer in another window
   helm-split-window-default-side 'other
   ;; limit the number of displayed canidates
   helm-candidate-number-limit 200
   ;; don't use recentf stuff in helm-ff
   helm-ff-file-name-history-use-recentf nil
   ;; fuzzy matching
   helm-buffers-fuzzy-matching t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-echo-input-in-header-line t
   ;; Don't ask to create new file
   helm-ff-newfile-prompt-p nil
   helm-reuse-last-window-split-state t
   helm-ff-transformer-show-only-basename nil
   ;; Split window down
   helm-split-window-in-side-p t
   ;; Split when multiple windows open
   helm-swoop-split-with-multiple-windows t
   ;; Don't show colors in Tramp mode
   helm-ff-tramp-not-fancy t
   ;; Smarter completion for Helm
   helm-ff-smart-completion t
   ;; Helm-dash should use W3m for showing documentation
   helm-dash-browser-func 'eww
   ;; Don't add delay when choosing
   helm-exit-idle-delay 0
   ;; Don't display header
   helm-display-header-line nil
   ;; Set a min / max height of 30% of current buffer
   helm-autoresize-max-height 30
   helm-autoresize-min-height 30
   helm-bookmark-show-location t
   helm-always-two-windows t
   helm-imenu-execute-action-at-once-if-one nil)
  :config
  (defun kwrooijen/helm-exit-minibuffer ()
    (interactive)
    (helm-exit-minibuffer))
  (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "C-l") nil)
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map (kbd "<escape>") #'helm-keyboard-quit)
  (define-key helm-map (kbd "<return>") #'kwrooijen/helm-exit-minibuffer))

(use-package helm-ag
  :straight t
  ;; :after evil-collection
  :config
  (define-key helm-ag-map (kbd "C-j")
    (lambda ()
      (interactive)
      (helm-next-line)
      (helm-execute-persistent-action)))
  (define-key helm-ag-map (kbd "C-k")
    (lambda ()
      (interactive)
      (helm-previous-line)
      (helm-execute-persistent-action))))

(use-package helm-swoop
  :straight t)

(use-package helm-ag
  :straight t)

(use-package helm-projectile
  :after projectile
  :straight t
  :init
  (setq projectile-use-git-grep t))

(use-package helm-descbinds
  :straight t)

(provide 'collection-helm)
