;;; collection/collection-helm.el -*- lexical-binding: t; -*-

(use-package helm
  :straight t
  :config
  ;; (evil-collection-init 'helm)
  (define-key global-map (kbd "M-x") #'helm-M-x)
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line)
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key helm-map (kbd "<escape>") #'helm-keyboard-quit))

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
    (lambda
      (interactive)
      (helm-previous-line)
      (helm-execute-persistent-action))))

(use-package helm-swoop
  :straight t)

(use-package helm-ag
  :straight t)

(use-package helm-projectile
  :after projectile
  :straight t)

(provide 'collection-helm)
