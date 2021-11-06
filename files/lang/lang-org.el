(use-package hide-mode-line
  :straight t)

(defvar bzg-big-fringe-mode nil)

(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-window-fringes (selected-window) 0 0)
    (set-window-fringes (selected-window) 1100 1100)))

(defun efs/presentation-setup ()
  (setq cider-use-fringe-indicators nil)
  (electric-indent-mode -1)
  (set-face-attribute 'org-block-begin-line nil :height 0.1 :foreground (face-attribute 'org-block :background))
  (set-face-attribute 'org-level-1 nil :height 2.0)
  (set-face-attribute 'org-level-2 nil :height 1.6)
  (set-face-attribute 'org-level-3 nil :height 1.3)
  (vi-tilde-fringe-mode -1)
  (flycheck-mode -1)
  (beacon-mode -1)
  (hl-line-mode -1)
  (global-hl-line-mode -1)
  (set-face-attribute 'default nil :height 180)
  (bzg-big-fringe-mode 1)
  (hide-mode-line-mode 1)

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

  ;; This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block))))

(defun efs/presentation-end ()
  ;; Show the mode line again
  (hide-mode-line-mode 0)
  (set-face-attribute 'org-block-begin-line nil :height 1.0 :foreground "#fff")

  ;; Turn off text scale mode (or use the next line if you didn't use text-scale-mode)
  ;; (text-scale-mode 0))

  ;; If you use face-remapping-alist, this clears the scaling:
  ;; (setq-local face-remapping-alist '((default variable-pitch default)))
  )

(use-package org-tree-slide
  :straight t
  :hook ((org-tree-slide-play . efs/presentation-setup)
         (org-tree-slide-stop . efs/presentation-end))
  :bind (("C-x C-e" . cider-eval-last-sexp))
  :init
  (setq org-tree-slide-content-margin-top 3)
  :custom
  ;; Hide begin / end block

  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "Presentation started!")
  (org-tree-slide-deactivate-message "Presentation finished!")
  (setq org-tree-slide-header t)
  ;; (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))

;; (use-package org-tree-slide
;;   :straight t)

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  ;; (org-indent-mode)
  ;; (setq evil-auto-indent nil)
  ;; (diminish org-indent-mode)
  (auto-fill-mode 0)
  (electric-indent-mode -1)
  (visual-line-mode 1))

(use-package org
  :straight t
  ;; :defer t
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq ;; org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks nil
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-eshell
          org-irc))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1)))

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (ledger . t))))

(use-package org-superstar
  :straight t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))

  ;; Replace list hyphen with dot
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Increase the size of various headings
  ;; (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    ;; (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face))
    )

  ;; Make sure org-indent face is available
  ;; (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(provide 'lang-org)
