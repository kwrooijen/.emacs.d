;;==============================================================================
;;== Keys
;;==============================================================================

;; (define-key emacs-lisp-mode-map (kbd "M-p") 'highlight-symbol-prev)
;; (define-key emacs-lisp-mode-map (kbd "M-n") 'highlight-symbol-next)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'litable-accept-as-pure)

;;==============================================================================
;;== Options
;;==============================================================================

;;==============================================================================
;;== Hooks
;;==============================================================================

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local emacs-lisp-mode t)
            (paredit-mode 1)
            (electric-pair-mode 0)
            (default-language-settings)
            ;; (litable-mode)
            (god-local-mode t)
            (setq-local helm-dash-docsets '("Emacs Lisp"))))

(provide 'attic-emacs-lisp)
